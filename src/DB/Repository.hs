{-# LANGUAGE OverloadedStrings #-}

-- | Універсальний доступ до таблиць: працює для будь-якого типу з класом 'Entity'.
module DB.Repository
  ( Db
  , DbError (..)
  , newDb
  , runSql
  , querySql
  , listAll
  , listBy
  , findById
  , insertRecord
  , updateRecord
  , deleteRecord
  , countRows
  , labelsOf
  , textColumns
  , selectColumns
  , insertSql
  , updateSql
  , escapeLike
  ) where

import Config
import Control.Exception (Exception, SomeException, bracket, try)
import qualified Control.Exception as E
import Control.Monad (forM)
import Data.Int (Int64)
import Data.Pool (Pool, defaultPoolConfig, newPool, withResource)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.MySQL.Base
import DB.Convert
import qualified System.IO.Streams as Streams
import Domain.Entity
import Domain.Registry (allEntities)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL

type Db = Pool MySQLConn

-- | Помилка рівня БД, придатна для показу користувачеві.
newtype DbError = DbError Text
  deriving (Eq, Show)

instance Exception DbError

-- | Створити пул з'єднань.
newDb :: Config -> IO Db
newDb cfg = newPool (defaultPoolConfig open close 30 8)
  where
    open = connect defaultConnectInfoMB4
      { ciHost = cfgDbHost cfg
      , ciPort = fromIntegral (cfgDbPort cfg)
      , ciUser = BC.pack (cfgDbUser cfg)
      , ciPassword = BC.pack (cfgDbPassword cfg)
      , ciDatabase = BC.pack (cfgDbName cfg)
      }

mkQuery :: Text -> Query
mkQuery = Query . BL.fromStrict . TE.encodeUtf8

-- | Перекласти помилки MySQL у зрозумілі повідомлення.
translate :: ERRException -> DbError
translate (ERRException err) = DbError $ case errCode err of
  1062 -> "Запис з такими унікальними значеннями вже існує"
  1451 -> "Неможливо видалити: на цей запис посилаються інші таблиці"
  1452 -> "Вказано неіснуючий пов'язаний запис"
  3819 -> "Порушено обмеження коректності даних"
  c -> "Помилка бази даних " <> T.pack (show c) <> ": " <> TE.decodeUtf8 (errMsg err)

guarded :: IO a -> IO (Either DbError a)
guarded act = do
  r <- try act
  case r of
    Right a -> pure (Right a)
    Left e -> case E.fromException (e :: SomeException) of
      Just err -> pure (Left (translate err))
      Nothing -> E.throwIO e

-- | Виконати запит, що змінює дані. Повертає @(кількість змінених рядків, останній id)@.
runSql :: Db -> Text -> [FieldValue] -> IO (Either DbError (Int, Int64))
runSql db sql params = guarded $ withResource db $ \conn ->
  bracket (prepareStmt conn (mkQuery sql)) (closeStmt conn) $ \stmt -> do
    ok <- executeStmt conn stmt (map toMySQL params)
    pure (okAffectedRows ok, fromIntegral (okLastInsertID ok))

-- | Виконати SELECT і повернути «сирі» значення рядків.
querySql :: Db -> Text -> [FieldValue] -> IO [[MySQLValue]]
querySql db sql params = withResource db $ \conn ->
  bracket (prepareStmt conn (mkQuery sql)) (closeStmt conn) $ \stmt -> do
    (_, rows) <- queryStmt conn stmt (map toMySQL params)
    Streams.toList rows

---------------------------------------------------------------- генерація SQL

selectColumns :: Entity a => Proxy a -> Text
selectColumns p = T.intercalate ", " ("id" : map fsName (fieldSpecs p))

-- | Текстові колонки, за якими працює пошук.
textColumns :: Entity a => Proxy a -> [Text]
textColumns p = [fsName s | s <- fieldSpecs p, isText (fsKind s)]
  where
    isText k = case k of
      KText _ -> True
      KTextArea -> True
      KEmail -> True
      _ -> False

insertSql :: Entity a => Proxy a -> Text
insertSql p =
  "INSERT INTO " <> tableName p <> " (" <> T.intercalate ", " cols <> ") VALUES ("
    <> T.intercalate ", " (map (const "?") cols) <> ")"
  where cols = map fsName (fieldSpecs p)

updateSql :: Entity a => Proxy a -> Text
updateSql p =
  "UPDATE " <> tableName p <> " SET "
    <> T.intercalate ", " [fsName s <> " = ?" | s <- fieldSpecs p] <> " WHERE id = ?"

-- | Екранувати спеціальні символи шаблону @LIKE@.
escapeLike :: Text -> Text
escapeLike = T.concatMap esc
  where esc c = if c `elem` ['%', '_', '\\'] then T.pack ['\\', c] else T.singleton c

------------------------------------------------------------------- читання

decodeRow :: Entity a => Proxy a -> [MySQLValue] -> Either Text a
decodeRow p (idv : rest) = do
  i <- fromMySQL KInt idv >>= \v -> case v of
    VInt n -> Right n
    _ -> Left "некоректний id"
  vals <- sequence (zipWith (fromMySQL . fsKind) (fieldSpecs p) rest)
  fromValues i vals
decodeRow _ [] = Left "порожній рядок"

decodeAll :: Entity a => Proxy a -> [[MySQLValue]] -> IO [a]
decodeAll p rows = forM rows $ \r -> case decodeRow p r of
  Right a -> pure a
  Left e -> E.throwIO (DbError ("Не вдалося прочитати запис таблиці " <> tableName p <> ": " <> e))

-- | Усі записи таблиці; необов'язковий пошуковий рядок шукається в усіх текстових колонках.
listAll :: Entity a => Db -> Proxy a -> Maybe Text -> IO [a]
listAll db p mq = do
  let base = "SELECT " <> selectColumns p <> " FROM " <> tableName p
      cols = textColumns p
      (cond, params) = case mq of
        Just q | not (T.null (T.strip q)), not (null cols) ->
          ( " WHERE " <> T.intercalate " OR " [c <> " LIKE ?" | c <- cols]
          , [VText ("%" <> escapeLike (T.strip q) <> "%") | _ <- cols] )
        _ -> ("", [])
  rows <- querySql db (base <> cond <> " ORDER BY id") params
  decodeAll p rows

-- | Записи, у яких зовнішній ключ @col@ дорівнює @val@.
listBy :: Entity a => Db -> Proxy a -> Text -> Int64 -> IO [a]
listBy db p col val = do
  rows <- querySql db
    ("SELECT " <> selectColumns p <> " FROM " <> tableName p <> " WHERE " <> col <> " = ? ORDER BY id")
    [VInt val]
  decodeAll p rows

findById :: Entity a => Db -> Proxy a -> Int64 -> IO (Maybe a)
findById db p i = do
  rows <- querySql db
    ("SELECT " <> selectColumns p <> " FROM " <> tableName p <> " WHERE id = ?") [VInt i]
  xs <- decodeAll p rows
  pure (case xs of (x : _) -> Just x; [] -> Nothing)

countRows :: Db -> Text -> IO Int64
countRows db table = do
  rows <- querySql db ("SELECT CAST(COUNT(*) AS SIGNED) FROM " <> table) []
  case rows of
    [[v]] | Right (VInt n) <- fromMySQL KInt v -> pure n
    _ -> pure 0

------------------------------------------------------------------- запис

-- | Додати запис; повертає id нового запису.
insertRecord :: Entity a => Db -> a -> IO (Either DbError Int64)
insertRecord db x = fmap snd <$> runSql db (insertSql (proxyOf x)) (toValues x)

updateRecord :: Entity a => Db -> a -> IO (Either DbError ())
updateRecord db x = fmap (const ()) <$>
  runSql db (updateSql (proxyOf x)) (toValues x ++ [VInt (entityId x)])

deleteRecord :: Entity a => Db -> Proxy a -> Int64 -> IO (Either DbError ())
deleteRecord db p i = fmap (const ()) <$>
  runSql db ("DELETE FROM " <> tableName p <> " WHERE id = ?") [VInt i]

proxyOf :: a -> Proxy a
proxyOf _ = Proxy

-- | Пари @(id, підпис)@ усіх записів таблиці - для випадних списків і посилань.
labelsOf :: Db -> Text -> IO [(Int64, Text)]
labelsOf db table = case entityByTable allEntities table of
  Nothing -> pure []
  Just e -> withEntity e $ \p -> do
    xs <- listAll db p Nothing
    pure [(entityId x, label x) | x <- xs]
