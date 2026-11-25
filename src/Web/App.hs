{-# LANGUAGE OverloadedStrings #-}

-- | Маршрути веб-застосунку (Scotty).
--
-- Універсальні маршрути @/t/:table/...@ працюють для будь-якої сутності з
-- реєстру 'allEntities' завдяки класам 'Entity', 'Validatable' та 'Labelled'.
module Web.App
  ( runApp
  , routes
  ) where

import Config
import Control.Monad (forM)
import Data.Int (Int64)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time
import DB.Repository
import DB.Reports
import Domain.Entity
import Domain.Form
import Domain.Model (ServiceStatus)
import Domain.Registry (allEntities)
import Network.HTTP.Types.Status (Status, status404, status409, status422)
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Read (readMaybe)
import Web.Scotty
import Web.Views

runApp :: Config -> Db -> IO ()
runApp cfg db = scotty (cfgAppPort cfg) (routes db)

page :: Html -> ActionM ()
page = html . renderHtml

pageWith :: Status -> Html -> ActionM ()
pageWith st h = status st >> page h

lazy :: TL.Text -> Text
lazy = TL.toStrict

queryMaybe :: Text -> ActionM (Maybe Text)
queryMaybe k = do
  v <- queryParamMaybe (TL.fromStrict k)
  pure $ case fmap (T.strip . lazy) v of
    Just t | not (T.null t) -> Just t
    _ -> Nothing

flashFromQuery :: ActionM (Maybe Flash)
flashFromQuery = do
  ok <- queryMaybe "ok"
  pure $ case ok of
    Just "created" -> Just (FlashOk "Запис додано")
    Just "updated" -> Just (FlashOk "Зміни збережено")
    Just "deleted" -> Just (FlashOk "Запис видалено")
    _ -> Nothing

notFound' :: Text -> ActionM a
notFound' msg = pageWith status404 (notFoundPage msg) >> finish

-- | Знайти сутність за назвою таблиці та виконати дію з її типом.
withTable :: Text -> (forall a. Record a => Proxy a -> ActionM ()) -> ActionM ()
withTable t k = case entityByTable allEntities t of
  Nothing -> notFound' ("Невідома таблиця: " <> t)
  Just e -> withEntity e k

-- | Підписи записів для кожного зовнішнього ключа сутності.
refLabelsFor :: Entity a => Db -> Proxy a -> IO RefLabels
refLabelsFor db p = do
  let targets = nub [t | FieldSpec {fsKind = KRef t} <- fieldSpecs p]
  pairs <- forM targets $ \t -> do
    ls <- labelsOf db t
    pure (t, Map.fromList ls)
  pure (Map.fromList pairs)

readId :: Text -> ActionM Int64
readId t = maybe (notFound' "Некоректний ідентифікатор") pure (readMaybe (T.unpack t))

formMap :: ActionM (Map.Map Text Text)
formMap = do
  ps <- formParams
  pure (Map.fromList ps)

localNow :: IO LocalTime
localNow = zonedTimeToLocalTime <$> getZonedTime

routes :: Db -> ScottyM ()
routes db = do
  ---------------------------------------------------------------- головна
  get "/" $ do
    flash <- flashFromQuery
    counts <- liftIO $ forM allEntities $ \e -> withEntity e $ \p -> do
      n <- countRows db (tableName p)
      pure (tableName p, titlePlural p, n)
    today <- liftIO (utctDay <$> getCurrentTime)
    expiring <- liftIO (expiringTerms db (addDays 90 today))
    usage <- liftIO (serviceUsage db Nothing Nothing)
    page (dashboardPage flash counts expiring usage)

  ---------------------------------------------------------------- сервіси
  get "/services" $ do
    q <- queryMaybe "q"
    kind <- (>>= readMaybe . T.unpack) <$> queryMaybe "kind"
    author <- (>>= readMaybe . T.unpack) <$> queryMaybe "author"
    st <- (>>= parseEnum) <$> queryMaybe "status"
    let f = ServiceFilter q kind (st :: Maybe ServiceStatus) author
    found <- liftIO (searchServices db f)
    kinds <- liftIO (labelsOf db "service_kinds")
    authors <- liftIO (labelsOf db "authors")
    let refs = Map.fromList [("service_kinds", Map.fromList kinds), ("authors", Map.fromList authors)]
    page (servicesPage Nothing f refs found)

  get "/services/:id" $ do
    sid <- pathParam "id" >>= readId
    flash <- flashFromQuery
    md <- liftIO (serviceDetails db sid)
    maybe (notFound' "Сервіс не знайдено") (page . servicePage flash) md

  ---------------------------------------------------------------- статистика
  get "/usage" $ do
    from <- (>>= parseDay) <$> queryMaybe "from"
    to <- (>>= parseDay) <$> queryMaybe "to"
    us <- liftIO (serviceUsage db from to)
    page (usagePage from to us)

  ---------------------------------------------------------------- універсальний CRUD
  get "/t/:table" $ do
    table <- pathParam "table"
    q <- queryMaybe "q"
    flash <- flashFromQuery
    withTable table $ \p -> do
      rows <- liftIO (listAll db p q)
      refs <- liftIO (refLabelsFor db p)
      page (listPage p flash q refs rows)

  get "/t/:table/new" $ do
    table <- pathParam "table"
    withTable table $ \p -> do
      now <- liftIO localNow
      -- початкові значення з адресного рядка (?service_id=3) і поточний час для дат
      prefill <- Map.fromList <$> queryParams
      let defaults = Map.fromList
            [ (fsName s, formatDateTimeInput now) | s <- fieldSpecs p, fsKind s == KDateTime ]
      refs <- liftIO (refLabelsFor db p)
      page (formPage p CreateMode (Map.union prefill defaults) [] refs)

  post "/t/:table" $ do
    table <- pathParam "table"
    withTable table $ \p -> do
      vals <- formMap
      refs <- liftIO (refLabelsFor db p)
      case buildRecord p 0 vals of
        Left errs -> pageWith status422 (formPage p CreateMode vals errs refs)
        Right r -> do
          res <- liftIO (insertRecord db r)
          case res of
            Left (DbError m) -> pageWith status409 (formPage p CreateMode vals [m] refs)
            Right newId
              | tableName p == "services" -> redirect (TL.pack ("/services/" <> show newId <> "?ok=created"))
              | otherwise -> redirect (TL.fromStrict ("/t/" <> tableName p <> "?ok=created"))

  get "/t/:table/:id/edit" $ do
    table <- pathParam "table"
    i <- pathParam "id" >>= readId
    withTable table $ \p -> do
      mr <- liftIO (findById db p i)
      case mr of
        Nothing -> notFound' "Запис не знайдено"
        Just r -> do
          refs <- liftIO (refLabelsFor db p)
          page (formPage p (EditMode i) (formValuesOf p r) [] refs)

  post "/t/:table/:id" $ do
    table <- pathParam "table"
    i <- pathParam "id" >>= readId
    withTable table $ \p -> do
      vals <- formMap
      refs <- liftIO (refLabelsFor db p)
      existing <- liftIO (findById db p i)
      case existing of
        Nothing -> notFound' "Запис не знайдено"
        Just _ -> case buildRecord p i vals of
          Left errs -> pageWith status422 (formPage p (EditMode i) vals errs refs)
          Right r -> do
            res <- liftIO (updateRecord db r)
            case res of
              Left (DbError m) -> pageWith status409 (formPage p (EditMode i) vals [m] refs)
              Right () -> redirect (TL.fromStrict ("/t/" <> tableName p <> "?ok=updated"))

  post "/t/:table/:id/delete" $ do
    table <- pathParam "table"
    i <- pathParam "id" >>= readId
    withTable table $ \p -> do
      res <- liftIO (deleteRecord db p i)
      case res of
        Right () -> redirect (TL.fromStrict ("/t/" <> tableName p <> "?ok=deleted"))
        Left (DbError m) -> do
          rows <- liftIO (listAll db p Nothing)
          refs <- liftIO (refLabelsFor db p)
          pageWith status409 (listPage p (Just (FlashErr m)) Nothing refs rows)

  notFound $ pageWith status404 (notFoundPage "Сторінку не знайдено")
