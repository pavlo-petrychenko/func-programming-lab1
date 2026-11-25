{-# LANGUAGE OverloadedStrings #-}

-- | Запити, що поєднують кілька таблиць: пошук сервісів, статистика, терміни дії.
module DB.Reports
  ( ServiceFilter (..)
  , emptyFilter
  , searchServices
  , ServiceUsage (..)
  , serviceUsage
  , ExpiringTerm (..)
  , expiringTerms
  , ServiceDetails (..)
  , serviceDetails
  ) where

import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import DB.Convert
import DB.Repository
import Domain.Entity
import Domain.Model

-- | Умови пошуку сервісів. Порожні умови не обмежують результат.
data ServiceFilter = ServiceFilter
  { sfQuery  :: Maybe Text
  , sfKind   :: Maybe Int64
  , sfStatus :: Maybe ServiceStatus
  , sfAuthor :: Maybe Int64
  } deriving (Eq, Show)

emptyFilter :: ServiceFilter
emptyFilter = ServiceFilter Nothing Nothing Nothing Nothing

-- | Пошук за текстом (назва, анотація), видом, статусом і автором.
searchServices :: Db -> ServiceFilter -> IO [Service]
searchServices db f = do
  let p = Proxy :: Proxy Service
      conds = catMaybes
        [ (\q -> ( "(s.name LIKE ? OR s.annotation LIKE ?)"
                 , [VText (like q), VText (like q)] )) <$> nonEmpty (sfQuery f)
        , (\k -> ("s.kind_id = ?", [VInt k])) <$> sfKind f
        , (\st -> ("s.status = ?", [enumValue st])) <$> sfStatus f
        , (\a -> ( "EXISTS (SELECT 1 FROM service_authors sa WHERE sa.service_id = s.id AND sa.author_id = ?)"
                 , [VInt a] )) <$> sfAuthor f
        ]
      where_ = if null conds then "" else " WHERE " <> T.intercalate " AND " (map fst conds)
      cols = T.intercalate ", " (map ("s." <>) ("id" : map fsName (fieldSpecs p)))
      sql = "SELECT " <> cols <> " FROM services s" <> where_ <> " ORDER BY s.name"
  rows <- querySql db sql (concatMap snd conds)
  mapM decodeService rows
  where
    nonEmpty (Just q) | not (T.null (T.strip q)) = Just (T.strip q)
    nonEmpty _ = Nothing
    like q = "%" <> escapeLike q <> "%"
    decodeService r = do
      let p = Proxy :: Proxy Service
          specs = fieldSpecs p
      case r of
        (idv : rest) | Right (VInt i) <- fromMySQL KInt idv
                     , Right vals <- sequence (zipWith (fromMySQL . fsKind) specs rest)
                     , Right s <- fromValues i vals -> pure s
        _ -> fail "не вдалося прочитати сервіс"

-- | Сумарна статистика використання одного сервісу.
data ServiceUsage = ServiceUsage
  { suServiceId     :: Int64
  , suServiceName   :: Text
  , suSessions      :: Int64
  , suMinutes       :: Int64
  , suActiveUsers   :: Int64   -- ^ скільки різних користувачів користувалось
  , suRegistrations :: Int64   -- ^ активні реєстрації
  } deriving (Eq, Show)

-- | Статистика за період (межі включно; @Nothing@ - без обмеження), від найпопулярніших.
serviceUsage :: Db -> Maybe Day -> Maybe Day -> IO [ServiceUsage]
serviceUsage db from to = do
  let conds = catMaybes
        [ (\d -> ("u.used_on >= ?", VDate d)) <$> from
        , (\d -> ("u.used_on <= ?", VDate d)) <$> to ]
      onClause = T.concat (map (\(c, _) -> " AND " <> c) conds)
      sql = T.unlines
        [ "SELECT s.id, s.name,"
        , "  CAST(COALESCE(SUM(u.sessions), 0) AS SIGNED),"
        , "  CAST(COALESCE(SUM(u.minutes), 0) AS SIGNED),"
        , "  CAST(COUNT(DISTINCT u.user_id) AS SIGNED),"
        , "  (SELECT CAST(COUNT(*) AS SIGNED) FROM registrations r"
        , "     WHERE r.service_id = s.id AND r.status = 'active')"
        , "FROM services s LEFT JOIN usage_stats u ON u.service_id = s.id" <> onClause
        , "GROUP BY s.id, s.name"
        , "ORDER BY 3 DESC, 4 DESC, s.name" ]
  rows <- querySql db sql (map snd conds)
  pure [u | r <- rows, Just u <- [decodeUsage r]]
  where
    decodeUsage [a, b, c, d, e, f] = do
      i <- int a; n <- txt b; s <- int c; m <- int d; us <- int e; rg <- int f
      Just (ServiceUsage i n s m us rg)
    decodeUsage _ = Nothing
    int v = case fromMySQL KInt v of Right (VInt n) -> Just n; _ -> Nothing
    txt v = case fromMySQL (KText 0) v of Right (VText t) -> Just t; _ -> Nothing

-- | Умови використання, строк дії яких спливає до вказаної дати.
data ExpiringTerm = ExpiringTerm
  { etServiceId   :: Int64
  , etServiceName :: Text
  , etValidUntil  :: Day
  } deriving (Eq, Show)

expiringTerms :: Db -> Day -> IO [ExpiringTerm]
expiringTerms db before = do
  rows <- querySql db
    "SELECT s.id, s.name, t.valid_until FROM usage_terms t JOIN services s ON s.id = t.service_id \
    \WHERE t.valid_until IS NOT NULL AND t.valid_until <= ? ORDER BY t.valid_until"
    [VDate before]
  pure [e | [a, b, c] <- rows, Just e <- [decode a b c]]
  where
    decode a b c = case (fromMySQL KInt a, fromMySQL (KText 0) b, fromMySQL KDate c) of
      (Right (VInt i), Right (VText n), Right (VDate d)) -> Just (ExpiringTerm i n d)
      _ -> Nothing

-- | Усе, що показується на сторінці сервісу.
data ServiceDetails = ServiceDetails
  { sdService       :: Service
  , sdKind          :: Maybe ServiceKind
  , sdAuthors       :: [(ServiceAuthor, Maybe Author)]
  , sdVersions      :: [ServiceVersion]
  , sdTerms         :: Maybe UsageTerms
  , sdRegistrations :: [(Registration, Maybe User)]
  , sdUsage         :: Maybe ServiceUsage
  }

serviceDetails :: Db -> Int64 -> IO (Maybe ServiceDetails)
serviceDetails db sid = do
  ms <- findById db (Proxy :: Proxy Service) sid
  case ms of
    Nothing -> pure Nothing
    Just s -> do
      kind <- findById db (Proxy :: Proxy ServiceKind) (serviceKindId s)
      sas <- listBy db (Proxy :: Proxy ServiceAuthor) "service_id" sid
      authors <- mapM (\sa -> (,) sa <$> findById db (Proxy :: Proxy Author) (saAuthorId sa)) sas
      versions <- listBy db (Proxy :: Proxy ServiceVersion) "service_id" sid
      terms <- listBy db (Proxy :: Proxy UsageTerms) "service_id" sid
      regs <- listBy db (Proxy :: Proxy Registration) "service_id" sid
      regsU <- mapM (\r -> (,) r <$> findById db (Proxy :: Proxy User) (regUserId r)) regs
      usage <- serviceUsage db Nothing Nothing
      pure . Just $ ServiceDetails
        { sdService = s
        , sdKind = kind
        , sdAuthors = authors
        , sdVersions = versions
        , sdTerms = case terms of (t : _) -> Just t; [] -> Nothing
        , sdRegistrations = regsU
        , sdUsage = case filter ((== sid) . suServiceId) usage of (u : _) -> Just u; [] -> Nothing
        }
