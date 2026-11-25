{-# LANGUAGE OverloadedStrings #-}

-- | HTML-сторінки. Усі універсальні сторінки (список, форма) будуються з опису
-- полів класу 'Entity', тому нова таблиця не потребує нового коду для відображення.
module Web.Views
  ( RefLabels
  , Flash (..)
  , layout
  , dashboardPage
  , listPage
  , formPage
  , FormMode (..)
  , servicesPage
  , servicePage
  , usagePage
  , notFoundPage
  , errorPage
  , formValuesOf
  , renderRaw
  ) where

import Control.Monad (forM_, unless, when)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import DB.Reports
import Domain.Entity
import Domain.Form (formatDateTime, formatDateTimeInput, formatDay)
import Domain.Model
import Text.Blaze ((!?))
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- | Підписи записів за таблицями: @таблиця -> (id -> підпис)@.
type RefLabels = Map Text (Map Int64 Text)

data Flash = FlashOk Text | FlashErr Text

data FormMode = CreateMode | EditMode Int64
  deriving (Eq, Show)

---------------------------------------------------------------- каркас

layout :: Text -> Maybe Flash -> Html -> Html
layout title flash body = H.docTypeHtml ! A.lang "uk" $ do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.title (H.toHtml (title <> " - Сервіси факультету"))
    H.style (H.preEscapedToHtml css)
  H.body $ do
    H.header $ H.div ! A.class_ "bar" $ do
      H.a ! A.class_ "brand" ! A.href "/" $ "Сервіси мережі факультету"
      H.nav $ do
        H.a ! A.href "/services" $ "Пошук сервісів"
        H.a ! A.href "/usage" $ "Статистика"
        H.a ! A.href "/" $ "Таблиці"
    H.main $ do
      case flash of
        Just (FlashOk m) -> H.div ! A.class_ "flash ok" $ H.toHtml m
        Just (FlashErr m) -> H.div ! A.class_ "flash err" $ H.toHtml m
        Nothing -> pure ()
      H.h1 (H.toHtml title)
      body
    H.footer "Лабораторна робота 1, варіант 4 - Haskell + MySQL"

css :: Text
css = T.unlines
  [ ":root{--bg:#f5f6f8;--fg:#1f2430;--muted:#657084;--card:#fff;--line:#dfe3ea;--accent:#2457c5;--ok:#1b7f4a;--err:#b3261e}"
  , "*{box-sizing:border-box}"
  , "body{margin:0;font:15px/1.5 system-ui,-apple-system,Segoe UI,Roboto,sans-serif;background:var(--bg);color:var(--fg)}"
  , "header{background:#1f2a44;color:#fff}.bar{max-width:1100px;margin:0 auto;padding:12px 16px;display:flex;gap:24px;align-items:center;flex-wrap:wrap}"
  , ".brand{color:#fff;font-weight:700;text-decoration:none;font-size:17px}nav{display:flex;gap:16px;flex-wrap:wrap}nav a{color:#c9d4f2;text-decoration:none}nav a:hover{color:#fff}"
  , "main{max-width:1100px;margin:0 auto;padding:16px}footer{max-width:1100px;margin:24px auto;padding:0 16px;color:var(--muted);font-size:13px}"
  , "h1{font-size:24px;margin:8px 0 16px}h2{font-size:18px;margin:24px 0 8px}"
  , "a{color:var(--accent)}.card{background:var(--card);border:1px solid var(--line);border-radius:8px;padding:16px;margin-bottom:16px}"
  , ".grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(210px,1fr));gap:12px}"
  , ".tile{background:var(--card);border:1px solid var(--line);border-radius:8px;padding:14px;text-decoration:none;color:inherit;display:block}.tile:hover{border-color:var(--accent)}"
  , ".tile b{display:block;font-size:26px}.tile span{color:var(--muted)}"
  , "table{width:100%;border-collapse:collapse;background:var(--card);border:1px solid var(--line);border-radius:8px;overflow:hidden}"
  , "th,td{padding:8px 10px;border-bottom:1px solid var(--line);text-align:left;vertical-align:top}th{background:#eef1f6;font-size:13px;color:var(--muted)}"
  , ".tablewrap{overflow-x:auto;margin-bottom:10px}td.num,th.num{text-align:right}"
  , "form.inline{display:inline}.actions{white-space:nowrap}"
  , "button,.btn{font:inherit;border:1px solid var(--accent);background:var(--accent);color:#fff;border-radius:6px;padding:6px 12px;cursor:pointer;text-decoration:none;display:inline-block}"
  , ".btn.secondary,button.secondary{background:#fff;color:var(--accent)}button.danger{background:#fff;color:var(--err);border-color:var(--err)}"
  , "label{display:block;font-weight:600;margin:12px 0 4px}input,select,textarea{font:inherit;width:100%;padding:7px 9px;border:1px solid var(--line);border-radius:6px;background:#fff}"
  , "textarea{min-height:100px}.row{display:flex;gap:12px;flex-wrap:wrap;align-items:flex-end}.row>div{flex:1;min-width:160px}.row label{margin-top:0}"
  , ".flash{padding:10px 14px;border-radius:6px;margin-bottom:12px}.flash.ok{background:#e4f5ec;color:var(--ok)}.flash.err{background:#fdeceb;color:var(--err)}"
  , ".errors{background:#fdeceb;color:var(--err);border-radius:6px;padding:8px 14px;margin-bottom:12px}.errors ul{margin:0;padding-left:18px}"
  , ".muted{color:var(--muted)}.tag{display:inline-block;padding:1px 8px;border-radius:10px;background:#eef1f6;font-size:12px}"
  , ".tag.active{background:#e4f5ec;color:var(--ok)}.tag.deprecated{background:#fdeceb;color:var(--err)}.tag.planned{background:#fff3d6;color:#8a5a00}"
  , ".toolbar{display:flex;justify-content:space-between;gap:12px;margin-bottom:12px;flex-wrap:wrap}.toolbar form{display:flex;gap:8px}"
  , "dl{display:grid;grid-template-columns:max-content 1fr;gap:6px 16px;margin:0}dt{color:var(--muted)}dd{margin:0}"
  ]

---------------------------------------------------------------- головна

-- | Головна: лічильники таблиць, строки дії умов, найпопулярніші сервіси.
dashboardPage :: Maybe Flash -> [(Text, Text, Int64)] -> [ExpiringTerm] -> [ServiceUsage] -> Html
dashboardPage flash counts expiring top = layout "Інформаційні сервіси мережі факультету" flash $ do
  H.p ! A.class_ "muted" $
    "Облік сервісів факультету, їх авторів, версій, умов використання, користувачів і статистики."
  H.h2 "Таблиці бази даних"
  H.div ! A.class_ "grid" $
    forM_ counts $ \(tbl, title, n) ->
      H.a ! A.class_ "tile" ! A.href (H.toValue ("/t/" <> tbl)) $ do
        H.b (H.toHtml (show n))
        H.span (H.toHtml title)
  H.h2 "Умови, строк дії яких спливає протягом 90 днів або вже сплив"
  if null expiring
    then H.p ! A.class_ "muted" $ "Таких умов немає."
    else H.div ! A.class_ "tablewrap" $ H.table $ do
      H.tr $ mapM_ H.th ["Сервіс", "Діє до"]
      forM_ expiring $ \e -> H.tr $ do
        H.td $ H.a ! A.href (H.toValue ("/services/" <> show (etServiceId e))) $ H.toHtml (etServiceName e)
        H.td (H.toHtml (formatDay (etValidUntil e)))
  H.h2 "Найпопулярніші сервіси"
  usageTable (take 5 top)

---------------------------------------------------------------- список

-- | Універсальна таблиця записів будь-якої сутності.
listPage :: Record a => Proxy a -> Maybe Flash -> Maybe Text -> RefLabels -> [a] -> Html
listPage p flash q refs rows = layout (titlePlural p) flash $ do
  H.div ! A.class_ "toolbar" $ do
    H.form ! A.method "get" ! A.action (H.toValue ("/t/" <> tableName p)) $ do
      H.input ! A.type_ "search" ! A.name "q" ! A.placeholder "Пошук за текстом..."
        ! A.value (H.toValue (fromMaybe "" q))
      H.button ! A.type_ "submit" $ "Знайти"
      when (maybe False (not . T.null) q) $
        H.a ! A.class_ "btn secondary" ! A.href (H.toValue ("/t/" <> tableName p)) $ "Скинути"
    H.a ! A.class_ "btn" ! A.href (H.toValue ("/t/" <> tableName p <> "/new")) $
      H.toHtml ("Додати " <> titleSingular p)
  if null rows
    then H.p ! A.class_ "muted" $ "Записів не знайдено."
    else H.div ! A.class_ "tablewrap" $ H.table $ do
      H.tr $ do
        H.th "ID"
        forM_ specs $ \s -> H.th (H.toHtml (fsLabel s))
        H.th ""
      forM_ rows $ \r -> H.tr $ do
        H.td (H.toHtml (show (entityId r)))
        forM_ (zip specs (toValues r)) $ \(s, v) -> H.td (renderCell refs s v)
        H.td ! A.class_ "actions" $ do
          H.a ! A.href (H.toValue (base <> "/" <> show (entityId r) <> "/edit")) $ "Редагувати"
          " "
          H.form ! A.class_ "inline" ! A.method "post"
            ! A.action (H.toValue (base <> "/" <> show (entityId r) <> "/delete"))
            ! A.onsubmit "return confirm('Видалити запис?')" $
            H.button ! A.class_ "danger" ! A.type_ "submit" $ "Видалити"
  where
    specs = fieldSpecs p
    base = "/t/" <> T.unpack (tableName p)

renderCell :: RefLabels -> FieldSpec -> FieldValue -> Html
renderCell refs spec v = case (fsKind spec, v) of
  (_, VNull) -> H.span ! A.class_ "muted" $ "-"
  (KRef t, VInt i) -> case Map.lookup t refs >>= Map.lookup i of
    Just l -> H.toHtml l
    Nothing -> H.toHtml ("#" <> T.pack (show i))
  (KEnum opts, VText c) -> H.span ! A.class_ (H.toValue ("tag " <> c)) $
    H.toHtml (fromMaybe c (lookup c opts))
  (_, VText t) -> H.toHtml (clip 90 t)
  (_, VInt n) -> H.toHtml (show n)
  (_, VDate d) -> H.toHtml (formatDay d)
  (_, VDateTime t) -> H.toHtml (formatDateTime t)

clip :: Int -> Text -> Text
clip n t = if T.length t > n then T.take n t <> "..." else t

---------------------------------------------------------------- форма

-- | Значення запису у вигляді текстів для полів форми.
formValuesOf :: Entity a => Proxy a -> a -> Map Text Text
formValuesOf p r = Map.fromList
  [ (fsName s, renderRaw v) | (s, v) <- zip (fieldSpecs p) (toValues r) ]

-- | Значення поля як текст для атрибута @value@.
renderRaw :: FieldValue -> Text
renderRaw VNull = ""
renderRaw (VText t) = t
renderRaw (VInt n) = T.pack (show n)
renderRaw (VDate d) = formatDay d
renderRaw (VDateTime t) = formatDateTimeInput t

formPage :: Entity a => Proxy a -> FormMode -> Map Text Text -> [Text] -> RefLabels -> Html
formPage p mode vals errs refs = layout heading Nothing $ do
  unless (null errs) $
    H.div ! A.class_ "errors" $ do
      "Дані не збережено:"
      H.ul (mapM_ (H.li . H.toHtml) errs)
  H.form ! A.class_ "card" ! A.method "post" ! A.action (H.toValue action) $ do
    forM_ (fieldSpecs p) $ \s -> inputFor refs s (Map.findWithDefault "" (fsName s) vals)
    H.div ! A.style "margin-top:16px" $ do
      H.button ! A.type_ "submit" $ "Зберегти"
      " "
      H.a ! A.class_ "btn secondary" ! A.href (H.toValue ("/t/" <> tableName p)) $ "Скасувати"
  where
    heading = case mode of
      CreateMode -> "Новий запис: " <> titleSingular p
      EditMode i -> "Редагування: " <> titleSingular p <> " #" <> T.pack (show i)
    action = "/t/" <> T.unpack (tableName p) <> case mode of
      CreateMode -> ""
      EditMode i -> "/" <> show i

inputFor :: RefLabels -> FieldSpec -> Text -> Html
inputFor refs s val = do
  H.label ! A.for (H.toValue (fsName s)) $ do
    H.toHtml (fsLabel s)
    unless (fsRequired s) $ H.span ! A.class_ "muted" $ " (необов'язково)"
  case fsKind s of
    KText n -> base "text" ! A.maxlength (H.toValue n)
    KEmail -> base "email"
    KInt -> base "number" ! A.step "1"
    KDate -> base "date"
    KDateTime -> base "datetime-local"
    KTextArea -> H.textarea ! A.id name ! A.name name !? req $ H.toHtml val
    KEnum opts -> H.select ! A.id name ! A.name name !? req $ options opts
    KRef t -> H.select ! A.id name ! A.name name !? req $ do
      unless (fsRequired s) $ H.option ! A.value "" $ "-"
      when (fsRequired s && T.null val) $ H.option ! A.value "" $ "- оберіть -"
      forM_ (Map.toList (Map.findWithDefault Map.empty t refs)) $ \(i, l) ->
        H.option ! A.value (H.toValue i) !? (T.pack (show i) == val, A.selected "selected") $ H.toHtml l
  where
    name = H.toValue (fsName s)
    req = (fsRequired s, A.required "required")
    base ty = H.input ! A.type_ ty ! A.id name ! A.name name ! A.value (H.toValue val) !? req
    options opts = do
      unless (fsRequired s) $ H.option ! A.value "" $ "-"
      forM_ opts $ \(c, l) ->
        H.option ! A.value (H.toValue c) !? (c == val, A.selected "selected") $ H.toHtml l

---------------------------------------------------------------- сервіси

-- | Сторінка пошуку сервісів.
servicesPage :: Maybe Flash -> ServiceFilter -> RefLabels -> [Service] -> Html
servicesPage flash f refs services = layout "Пошук сервісів" flash $ do
  H.form ! A.class_ "card" ! A.method "get" ! A.action "/services" $ do
    H.div ! A.class_ "row" $ do
      H.div $ do
        H.label ! A.for "q" $ "Текст у назві або анотації"
        H.input ! A.type_ "search" ! A.id "q" ! A.name "q" ! A.value (H.toValue (fromMaybe "" (sfQuery f)))
      H.div $ do
        H.label ! A.for "kind" $ "Вид"
        select "kind" (labelsFor "service_kinds") (fmap (T.pack . show) (sfKind f))
      H.div $ do
        H.label ! A.for "status" $ "Статус"
        H.select ! A.id "status" ! A.name "status" $ do
          H.option ! A.value "" $ "Будь-який"
          forM_ (enumOptions (Proxy :: Proxy ServiceStatus)) $ \(c, l) ->
            H.option ! A.value (H.toValue c) !? (Just c == fmap enumCode (sfStatus f), A.selected "selected") $ H.toHtml l
      H.div $ do
        H.label ! A.for "author" $ "Автор"
        select "author" (labelsFor "authors") (fmap (T.pack . show) (sfAuthor f))
      H.div $ do
        H.button ! A.type_ "submit" $ "Шукати"
        " "
        H.a ! A.class_ "btn secondary" ! A.href "/services" $ "Скинути"
  H.div ! A.class_ "toolbar" $ do
    H.span ! A.class_ "muted" $ H.toHtml ("Знайдено: " <> show (length services))
    H.a ! A.class_ "btn" ! A.href "/t/services/new" $ "Додати сервіс"
  if null services
    then H.p ! A.class_ "muted" $ "За цими умовами сервісів не знайдено."
    else H.div ! A.class_ "tablewrap" $ H.table $ do
      H.tr $ mapM_ H.th ["Назва", "Вид", "Версія", "Статус", "Анотація"]
      forM_ services $ \s -> H.tr $ do
        H.td $ H.a ! A.href (H.toValue ("/services/" <> show (serviceId s))) $ H.toHtml (serviceName s)
        H.td (H.toHtml (Map.findWithDefault "-" (serviceKindId s) (labelsFor "service_kinds")))
        H.td (H.toHtml (serviceVersion s))
        H.td (statusTag (serviceStatus s))
        H.td (H.toHtml (clip 100 (serviceAnnotation s)))
  where
    labelsFor t = Map.findWithDefault Map.empty t refs
    select name opts cur = H.select ! A.id name ! A.name name $ do
      H.option ! A.value "" $ "Будь-який"
      forM_ (Map.toList opts) $ \(i, l) ->
        H.option ! A.value (H.toValue i) !? (Just (T.pack (show i)) == cur, A.selected "selected") $ H.toHtml l

statusTag :: ServiceStatus -> Html
statusTag st = H.span ! A.class_ (H.toValue ("tag " <> enumCode st)) $ H.toHtml (enumLabel st)

-- | Картка сервісу з усіма пов'язаними даними.
servicePage :: Maybe Flash -> ServiceDetails -> Html
servicePage flash d = layout (serviceName s) flash $ do
  H.div ! A.class_ "toolbar" $ do
    H.a ! A.href "/services" $ "< до списку сервісів"
    H.a ! A.class_ "btn" ! A.href (H.toValue ("/t/services/" <> show (serviceId s) <> "/edit")) $ "Редагувати"
  H.div ! A.class_ "card" $ do
    H.p (H.toHtml (serviceAnnotation s))
    H.dl $ do
      H.dt "Вид"; H.dd (H.toHtml (maybe "-" kindName (sdKind d)))
      H.dt "Версія"; H.dd (H.toHtml (serviceVersion s))
      H.dt "Статус"; H.dd (statusTag (serviceStatus s))
      H.dt "Запущено"; H.dd (H.toHtml (maybe "-" formatDay (serviceLaunchedOn s)))
      forM_ (sdUsage d) $ \u -> do
        H.dt "Сеансів / хвилин"; H.dd (H.toHtml (show (suSessions u) <> " / " <> show (suMinutes u)))
        H.dt "Користувачів у статистиці"; H.dd (H.toHtml (show (suActiveUsers u)))
        H.dt "Активних реєстрацій"; H.dd (H.toHtml (show (suRegistrations u)))
  H.h2 "Автори"
  section "service_authors" (null (sdAuthors d)) "Авторів ще не додано." $
    H.table $ do
      H.tr $ mapM_ H.th ["Автор", "Роль", "Підрозділ"]
      forM_ (sdAuthors d) $ \(sa, ma) -> H.tr $ do
        H.td (H.toHtml (maybe "-" authorFullName ma))
        H.td (H.toHtml (enumLabel (saRole sa)))
        H.td (H.toHtml (fromMaybe "-" (ma >>= authorDepartment)))
  H.h2 "Версії"
  section "service_versions" (null (sdVersions d)) "Історії версій немає." $
    H.table $ do
      H.tr $ mapM_ H.th ["Версія", "Дата", "Зміни"]
      forM_ (sdVersions d) $ \v -> H.tr $ do
        H.td (H.toHtml (svVersion v))
        H.td (H.toHtml (formatDay (svReleasedOn v)))
        H.td (H.toHtml (fromMaybe "-" (svChangelog v)))
  H.h2 "Умови та правила використання"
  case sdTerms d of
    Nothing -> do
      H.p ! A.class_ "muted" $ "Умови не задано."
      H.a ! A.class_ "btn secondary" ! A.href (H.toValue ("/t/usage_terms/new?service_id=" <> show (serviceId s))) $ "Додати умови"
    Just t -> H.div ! A.class_ "card" $ do
      H.dl $ do
        H.dt "Умови"; H.dd (H.toHtml (utConditions t))
        H.dt "Правила"; H.dd (H.toHtml (utRules t))
        H.dt "Кінцевий термін"; H.dd (H.toHtml (maybe "безстроково" formatDay (utValidUntil t)))
      H.a ! A.href (H.toValue ("/t/usage_terms/" <> show (utId t) <> "/edit")) $ "Редагувати умови"
  H.h2 "Зареєстровані користувачі"
  section "registrations" (null (sdRegistrations d)) "Реєстрацій немає." $
    H.table $ do
      H.tr $ mapM_ H.th ["Користувач", "Роль", "Дата", "Статус"]
      forM_ (sdRegistrations d) $ \(r, mu) -> H.tr $ do
        H.td (H.toHtml (maybe "-" userFullName mu))
        H.td (H.toHtml (maybe "-" (enumLabel . userRole) mu))
        H.td (H.toHtml (formatDateTime (regRegisteredAt r)))
        H.td (H.toHtml (enumLabel (regStatus r)))
  where
    s = sdService d
    section tbl isEmpty emptyMsg content = do
      if isEmpty
        then H.p ! A.class_ "muted" $ H.toHtml (emptyMsg :: Text)
        else H.div ! A.class_ "tablewrap" $ content
      H.a ! A.class_ "btn secondary"
        ! A.href (H.toValue ("/t/" <> (tbl :: Text) <> "/new?service_id=" <> T.pack (show (serviceId s)))) $ "Додати"

---------------------------------------------------------------- статистика

usageTable :: [ServiceUsage] -> Html
usageTable us = H.div ! A.class_ "tablewrap" $ H.table $ do
  H.tr $ do
    H.th "Сервіс"
    mapM_ (H.th ! A.class_ "num") ["Сеанси", "Хвилини", "Користувачі", "Активні реєстрації"]
  forM_ us $ \u -> H.tr $ do
    H.td $ H.a ! A.href (H.toValue ("/services/" <> show (suServiceId u))) $ H.toHtml (suServiceName u)
    H.td ! A.class_ "num" $ H.toHtml (show (suSessions u))
    H.td ! A.class_ "num" $ H.toHtml (show (suMinutes u))
    H.td ! A.class_ "num" $ H.toHtml (show (suActiveUsers u))
    H.td ! A.class_ "num" $ H.toHtml (show (suRegistrations u))

-- | Звіт зі статистики використання за період.
usagePage :: Maybe Day -> Maybe Day -> [ServiceUsage] -> Html
usagePage from to us = layout "Статистика використання" Nothing $ do
  H.form ! A.class_ "card" ! A.method "get" ! A.action "/usage" $
    H.div ! A.class_ "row" $ do
      H.div $ do
        H.label ! A.for "from" $ "З дати"
        H.input ! A.type_ "date" ! A.id "from" ! A.name "from" ! A.value (H.toValue (maybe "" formatDay from))
      H.div $ do
        H.label ! A.for "to" $ "По дату"
        H.input ! A.type_ "date" ! A.id "to" ! A.name "to" ! A.value (H.toValue (maybe "" formatDay to))
      H.div $ do
        H.button ! A.type_ "submit" $ "Показати"
        " "
        H.a ! A.class_ "btn secondary" ! A.href "/t/usage_stats/new" $ "Додати запис"
  usageTable us
  H.p ! A.class_ "muted" $ H.toHtml ("Разом сеансів: " <> show (sum (map suSessions us))
    <> ", хвилин: " <> show (sum (map suMinutes us)))

---------------------------------------------------------------- помилки

notFoundPage :: Text -> Html
notFoundPage msg = layout "Не знайдено" Nothing $ do
  H.p (H.toHtml msg)
  H.a ! A.href "/" $ "На головну"

errorPage :: Text -> Html
errorPage msg = layout "Помилка" Nothing $ do
  H.div ! A.class_ "flash err" $ H.toHtml msg
  H.a ! A.href "/" $ "На головну"
