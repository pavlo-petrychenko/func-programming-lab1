{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

-- Екземпляри навмисно винесені з модуля Domain.Model (орфани), щоб типи даних
-- не залежали від класів відображення.
--
-- | Екземпляри класів 'Entity', 'Validatable' і 'Labelled' для кожної таблиці.
module Domain.Instances
  ( validVersion
  ) where

import Data.Char (isDigit, isSpace)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (formatTime, defaultTimeLocale)
import Domain.Entity
import Domain.Model

-- | Версія має вигляд @1.2@ або @1.2.3@.
validVersion :: Text -> Bool
validVersion v =
  let parts = T.splitOn "." v
  in length parts `elem` [2, 3] && all (\p -> not (T.null p) && T.all isDigit p) parts

-- | Рівно два слова або більше (прізвище та ім'я).
atLeastTwoWords :: Text -> Bool
atLeastTwoWords = (>= 2) . length . T.words

badShape :: Text -> Either Text a
badShape t = Left ("неправильна кількість полів для " <> t)

---------------------------------------------------------------- service_kinds

instance Entity ServiceKind where
  tableName _ = "service_kinds"
  titlePlural _ = "Види сервісів"
  titleSingular _ = "вид сервісу"
  fieldSpecs _ =
    [ field "name" "Назва" (KText 100)
    , optional (field "description" "Опис" (KText 500))
    ]
  entityId = kindId
  toValues k = [toValue (kindName k), toValue (kindDescription k)]
  fromValues i [n, d] = ServiceKind i <$> fromValue n <*> fromValue d
  fromValues _ _ = badShape "service_kinds"

instance Validatable ServiceKind
instance Labelled ServiceKind where label = kindName

--------------------------------------------------------------------- authors

instance Entity Author where
  tableName _ = "authors"
  titlePlural _ = "Автори"
  titleSingular _ = "автора"
  fieldSpecs _ =
    [ field "full_name" "ПІБ" (KText 150)
    , field "email" "Email" KEmail
    , optional (field "position" "Посада" (KText 100))
    , optional (field "department" "Підрозділ" (KText 150))
    ]
  entityId = authorId
  toValues a =
    [ toValue (authorFullName a), toValue (authorEmail a)
    , toValue (authorPosition a), toValue (authorDepartment a) ]
  fromValues i [n, e, p, d] =
    Author i <$> fromValue n <*> fromValue e <*> fromValue p <*> fromValue d
  fromValues _ _ = badShape "authors"

instance Validatable Author where
  validate a =
    [ "ПІБ має містити прізвище та ім'я" | not (atLeastTwoWords (authorFullName a)) ]

instance Labelled Author where label = authorFullName

-------------------------------------------------------------------- services

instance Entity Service where
  tableName _ = "services"
  titlePlural _ = "Сервіси"
  titleSingular _ = "сервіс"
  fieldSpecs _ =
    [ field "name" "Назва сервісу" (KText 150)
    , field "annotation" "Анотація" KTextArea
    , field "kind_id" "Вид" (KRef "service_kinds")
    , field "current_version" "Версія" (KText 30)
    , field "status" "Статус" (KEnum (enumOptions (Proxy :: Proxy ServiceStatus)))
    , optional (field "launched_on" "Дата запуску" KDate)
    ]
  entityId = serviceId
  toValues s =
    [ toValue (serviceName s), toValue (serviceAnnotation s)
    , toValue (serviceKindId s), toValue (serviceVersion s)
    , enumValue (serviceStatus s), toValue (serviceLaunchedOn s) ]
  fromValues i [n, a, k, v, st, l] =
    Service i <$> fromValue n <*> fromValue a <*> fromValue k <*> fromValue v
              <*> fromEnumValue st <*> fromValue l
  fromValues _ _ = badShape "services"

instance Validatable Service where
  validate s = concat
    [ [ "Версія має вигляд 1.2 або 1.2.3" | not (validVersion (serviceVersion s)) ]
    , [ "Для діючого або застарілого сервісу потрібна дата запуску"
      | serviceStatus s /= Planned, serviceLaunchedOn s == Nothing ]
    ]

instance Labelled Service where label = serviceName

------------------------------------------------------------ service_authors

instance Entity ServiceAuthor where
  tableName _ = "service_authors"
  titlePlural _ = "Авторство сервісів"
  titleSingular _ = "авторство"
  fieldSpecs _ =
    [ field "service_id" "Сервіс" (KRef "services")
    , field "author_id" "Автор" (KRef "authors")
    , field "role" "Роль" (KEnum (enumOptions (Proxy :: Proxy AuthorRole)))
    ]
  entityId = saId
  toValues x = [toValue (saServiceId x), toValue (saAuthorId x), enumValue (saRole x)]
  fromValues i [s, a, r] =
    ServiceAuthor i <$> fromValue s <*> fromValue a <*> fromEnumValue r
  fromValues _ _ = badShape "service_authors"

instance Validatable ServiceAuthor
instance Labelled ServiceAuthor where
  label x = "сервіс #" <> tshow (saServiceId x) <> " / автор #" <> tshow (saAuthorId x)

------------------------------------------------------------ service_versions

instance Entity ServiceVersion where
  tableName _ = "service_versions"
  titlePlural _ = "Версії сервісів"
  titleSingular _ = "версію"
  fieldSpecs _ =
    [ field "service_id" "Сервіс" (KRef "services")
    , field "version" "Версія" (KText 30)
    , field "released_on" "Дата випуску" KDate
    , optional (field "changelog" "Зміни" KTextArea)
    ]
  entityId = svId
  toValues v =
    [ toValue (svServiceId v), toValue (svVersion v)
    , toValue (svReleasedOn v), toValue (svChangelog v) ]
  fromValues i [s, v, d, c] =
    ServiceVersion i <$> fromValue s <*> fromValue v <*> fromValue d <*> fromValue c
  fromValues _ _ = badShape "service_versions"

instance Validatable ServiceVersion where
  validate v = [ "Версія має вигляд 1.2 або 1.2.3" | not (validVersion (svVersion v)) ]

instance Labelled ServiceVersion where
  label v = "версія " <> svVersion v <> " (сервіс #" <> tshow (svServiceId v) <> ")"

------------------------------------------------------------------ usage_terms

instance Entity UsageTerms where
  tableName _ = "usage_terms"
  titlePlural _ = "Умови використання"
  titleSingular _ = "умови використання"
  fieldSpecs _ =
    [ field "service_id" "Сервіс" (KRef "services")
    , field "conditions" "Умови" KTextArea
    , field "rules" "Правила використання" KTextArea
    , optional (field "valid_until" "Кінцевий термін" KDate)
    ]
  entityId = utId
  toValues t =
    [ toValue (utServiceId t), toValue (utConditions t)
    , toValue (utRules t), toValue (utValidUntil t) ]
  fromValues i [s, c, r, d] =
    UsageTerms i <$> fromValue s <*> fromValue c <*> fromValue r <*> fromValue d
  fromValues _ _ = badShape "usage_terms"

instance Validatable UsageTerms where
  validate t =
    [ "Правила мають містити хоча б 10 символів" | T.length (T.strip (utRules t)) < 10 ]

instance Labelled UsageTerms where
  label t = "умови сервісу #" <> tshow (utServiceId t)

------------------------------------------------------------------------ users

instance Entity User where
  tableName _ = "users"
  titlePlural _ = "Користувачі"
  titleSingular _ = "користувача"
  fieldSpecs _ =
    [ field "full_name" "ПІБ" (KText 150)
    , field "email" "Email" KEmail
    , field "role" "Роль" (KEnum (enumOptions (Proxy :: Proxy UserRole)))
    , optional (field "group_name" "Група" (KText 50))
    , field "registered_at" "Дата реєстрації" KDateTime
    ]
  entityId = userId
  toValues u =
    [ toValue (userFullName u), toValue (userEmail u), enumValue (userRole u)
    , toValue (userGroup u), toValue (userRegisteredAt u) ]
  fromValues i [n, e, r, g, t] =
    User i <$> fromValue n <*> fromValue e <*> fromEnumValue r <*> fromValue g <*> fromValue t
  fromValues _ _ = badShape "users"

instance Validatable User where
  validate u = concat
    [ [ "ПІБ має містити прізвище та ім'я" | not (atLeastTwoWords (userFullName u)) ]
    , [ "Для студента потрібно вказати групу"
      | userRole u == Student, maybe True (T.all isSpace) (userGroup u) ]
    ]

instance Labelled User where label = userFullName

------------------------------------------------------------------ registrations

instance Entity Registration where
  tableName _ = "registrations"
  titlePlural _ = "Реєстрації"
  titleSingular _ = "реєстрацію"
  fieldSpecs _ =
    [ field "user_id" "Користувач" (KRef "users")
    , field "service_id" "Сервіс" (KRef "services")
    , field "registered_at" "Дата реєстрації" KDateTime
    , field "status" "Статус" (KEnum (enumOptions (Proxy :: Proxy RegStatus)))
    ]
  entityId = regId
  toValues r =
    [ toValue (regUserId r), toValue (regServiceId r)
    , toValue (regRegisteredAt r), enumValue (regStatus r) ]
  fromValues i [u, s, t, st] =
    Registration i <$> fromValue u <*> fromValue s <*> fromValue t <*> fromEnumValue st
  fromValues _ _ = badShape "registrations"

instance Validatable Registration
instance Labelled Registration where
  label r = "користувач #" <> tshow (regUserId r) <> " у сервісі #" <> tshow (regServiceId r)

----------------------------------------------------------------- usage_stats

instance Entity UsageStat where
  tableName _ = "usage_stats"
  titlePlural _ = "Статистика використання"
  titleSingular _ = "запис статистики"
  fieldSpecs _ =
    [ field "service_id" "Сервіс" (KRef "services")
    , field "user_id" "Користувач" (KRef "users")
    , field "used_on" "Дата" KDate
    , field "sessions" "Сеанси" KInt
    , field "minutes" "Хвилини" KInt
    ]
  entityId = usId
  toValues s =
    [ toValue (usServiceId s), toValue (usUserId s), toValue (usUsedOn s)
    , toValue (usSessions s), toValue (usMinutes s) ]
  fromValues i [s, u, d, n, m] =
    UsageStat i <$> fromValue s <*> fromValue u <*> fromValue d <*> fromValue n <*> fromValue m
  fromValues _ _ = badShape "usage_stats"

instance Validatable UsageStat where
  validate s = concat
    [ [ "Кількість сеансів не може бути від'ємною" | usSessions s < 0 ]
    , [ "Тривалість не може бути від'ємною" | usMinutes s < 0 ]
    , [ "За добу не може бути більше 1440 хвилин" | usMinutes s > 1440 ]
    , [ "Без сеансів тривалість має дорівнювати нулю" | usSessions s == 0, usMinutes s > 0 ]
    ]

instance Labelled UsageStat where
  label s = formatDay (usUsedOn s) <> ": " <> tshow (usSessions s) <> " сеанс(ів)"
    where formatDay = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

------------------------------------------------------------------- helpers

tshow :: Show a => a -> Text
tshow = T.pack . show
