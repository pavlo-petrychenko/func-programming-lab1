{-# LANGUAGE OverloadedStrings #-}

-- | Тести чистої частини: класи, екземпляри, розбір форм, правила перевірки.
-- База даних для них не потрібна; повний цикл з MySQL перевіряє scripts/smoke.sh.
module Main (main) where

import Config (parseDotEnv)
import Control.Monad (unless)
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import DB.Repository (escapeLike, insertSql, selectColumns, updateSql)
import Domain.Entity
import Domain.Form
import Domain.Instances (validVersion)
import Domain.Model
import Domain.Registry (allEntities, tableNames)
import System.Exit (exitFailure)

data Result = Result String Bool

check :: String -> Bool -> Result
check = Result

day :: Integer -> Int -> Int -> Day
day = fromGregorian

at :: Day -> LocalTime
at d = LocalTime d (TimeOfDay 10 30 0)

-- Зразковий запис кожної таблиці.
kind1 :: ServiceKind
kind1 = ServiceKind 1 "Освітня платформа" (Just "опис")

author1 :: Author
author1 = Author 2 "Коваленко Ірина Петрівна" "i@fac.example.edu" Nothing (Just "Кафедра")

service1 :: Service
service1 = Service 3 "Moodle" "Дистанційне навчання" 1 "4.1.5" Active (Just (day 2021 2 15))

sa1 :: ServiceAuthor
sa1 = ServiceAuthor 4 3 2 Lead

sv1 :: ServiceVersion
sv1 = ServiceVersion 5 3 "4.1.5" (day 2024 6 12) Nothing

terms1 :: UsageTerms
terms1 = UsageTerms 6 3 "Лише студенти" "Не передавати пароль іншим" (Just (day 2027 8 31))

user1 :: User
user1 = User 7 "Петренко Олена" "o@stud.example.edu" Student (Just "ПІ-31") (at (day 2024 9 2))

reg1 :: Registration
reg1 = Registration 8 7 3 (at (day 2024 9 2)) RegBlocked

stat1 :: UsageStat
stat1 = UsageStat 9 3 7 (day 2025 3 3) 4 35

-- | Запис -> значення -> запис: перевіряє, що toValues/fromValues узгоджені.
roundtrip :: (Entity a, Eq a) => a -> Bool
roundtrip x = fromValues (entityId x) (toValues x) == Right x

-- | Кількість значень збігається з кількістю описаних полів.
shapeOk :: Entity a => a -> Bool
shapeOk x = length (toValues x) == length (fieldSpecs (proxyOf x))
  where proxyOf :: a -> Proxy a
        proxyOf _ = Proxy

formFor :: [(Text, Text)] -> Map.Map Text Text
formFor = Map.fromList

validServiceForm :: [(Text, Text)]
validServiceForm =
  [ ("name", "Новий сервіс"), ("annotation", "Опис"), ("kind_id", "1")
  , ("current_version", "1.0"), ("status", "active"), ("launched_on", "2025-01-01") ]

tests :: [Result]
tests =
  [ -- реєстр сутностей
    check "таблиць більше п'яти" (length allEntities > 5)
  , check "таблиць рівно дев'ять" (length allEntities == 9)
  , check "назви таблиць унікальні" (length tableNames == length (Map.fromList [(t, ()) | t <- tableNames]))
  , check "пошук сутності за назвою таблиці" (isJust (entityByTable allEntities "services"))
  , check "невідома таблиця не знаходиться" (isNothing (entityByTable allEntities "nope"))
  , check "кожне посилання KRef веде на існуючу таблицю" $
      and [ t `elem` tableNames
          | e <- allEntities
          , s <- withEntity e fieldSpecs
          , KRef t <- [fsKind s] ]
    -- клас Entity: екземпляри
  , check "ServiceKind: кругова перевірка" (roundtrip kind1)
  , check "Author: кругова перевірка" (roundtrip author1)
  , check "Service: кругова перевірка" (roundtrip service1)
  , check "ServiceAuthor: кругова перевірка" (roundtrip sa1)
  , check "ServiceVersion: кругова перевірка" (roundtrip sv1)
  , check "UsageTerms: кругова перевірка" (roundtrip terms1)
  , check "User: кругова перевірка" (roundtrip user1)
  , check "Registration: кругова перевірка" (roundtrip reg1)
  , check "UsageStat: кругова перевірка" (roundtrip stat1)
  , check "кількість значень = кількість полів (усі таблиці)" $
      and [ shapeOk kind1, shapeOk author1, shapeOk service1, shapeOk sa1, shapeOk sv1
          , shapeOk terms1, shapeOk user1, shapeOk reg1, shapeOk stat1 ]
  , check "fromValues відхиляє неправильну кількість полів"
      (isLeft (fromValues 1 [VText "a"] :: Either Text Author))
  , check "fromValues відхиляє неправильний тип значення"
      (isLeft (fromValues 1 [VInt 1, VText "a"] :: Either Text ServiceKind))
    -- Enumerated
  , check "enumCode/parseEnum взаємно обернені (статус сервісу)"
      (all (\s -> parseEnum (enumCode s) == Just (s :: ServiceStatus)) [minBound .. maxBound])
  , check "enumOptions містить усі значення ролі користувача"
      (map fst (enumOptions (Proxy :: Proxy UserRole)) == ["student", "teacher", "staff"])
  , check "parseEnum невідомого коду" (isNothing (parseEnum "zzz" :: Maybe RegStatus))
  , check "enumLabel українською" (enumLabel Deprecated == "Застарілий")
    -- Validatable
  , check "коректний сервіс проходить перевірку" (null (validate service1))
  , check "версія 'abc' не проходить" (not (null (validate service1 { serviceVersion = "abc" })))
  , check "діючий сервіс без дати запуску не проходить"
      (not (null (validate service1 { serviceLaunchedOn = Nothing })))
  , check "плановий сервіс без дати запуску проходить"
      (null (validate service1 { serviceStatus = Planned, serviceLaunchedOn = Nothing }))
  , check "студент без групи не проходить" (not (null (validate user1 { userGroup = Nothing })))
  , check "викладач без групи проходить"
      (null (validate user1 { userRole = Teacher, userGroup = Nothing }))
  , check "ПІБ з одного слова не проходить" (not (null (validate author1 { authorFullName = "Один" })))
  , check "статистика: від'ємні хвилини" (not (null (validate stat1 { usMinutes = -1 })))
  , check "статистика: більше доби" (not (null (validate stat1 { usMinutes = 1441 })))
  , check "статистика: хвилини без сеансів" (not (null (validate stat1 { usSessions = 0 })))
  , check "статистика: коректна" (null (validate stat1))
  , check "надто короткі правила не проходять" (not (null (validate terms1 { utRules = "ok" })))
  , check "validVersion: 1.2 і 1.2.3" (validVersion "1.2" && validVersion "1.2.3")
  , check "validVersion: відхиляє сміття" (not (any validVersion ["1", "1.", "a.b", "1.2.3.4", ""]))
    -- Labelled
  , check "підпис сервісу - його назва" (label service1 == "Moodle")
  , check "підпис користувача - ПІБ" (label user1 == "Петренко Олена")
    -- розбір форм
  , check "форма сервісу розбирається" $
      either (const False) ((== "Новий сервіс") . serviceName) (buildRecord (Proxy :: Proxy Service) 0 (formFor validServiceForm))
  , check "обов'язкове поле не може бути порожнім" $
      isLeft (buildRecord (Proxy :: Proxy Service) 0 (formFor (("name", "") : drop 1 validServiceForm)))
  , check "необов'язкова дата може бути порожньою" $
      isJust (either (const Nothing) Just (buildRecord (Proxy :: Proxy Service) 0
        (formFor (validServiceForm ++ [("status", "planned"), ("launched_on", "")]))))
  , check "помилки правил перевірки повертаються з форми" $
      case buildRecord (Proxy :: Proxy Service) 0 (formFor (validServiceForm ++ [("current_version", "x")])) of
        Left errs -> any (T.isInfixOf "Версія") errs
        Right _ -> False
  , check "кілька помилок збираються разом" $
      case buildRecord (Proxy :: Proxy Service) 0 (formFor [("name", ""), ("kind_id", "abc")]) of
        Left errs -> length errs >= 4
        Right _ -> False
  , check "невідомий статус відхиляється" $
      isLeft (buildRecord (Proxy :: Proxy Service) 0 (formFor (validServiceForm ++ [("status", "boom")])))
  , check "дата у форматі РРРР-ММ-ДД" (parseDay "2025-03-04" == Just (day 2025 3 4))
  , check "дата з дурницею" (isNothing (parseDay "04.03.2025"))
  , check "datetime-local без секунд" (parseDateTime "2025-03-04T09:05" == Just (LocalTime (day 2025 3 4) (TimeOfDay 9 5 0)))
  , check "datetime з пробілом і секундами" (parseDateTime "2025-03-04 09:05:07" == Just (LocalTime (day 2025 3 4) (TimeOfDay 9 5 7)))
  , check "isEmail приймає й відхиляє" (isEmail "a@b.ua" && not (any isEmail ["a@b", "@b.ua", "a b@c.ua", "abc"]))
  , check "ціле число в KInt" $
      parseFields [field "n" "N" KInt] (formFor [("n", "42")]) == Right [VInt 42]
  , check "не-число в KInt" $
      isLeft (parseFields [field "n" "N" KInt] (formFor [("n", "4x")]))
  , check "перевищення довжини KText" $
      isLeft (parseFields [field "n" "N" (KText 3)] (formFor [("n", "abcd")]))
    -- генерація SQL
  , check "SELECT-колонки" (selectColumns (Proxy :: Proxy ServiceKind) == "id, name, description")
  , check "INSERT" (insertSql (Proxy :: Proxy ServiceKind) == "INSERT INTO service_kinds (name, description) VALUES (?, ?)")
  , check "UPDATE" (updateSql (Proxy :: Proxy ServiceKind) == "UPDATE service_kinds SET name = ?, description = ? WHERE id = ?")
  , check "escapeLike екранує % _ \\" (escapeLike "50%_a\\" == "50\\%\\_a\\\\")
    -- налаштування
  , check "parseDotEnv" $
      parseDotEnv "# коментар\nDB_PORT=3407\n\n APP_PORT = 4000 \nbroken" == [("DB_PORT", "3407"), ("APP_PORT", "4000")]
  ]

main :: IO ()
main = do
  results <- mapM run tests
  let failed = length (filter not results)
  putStrLn (show (length results - failed) ++ " з " ++ show (length results) ++ " тестів пройдено")
  unless (failed == 0) exitFailure
  where
    run (Result name ok) = do
      putStrLn ((if ok then "  ok   - " else "  FAIL - ") ++ name)
      pure ok
