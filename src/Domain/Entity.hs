{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Класи типів, на яких побудована вся система.
--
-- * 'Entity'      - опис відображення типу на таблицю БД та на форму введення;
-- * 'Validatable' - бізнес-правила, які не можна виразити типами;
-- * 'Labelled'    - короткий підпис запису (для списків вибору та посилань);
-- * 'Enumerated'  - перелічувані значення (ENUM у MySQL) з українськими назвами;
-- * 'ToValue' / 'FromValue' - перетворення полів запису у нейтральне значення.
module Domain.Entity
  ( -- * Нейтральне значення поля
    FieldValue (..)
  , ToValue (..)
  , FromValue (..)
  , enumValue
  , fromEnumValue
    -- * Перелічувані типи
  , Enumerated (..)
  , enumOptions
  , parseEnum
    -- * Опис полів
  , FieldKind (..)
  , FieldSpec (..)
  , field
  , optional
    -- * Основні класи
  , Entity (..)
  , Validatable (..)
  , Labelled (..)
  , Record
  , SomeEntity (..)
  , withEntity
  , entityByTable
  ) where

import Data.Int (Int64)
import Data.List (find)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, LocalTime)

-- | Значення одного поля запису, незалежне від драйвера БД.
data FieldValue
  = VNull
  | VText Text
  | VInt Int64
  | VDate Day
  | VDateTime LocalTime
  deriving (Eq, Show)

-- | Тип, який можна записати у поле.
class ToValue a where
  toValue :: a -> FieldValue

-- | Тип, який можна прочитати з поля.
class FromValue a where
  fromValue :: FieldValue -> Either Text a

instance ToValue Text where toValue = VText
instance ToValue Int64 where toValue = VInt
instance ToValue Day where toValue = VDate
instance ToValue LocalTime where toValue = VDateTime
instance ToValue a => ToValue (Maybe a) where
  toValue = maybe VNull toValue

instance FromValue Text where
  fromValue (VText t) = Right t
  fromValue v = Left ("очікувався текст, отримано " <> T.pack (show v))

instance FromValue Int64 where
  fromValue (VInt n) = Right n
  fromValue v = Left ("очікувалось число, отримано " <> T.pack (show v))

instance FromValue Day where
  fromValue (VDate d) = Right d
  fromValue v = Left ("очікувалась дата, отримано " <> T.pack (show v))

instance FromValue LocalTime where
  fromValue (VDateTime t) = Right t
  fromValue v = Left ("очікувалась дата й час, отримано " <> T.pack (show v))

instance FromValue a => FromValue (Maybe a) where
  fromValue VNull = Right Nothing
  fromValue v = Just <$> fromValue v

-- | Перелічуваний тип: код у БД + назва для користувача.
class (Bounded a, Enum a) => Enumerated a where
  enumCode  :: a -> Text
  enumLabel :: a -> Text

enumOptions :: forall a. Enumerated a => Proxy a -> [(Text, Text)]
enumOptions _ = [(enumCode x, enumLabel x) | x <- [minBound .. maxBound :: a]]

parseEnum :: forall a. Enumerated a => Text -> Maybe a
parseEnum code = find ((== code) . enumCode) [minBound .. maxBound :: a]

enumValue :: Enumerated a => a -> FieldValue
enumValue = VText . enumCode

fromEnumValue :: Enumerated a => FieldValue -> Either Text a
fromEnumValue v = do
  t <- fromValue v
  maybe (Left ("невідоме значення переліку: " <> t)) Right (parseEnum t)

-- | Вид поля у формі та у таблиці.
data FieldKind
  = KText Int              -- ^ короткий текст (максимальна довжина)
  | KTextArea              -- ^ довгий текст
  | KEmail                 -- ^ адреса електронної пошти
  | KInt                   -- ^ ціле число
  | KDate                  -- ^ дата
  | KDateTime              -- ^ дата й час
  | KEnum [(Text, Text)]   -- ^ вибір зі списку (код, назва)
  | KRef Text              -- ^ зовнішній ключ на таблицю з такою назвою
  deriving (Eq, Show)

data FieldSpec = FieldSpec
  { fsName     :: Text        -- ^ назва колонки в БД
  , fsLabel    :: Text        -- ^ підпис для користувача
  , fsKind     :: FieldKind
  , fsRequired :: Bool
  } deriving (Eq, Show)

-- | Обов'язкове поле.
field :: Text -> Text -> FieldKind -> FieldSpec
field n l k = FieldSpec n l k True

-- | Зробити поле необов'язковим.
optional :: FieldSpec -> FieldSpec
optional f = f { fsRequired = False }

-- | Відображення доменного типу на таблицю БД.
--
-- Контракт: 'fieldSpecs' описує всі колонки, крім сурогатного ключа @id@;
-- 'toValues' повертає значення рівно в тому ж порядку; 'fromValues'
-- отримує @id@ і значення в тому ж порядку. Для нового запису @id = 0@.
class Entity a where
  tableName     :: Proxy a -> Text
  titlePlural   :: Proxy a -> Text
  titleSingular :: Proxy a -> Text
  fieldSpecs    :: Proxy a -> [FieldSpec]
  entityId      :: a -> Int64
  toValues      :: a -> [FieldValue]
  fromValues    :: Int64 -> [FieldValue] -> Either Text a

-- | Правила коректності запису понад перевірки типів полів.
-- Порожній список - запис коректний.
class Validatable a where
  validate :: a -> [Text]
  validate _ = []

-- | Короткий підпис запису.
class Labelled a where
  label :: a -> Text

-- | Усе, що потрібно для універсальної роботи із записами.
type Record a = (Entity a, Validatable a, Labelled a)

-- | Екзистенційна обгортка: дозволяє тримати різні сутності в одному списку.
data SomeEntity = forall a. Record a => SomeEntity (Proxy a)

withEntity :: SomeEntity -> (forall a. Record a => Proxy a -> r) -> r
withEntity (SomeEntity p) k = k p

entityByTable :: [SomeEntity] -> Text -> Maybe SomeEntity
entityByTable es t = find (\e -> withEntity e tableName == t) es
