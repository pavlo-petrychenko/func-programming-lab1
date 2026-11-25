{-# LANGUAGE OverloadedStrings #-}

-- | Перетворення між нейтральним 'FieldValue' та значеннями драйвера MySQL.
module DB.Convert
  ( toMySQL
  , fromMySQL
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Database.MySQL.Base (MySQLValue (..))
import Domain.Entity

toMySQL :: FieldValue -> MySQLValue
toMySQL VNull = MySQLNull
toMySQL (VText t) = MySQLText t
toMySQL (VInt n) = MySQLInt64 n
toMySQL (VDate d) = MySQLDate d
toMySQL (VDateTime t) = MySQLDateTime t

-- | Прочитати значення колонки відповідно до її виду в 'FieldSpec'.
fromMySQL :: FieldKind -> MySQLValue -> Either Text FieldValue
fromMySQL _ MySQLNull = Right VNull
fromMySQL kind v = case kind of
  KInt -> VInt <$> asInt v
  KRef _ -> VInt <$> asInt v
  KDate -> case v of
    MySQLDate d -> Right (VDate d)
    _ -> bad
  KDateTime -> case v of
    MySQLDateTime t -> Right (VDateTime t)
    MySQLTimeStamp t -> Right (VDateTime t)
    _ -> bad
  _ -> VText <$> asText v
  where
    bad = Left ("неочікуваний тип значення з БД: " <> T.pack (show v))

asText :: MySQLValue -> Either Text Text
asText (MySQLText t) = Right t
asText (MySQLBytes b) = Right (TE.decodeUtf8With lenientDecode b)
asText v = Left ("очікувався текст, отримано " <> T.pack (show v))

asInt :: MySQLValue -> Either Text Int64
asInt v = case v of
  MySQLInt64 n -> Right n
  MySQLInt64U n -> Right (fromIntegral n)
  MySQLInt32 n -> Right (fromIntegral n)
  MySQLInt32U n -> Right (fromIntegral n)
  MySQLInt16 n -> Right (fromIntegral n)
  MySQLInt16U n -> Right (fromIntegral n)
  MySQLInt8 n -> Right (fromIntegral n)
  MySQLInt8U n -> Right (fromIntegral n)
  MySQLDecimal d -> Right (truncate d)
  _ -> Left ("очікувалось число, отримано " <> T.pack (show v))
