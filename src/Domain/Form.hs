{-# LANGUAGE OverloadedStrings #-}

-- | Розбір значень HTML-форми у 'FieldValue' за описом полів ('FieldSpec').
module Domain.Form
  ( parseFields
  , buildRecord
  , parseDay
  , parseDateTime
  , formatDay
  , formatDateTime
  , formatDateTimeInput
  , isEmail
  ) where

import Data.Int (Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Domain.Entity
import Text.Read (readMaybe)

-- | Розібрати всі поля. Повертає або список помилок, або значення у порядку 'fieldSpecs'.
parseFields :: [FieldSpec] -> Map Text Text -> Either [Text] [FieldValue]
parseFields specs input =
  case [e | Left e <- results] of
    [] -> Right [v | Right v <- results]
    es -> Left es
  where
    results = map (\s -> parseOne s (T.strip (fromMaybe "" (Map.lookup (fsName s) input)))) specs

parseOne :: FieldSpec -> Text -> Either Text FieldValue
parseOne spec raw
  | T.null raw = if fsRequired spec then Left (name <> ": поле обов'язкове") else Right VNull
  | otherwise = case fsKind spec of
      KText n
        | T.length raw > n -> Left (name <> ": не більше " <> tshow n <> " символів")
        | otherwise -> Right (VText raw)
      KTextArea -> Right (VText raw)
      KEmail
        | isEmail raw -> Right (VText raw)
        | otherwise -> Left (name <> ": некоректна адреса електронної пошти")
      KInt -> maybe (Left (name <> ": має бути цілим числом")) (Right . VInt) (readInt raw)
      KRef _ -> maybe (Left (name <> ": оберіть значення зі списку")) (Right . VInt) (readInt raw)
      KDate -> maybe (Left (name <> ": дата має вигляд РРРР-ММ-ДД")) (Right . VDate) (parseDay raw)
      KDateTime ->
        maybe (Left (name <> ": невірна дата й час")) (Right . VDateTime) (parseDateTime raw)
      KEnum opts
        | raw `elem` map fst opts -> Right (VText raw)
        | otherwise -> Left (name <> ": недопустиме значення")
  where
    name = fsLabel spec

readInt :: Text -> Maybe Int64
readInt = readMaybe . T.unpack

-- | Побудувати запис з @id@ та значень форми; тоді виконати 'validate'.
buildRecord :: Record a => Proxy a -> Int64 -> Map Text Text -> Either [Text] a
buildRecord p i input = do
  vals <- parseFields (fieldSpecs p) input
  rec' <- either (\e -> Left [e]) Right (fromValues i vals)
  case validate rec' of
    [] -> Right rec'
    es -> Left es

isEmail :: Text -> Bool
isEmail t = case T.splitOn "@" t of
  [user, domain] ->
    not (T.null user) && T.any (== '.') domain
      && not (T.isPrefixOf "." domain) && not (T.isSuffixOf "." domain)
      && not (T.any (`elem` [' ', '\t']) t)
  _ -> False

parseDay :: Text -> Maybe Day
parseDay = parseTimeM False defaultTimeLocale "%Y-%m-%d" . T.unpack

-- | Приймає формати @datetime-local@ браузера та звичайний запис через пробіл.
parseDateTime :: Text -> Maybe LocalTime
parseDateTime t = firstJust [try fmt | fmt <- fmts]
  where
    s = T.unpack t
    try fmt = parseTimeM False defaultTimeLocale fmt s
    fmts = ["%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M"]
    firstJust xs = case [x | Just x <- xs] of
      (x : _) -> Just x
      [] -> Nothing

formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

formatDateTime :: LocalTime -> Text
formatDateTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

-- | Формат значення для @<input type="datetime-local">@.
formatDateTimeInput :: LocalTime -> Text
formatDateTimeInput = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M"

tshow :: Show a => a -> Text
tshow = T.pack . show
