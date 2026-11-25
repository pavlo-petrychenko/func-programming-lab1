{-# LANGUAGE OverloadedStrings #-}

-- | Налаштування застосунку: змінні середовища, а також необов'язковий файл @.env@.
-- Значення за замовчуванням відповідають @docker-compose.yml@.
module Config
  ( Config (..)
  , loadConfig
  , parseDotEnv
  ) where

import Control.Exception (IOException, try)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Config = Config
  { cfgDbHost     :: String
  , cfgDbPort     :: Int
  , cfgDbUser     :: String
  , cfgDbPassword :: String
  , cfgDbName     :: String
  , cfgAppPort    :: Int
  } deriving (Show, Eq)

-- | Розбір вмісту @.env@: рядки виду @KEY=VALUE@, коментарі починаються з @#@.
parseDotEnv :: String -> [(String, String)]
parseDotEnv = concatMap line . lines
  where
    line l = case break (== '=') (trim l) of
      ('#' : _, _) -> []
      (k, '=' : v) | not (null k) -> [(trim k, trim v)]
      _ -> []
    trim = f . f where f = reverse . dropWhile isSpace

loadConfig :: IO Config
loadConfig = do
  dotenv <- either (\e -> const [] (e :: IOException)) parseDotEnv <$> try (readFile ".env")
  let get k def = do
        fromEnv <- lookupEnv k
        pure (fromMaybe (fromMaybe def (lookup k dotenv)) fromEnv)
      getInt k def = do
        s <- get k (show (def :: Int))
        pure (fromMaybe def (readMaybe s))
  Config
    <$> get "DB_HOST" "127.0.0.1"
    <*> getInt "DB_PORT" 3407
    <*> get "DB_USER" "faculty"
    <*> get "DB_PASSWORD" "faculty_pass"
    <*> get "DB_NAME" "faculty_services"
    <*> getInt "APP_PORT" 3000
