module Main (main) where

import Config (Config (..), loadConfig)
import DB.Repository (newDb)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Web.App (runApp)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  cfg <- loadConfig
  db <- newDb cfg
  putStrLn ("Сервер запущено: http://localhost:" ++ show (cfgAppPort cfg))
  runApp cfg db
