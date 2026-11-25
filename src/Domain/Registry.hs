-- | Реєстр усіх сутностей системи. Кожна сутність - окрема таблиця БД.
module Domain.Registry
  ( allEntities
  , tableNames
  ) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Domain.Entity
import Domain.Instances ()
import Domain.Model

-- | Порядок важливий: таблиці-довідники йдуть перед тими, що на них посилаються.
allEntities :: [SomeEntity]
allEntities =
  [ SomeEntity (Proxy :: Proxy ServiceKind)
  , SomeEntity (Proxy :: Proxy Author)
  , SomeEntity (Proxy :: Proxy Service)
  , SomeEntity (Proxy :: Proxy ServiceAuthor)
  , SomeEntity (Proxy :: Proxy ServiceVersion)
  , SomeEntity (Proxy :: Proxy UsageTerms)
  , SomeEntity (Proxy :: Proxy User)
  , SomeEntity (Proxy :: Proxy Registration)
  , SomeEntity (Proxy :: Proxy UsageStat)
  ]

tableNames :: [Text]
tableNames = map (`withEntity` tableName) allEntities
