{-# LANGUAGE OverloadedStrings #-}

-- | Доменні типи: по одному запису на кожну таблицю БД.
module Domain.Model
  ( ServiceStatus (..)
  , AuthorRole (..)
  , UserRole (..)
  , RegStatus (..)
  , ServiceKind (..)
  , Author (..)
  , Service (..)
  , ServiceAuthor (..)
  , ServiceVersion (..)
  , UsageTerms (..)
  , User (..)
  , Registration (..)
  , UsageStat (..)
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (Day, LocalTime)
import Domain.Entity (Enumerated (..))

data ServiceStatus = Active | Planned | Deprecated
  deriving (Eq, Show, Enum, Bounded)

data AuthorRole = Lead | Developer | Maintainer
  deriving (Eq, Show, Enum, Bounded)

data UserRole = Student | Teacher | Staff
  deriving (Eq, Show, Enum, Bounded)

data RegStatus = RegActive | RegBlocked | RegCancelled
  deriving (Eq, Show, Enum, Bounded)

instance Enumerated ServiceStatus where
  enumCode Active = "active"
  enumCode Planned = "planned"
  enumCode Deprecated = "deprecated"
  enumLabel Active = "Діючий"
  enumLabel Planned = "Плановий"
  enumLabel Deprecated = "Застарілий"

instance Enumerated AuthorRole where
  enumCode Lead = "lead"
  enumCode Developer = "developer"
  enumCode Maintainer = "maintainer"
  enumLabel Lead = "Керівник"
  enumLabel Developer = "Розробник"
  enumLabel Maintainer = "Супровід"

instance Enumerated UserRole where
  enumCode Student = "student"
  enumCode Teacher = "teacher"
  enumCode Staff = "staff"
  enumLabel Student = "Студент"
  enumLabel Teacher = "Викладач"
  enumLabel Staff = "Працівник"

instance Enumerated RegStatus where
  enumCode RegActive = "active"
  enumCode RegBlocked = "blocked"
  enumCode RegCancelled = "cancelled"
  enumLabel RegActive = "Активна"
  enumLabel RegBlocked = "Заблокована"
  enumLabel RegCancelled = "Скасована"

-- | Вид сервісу (довідник).
data ServiceKind = ServiceKind
  { kindId          :: Int64
  , kindName        :: Text
  , kindDescription :: Maybe Text
  } deriving (Eq, Show)

-- | Автор сервісу.
data Author = Author
  { authorId         :: Int64
  , authorFullName   :: Text
  , authorEmail      :: Text
  , authorPosition   :: Maybe Text
  , authorDepartment :: Maybe Text
  } deriving (Eq, Show)

-- | Сервіс: назва, анотація, вид, версія.
data Service = Service
  { serviceId         :: Int64
  , serviceName       :: Text
  , serviceAnnotation :: Text
  , serviceKindId     :: Int64
  , serviceVersion    :: Text
  , serviceStatus     :: ServiceStatus
  , serviceLaunchedOn :: Maybe Day
  } deriving (Eq, Show)

-- | Авторство: який автор у якому сервісі й з якою роллю.
data ServiceAuthor = ServiceAuthor
  { saId        :: Int64
  , saServiceId :: Int64
  , saAuthorId  :: Int64
  , saRole      :: AuthorRole
  } deriving (Eq, Show)

-- | Випуск версії сервісу.
data ServiceVersion = ServiceVersion
  { svId         :: Int64
  , svServiceId  :: Int64
  , svVersion    :: Text
  , svReleasedOn :: Day
  , svChangelog  :: Maybe Text
  } deriving (Eq, Show)

-- | Умови, правила та кінцевий термін використання сервісу.
data UsageTerms = UsageTerms
  { utId         :: Int64
  , utServiceId  :: Int64
  , utConditions :: Text
  , utRules      :: Text
  , utValidUntil :: Maybe Day
  } deriving (Eq, Show)

-- | Користувач (дані реєстрації).
data User = User
  { userId           :: Int64
  , userFullName     :: Text
  , userEmail        :: Text
  , userRole         :: UserRole
  , userGroup        :: Maybe Text
  , userRegisteredAt :: LocalTime
  } deriving (Eq, Show)

-- | Реєстрація користувача в сервісі.
data Registration = Registration
  { regId           :: Int64
  , regUserId       :: Int64
  , regServiceId    :: Int64
  , regRegisteredAt :: LocalTime
  , regStatus       :: RegStatus
  } deriving (Eq, Show)

-- | Статистика використання сервісу користувачем за день.
data UsageStat = UsageStat
  { usId        :: Int64
  , usServiceId :: Int64
  , usUserId    :: Int64
  , usUsedOn    :: Day
  , usSessions  :: Int64
  , usMinutes   :: Int64
  } deriving (Eq, Show)
