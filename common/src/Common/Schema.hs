{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# Language DeriveAnyClass #-}


module Common.Schema (module Common.Schema, module X) where

import Jenga.Common.Schema
import Common.SchemaHelpers as X

import Rhyolite.Account
-- import Common.Types
import Database.Beam
-- import Data.Aeson as Json
-- import Data.Aeson.TH
-- import Data.Kind (Constraint)
-- import Data.Time.Clock
-- import Database.Beam.Backend.SQL.Types (SqlSerial)
-- import Data.Int (Int64)

data Db f = D
  { -- Auth Table
    _db_accounts :: f (TableEntity Account)
    -- Emailing Tasks
  , _db_sendEmailTask :: f (TableEntity SendEmailTask)
  , _db_reporting :: f (TableEntity LogItemRow)
  , _db_validOrgEmails :: f (TableEntity OrganizationEmails)
  , _db_userType :: f (TableEntity UserTypeTable)
  -- , _db_log :: f (TableEntity LogItemRow)
  } deriving (Generic)


-- -- text-based json
-- newtype Json a = Json { unJson :: a }
--   deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid, ToJSON, ToJSONKey, FromJSON, Generic)

-- -- binary json
-- newtype Jsonb a = Jsonb { unJsonb :: a }
--   deriving newtype (Eq, Ord, Show, Read, Semigroup, Monoid, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Generic)

instance Database be Db


-- data LogItemRow f = LogItemRow
--   { _logItemRow_id :: Columnar f (SqlSerial Int64)
--   , _logItemRow_hasBeenSent :: Columnar f Bool
--   , _logItemRow_insertionTime :: Columnar f UTCTime
--   , _logItemRow_isUrgent :: Columnar f Bool
--   , _logItemRow_payload :: Columnar f LogItem
--   } deriving Generic


-- instance Beamable LogItemRow

-- type HasLogItemRowConstraint (c :: * -> Constraint) f =
--   ( c (Columnar f (SqlSerial Int64))
--   , c (Columnar f Bool)
--   , c (Columnar f UTCTime)
--   , c (Columnar f Bool)
--   , c (Columnar f LogItem)
--   )

-- type HasLogItemRowIdConstraint (c :: * -> Constraint) f =
--   ( c (Columnar f (SqlSerial Int64))
--   )

-- deriving instance HasLogItemRowConstraint Eq f => Eq (LogItemRow f)
-- deriving instance HasLogItemRowConstraint Ord f => Ord (LogItemRow f)
-- deriving instance HasLogItemRowConstraint Show f => Show (LogItemRow f)

-- deriving instance HasLogItemRowIdConstraint Eq f => Eq (PrimaryKey LogItemRow f)
-- deriving instance HasLogItemRowIdConstraint Ord f => Ord (PrimaryKey LogItemRow f)
-- deriving instance HasLogItemRowIdConstraint Read f => Read (PrimaryKey LogItemRow f)
-- deriving instance HasLogItemRowIdConstraint Show f => Show (PrimaryKey LogItemRow f)
-- deriving instance HasLogItemRowIdConstraint ToJSON f => ToJSON (PrimaryKey LogItemRow f)
-- deriving instance HasLogItemRowIdConstraint ToJSONKey f => ToJSONKey (PrimaryKey LogItemRow f)
-- deriving instance HasLogItemRowIdConstraint FromJSON f => FromJSON (PrimaryKey LogItemRow f)
-- deriving instance HasLogItemRowIdConstraint FromJSONKey f => FromJSONKey (PrimaryKey LogItemRow f)

-- deriving instance HasLogItemRowIdConstraint Semigroup f => Semigroup (PrimaryKey LogItemRow f)
-- deriving instance HasLogItemRowIdConstraint Monoid f => Monoid (PrimaryKey LogItemRow f)

-- instance Beamable (PrimaryKey LogItemRow)
-- instance Table LogItemRow where
--   newtype PrimaryKey LogItemRow f = LogItemRow_Key { logKey :: Columnar f (SqlSerial Int64) } deriving Generic
--   primaryKey = LogItemRow_Key <$> _logItemRow_id

-- data SendEmailTask f = SendEmailTask
--   { _sendEmailTask_id :: Columnar f (SqlSerial Int64)
--   , _sendEmailTask_isUrgent :: Columnar f (Maybe Bool)
--   , _sendEmailTask_finished :: Columnar f Bool
--   , _sendEmailTask_checkedOutBy :: Columnar f (Maybe Text)
--   , _sendEmailTask_payload :: Columnar f Mail
--   , _sendEmailTask_result :: Columnar f (Maybe Bool)
--   } deriving (Generic, Beamable)

-- instance Table SendEmailTask where
--   newtype PrimaryKey SendEmailTask f = SendEmailTaskId
--     { unSendEmailTaskId :: Columnar f (SqlSerial Int64)
--     } deriving stock (Generic)
--   primaryKey = SendEmailTaskId . _sendEmailTask_id

-- deriving anyclass instance Beamable (PrimaryKey SendEmailTask)

-- deriving instance Show (PrimaryKey SendEmailTask Identity)
-- deriving instance Eq (PrimaryKey SendEmailTask Identity)
-- instance ToJSON (PrimaryKey SendEmailTask Identity)
-- instance FromJSON (PrimaryKey SendEmailTask Identity)
-- instance ToJSONKey (PrimaryKey SendEmailTask Identity)
-- instance FromJSONKey (PrimaryKey SendEmailTask Identity)

-- makeLenses ''SendEmailTask
