-- {-# Language DeriveAnyClass #-}
-- {-# Language StandaloneDeriving #-}
module Common.ChatSchema where

-- --experimental
-- --import Data.Map as Map
-- --import Database.Beam.Postgres
-- import Control.Lens.TH
-- import Data.Aeson
-- import Data.Functor.Identity
-- import Data.Int (Int64)
-- import Data.Text
-- import Database.Beam.Backend.SQL.Types
-- import Database.Beam.Schema
-- import GHC.Generics
-- import Network.Mail.Mime (Mail)
-- import Network.Mail.Mime.Orphans ()

-- -- data Db f = Db
-- --   { _db_account :: f (TableEntity Account)
-- --   , _db_chatroom :: f (TableEntity Chatroom)
-- --   , _db_channel :: f (TableEntity Channel)
-- --   , _db_directmessage :: f (TableEntity DirectMessage)
-- --   , _db_message :: f (TableEntity Message)
-- --   , _db_attachment :: f (TableEntity Attachment)
-- --   , _db_sendEmailTask :: f (TableEntity SendEmailTask)
-- --   } deriving (Generic, Database be)


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
--     } deriving (Generic, Beamable)
--   primaryKey = SendEmailTaskId . _sendEmailTask_id

-- deriving instance Show (PrimaryKey SendEmailTask Identity)
-- deriving instance Eq (PrimaryKey SendEmailTask Identity)
-- instance ToJSON (PrimaryKey SendEmailTask Identity)
-- instance FromJSON (PrimaryKey SendEmailTask Identity)
-- instance ToJSONKey (PrimaryKey SendEmailTask Identity)
-- instance FromJSONKey (PrimaryKey SendEmailTask Identity)

-- makeLenses ''SendEmailTask
