{-# OPTIONS_GHC -fno-warn-orphans #-}
module Backend.Config where

import Backend.DB
import Common.Schema
import Common.Constants
import Common.Route

import Jenga.Backend.Utils.HasTable
import Jenga.Backend.Utils.Email
import Jenga.Common.HasJengaConfig
import Jenga.Backend.Utils.Query
import Jenga.Common.Schema

import Rhyolite.Account
import Rhyolite.Email
import Obelisk.Route as ObR
import Control.Monad.Trans.Reader
import Database.Beam.Postgres

import Data.Pool
import Network.Mail.Mime
import Network.Mail.Mime.Orphans ()
import Web.ClientSession as CS
import Control.Monad.IO.Class
import Network.URI
import qualified Data.Text as T


data ConfigEnv = ConfigEnv
  { _dbPool :: Pool Connection
  , _clientSessionKey :: CS.Key
  , _adminEmail :: Address
  , _emailConfig :: EmailConfig
  , _routeEncoder :: IEncoder (ObR.R (FullRoute BackendRoute FrontendRoute)) PageName
    -- :: Encoder Identity Identity (ObR.R (FullRoute BackendRoute FrontendRoute)) PageName
  , _baseURI :: URI
  , _domainName :: DomainOption
  -- Add more environment information here
  }
type EnvT m a = ReaderT ConfigEnv m a

instance HasConfig ConfigEnv (IEncoder (ObR.R (FullRoute BackendRoute FrontendRoute)) PageName) where
  fromCfg = _routeEncoder
instance HasConfig ConfigEnv (Pool Connection) where
  fromCfg = _dbPool
instance HasConfig ConfigEnv AdminEmail where
  fromCfg = AdminEmail . _adminEmail
instance HasConfig ConfigEnv Key where
  fromCfg = _clientSessionKey
instance HasConfig ConfigEnv BaseURL where
  fromCfg = BaseURL . _baseURI
instance HasConfig ConfigEnv AuthCookieName where
  fromCfg _ = AuthCookieName authCookieName
instance HasConfig ConfigEnv DomainOption where
  fromCfg = _domainName

instance HasJengaTable Postgres Db Account where
  tableRef = _db_accounts db
instance HasJengaTable Postgres Db LogItemRow where
  tableRef = _db_reporting db
instance HasJengaTable Postgres Db SendEmailTask where
  tableRef = _db_sendEmailTask db
instance HasJengaTable Postgres Db OrganizationEmails where
  tableRef = _db_validOrgEmails db
instance HasJengaTable Postgres Db UserTypeTable where
  tableRef = _db_userType db

-- instance HasJengaTable Postgres Db LogItemRow where
--   tableRef = _db_reporting db

asksCfg :: Monad m => (ConfigEnv -> cfg) -> EnvT m cfg
asksCfg = Control.Monad.Trans.Reader.asks

runEnvT :: ConfigEnv -> ReaderT ConfigEnv m a -> m a
runEnvT cfg rma = runReaderT rma cfg

withDbEnv :: MonadIO m => Pg a -> EnvT m a
withDbEnv query_ = do
  dbConn <- asksCfg _dbPool
  runSerializable dbConn query_

renderFullRouteFE :: Monad m => ObR.R FrontendRoute -> EnvT m T.Text
renderFullRouteFE route = do
  enc <- asks _routeEncoder
  baseUrl <- T.pack . show <$> asks _baseURI
  pure $ baseUrl <> renderFrontendRoute enc route

askDomain :: Monad m => EnvT m (Maybe T.Text)
askDomain = do
  uri_ <- asks _baseURI
  pure $ fmap (T.pack . uriRegName) $ uriAuthority uri_

-- getJsonConfigBase :: FromJSON a => T.Text -> Map.Map T.Text BS.ByteString -> (Maybe (Either String a))
-- getJsonConfigBase key cfgs = fmap eitherDecodeStrict' $ cfgs Map.!? key

-- -- | Get and parse a json configuration
-- getJsonConfig :: (FromJSON a, Fail.MonadFail m) => T.Text -> Map.Map T.Text BS.ByteString -> m a
-- getJsonConfig k cfgs = case getJsonConfigBase k cfgs of
--   Nothing -> Fail.fail $ "getJsonConfig missing key: " <> T.unpack k
--   Just (Left err) -> Fail.fail $ "getJsonConfig invalid for key " <> T.unpack k <> " : " <> err
--   Just (Right val) -> pure val
