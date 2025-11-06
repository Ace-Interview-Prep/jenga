module Backend.Handlers.Names where

import Jenga.Common.Auth
import Jenga.Common.Errors
import Jenga.Backend.DB.Auth
import Backend.Config
import Backend.DB
import Common.Schema
import Common.Types

import Rhyolite.Account
import Control.Monad.IO.Class

getDisplayNamesHandler
  :: MonadIO m
  => AccountId
  -> EnvT m (Either (BackendError NameError) (Email))
getDisplayNamesHandler acctId = do
  withDbEnv (getUsersEmail (_db_accounts db) acctId) >>= \case
    Nothing ->
      -- Since we call this when logged in, this should never happen
      criticalErrorM $ NameError "email not found"
    Just email -> pure $ Right (Email email)


userError :: e -> Either (BackendError e) a
userError = Left . BUserError

criticalError :: e -> Either (BackendError e) a
criticalError = Left . BCritical

userErrorM
  :: Applicative m
  => e
  -> m (Either (BackendError e) a)
userErrorM = pure . Left . BUserError

criticalErrorM
  :: Applicative m
  => e
  -> m (Either (BackendError e) a)
criticalErrorM = pure . Left . BCritical
