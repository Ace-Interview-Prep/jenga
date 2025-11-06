{-# LANGUAGE NoMonomorphismRestriction #-}


module Backend.Workers.SendEmail where

import Backend.DB (db)
import RhyConcurrent
import Common.Schema
import Backend.Types
import Backend.Config

import Jenga.Backend.Utils.Email
import Jenga.Backend.Utils.Query (runDbQuiet)
import Jenga.Common.Schema

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad (forM_)
import Database.Beam.Query
import Database.Beam.Postgres

import Data.Int (Int64)
import Database.Beam.Backend.SQL.Types (SqlSerial(..))



sendEmailWorker :: MonadIO m => EnvT m ()
sendEmailWorker = do
  dbConnection <- asksCfg _dbPool
  emailConfig' <- asksCfg _emailConfig
  env <- asksCfg Prelude.id
  _ <- taggedWorker "EmailSender" env (60 * microsecondsInSecond) $ runEnvT env $ do
    incomplete' <- withDbEnv getUnsentEmails
    --incomplete <- liftIO $ shuffleWell incomplete'
    forM_ incomplete' $ \emailTask -> do
      liftIO $ putStrLn "SendEmail"
      sendEmailIfNotLocal emailConfig' (_sendEmailTask_payload emailTask) >>= \case
        Left e -> liftIO $ print e
        Right () -> do
          runDbQuiet dbConnection $ markEmailTaskComplete (_sendEmailTask_id emailTask)
  pure ()

getUnsentEmails :: Pg [SendEmailTask Identity]
getUnsentEmails = runSelectReturningList $ select $ all_ (_db_sendEmailTask db)

markEmailTaskComplete :: SqlSerial Int64 -> Pg ()
markEmailTaskComplete emailTaskId =
  runDelete $ delete (_db_sendEmailTask db) $ (\task -> _sendEmailTask_id task ==. (val_ emailTaskId))
