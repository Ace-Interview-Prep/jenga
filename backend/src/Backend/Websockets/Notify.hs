{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}

{-# options_ghc -fno-warn-orphans #-}
module Backend.Websockets.Notify where


import Backend.Types
import Common.Schema
import Common.View

import Jenga.Backend.Utils.Query

import Data.Vessel
import Data.Vessel.Void
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.Beam ()
import Obelisk.Route
import Rhyolite.Account
import Rhyolite.DB.NotifyListen
import Rhyolite.Vessel.AuthenticatedV
import Rhyolite.Vessel.App

import qualified Data.Map.Monoidal as Map
import Data.Pool

notifyHandler ::
     Pool Connection
  -> (ChatAuthCredential -> (Maybe (Id Account)))
  -> DbNotification Notify
  -> FullAppV JengaApp Proxy
  -> IO (FullAppV JengaApp Identity)
notifyHandler pool a10n nm q = do
  case _dbNotification_message nm of
    _ -> pure ()
  runSerializable pool $ handleAuthenticatedQuery a10n
    (publicNotifyHandler nm)
    (privateNotifyHandler nm)
    (personalNotifyHandler nm)
    q

publicNotifyHandler
  :: DbNotification Notify
  -> PublicChatV Proxy
  -> Pg (PublicChatV Identity)
publicNotifyHandler _ VoidV = pure VoidV

personalNotifyHandler
  -- :: forall f g.
  --    (Psql m, MonadBeam Postgres m)
  :: DbNotification Notify
  -> (forall x. x -> f x -> g x) -- Id Account
  -> PersonalChatV (Compose (Map.MonoidalMap (Id Account)) f)
  -> Pg (PersonalChatV (Compose (Map.MonoidalMap (Id Account)) g))
personalNotifyHandler nm _build _v = case _dbNotification_message nm of
  Notify_Account :/ _ -> pure emptyV
  Notify_SendEmail :/ _ -> pure emptyV

privateNotifyHandler
  :: DbNotification Notify
  -> PrivateChatV Proxy
  -> Pg (PrivateChatV Identity)
privateNotifyHandler nm _v = case _dbNotification_message nm of
  Notify_Account :/ _ -> pure emptyV
  Notify_SendEmail :/ _ -> pure emptyV
