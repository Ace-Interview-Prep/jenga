module Backend.Websockets.View where

import Common.Schema
import Common.View
import Jenga.Backend.Utils.Query

import Data.Vessel
import Data.Vessel.Void
import Database.PostgreSQL.Simple.Beam ()
import Database.Beam.Postgres (Pg)
import Database.PostgreSQL.Simple (Connection)
import Rhyolite.Account
import Rhyolite.Vessel.AuthenticatedV
import Rhyolite.Vessel.AuthMapV (AuthMapV)

import Data.Pool (Pool)
import Control.Monad.IO.Class
import qualified Data.Map.Monoidal as Map


fullQueryHandler :: (MonadIO m, Ord token)
  => (token -> (Maybe (Id Account)))
  -> Pool Connection
  -> AuthenticatedV
       VoidV
       (AuthMapV token (Vessel PrivateVK))
       (AuthMapV token (Vessel PersonalVK))
       Proxy
  -> m (AuthenticatedV
          VoidV
          (AuthMapV token (Vessel PrivateVK))
          (AuthMapV token (Vessel PersonalVK))
          Identity)
fullQueryHandler a10n db' q = runSerializable db' $ do
  handleAuthenticatedQuery a10n
    publicQueryHandler
    privateQueryHandler
    personalQueryHandler
    q

publicQueryHandler
  :: Applicative m
  => PublicChatV Proxy
  -> m (PublicChatV Identity)
publicQueryHandler VoidV = pure VoidV

privateQueryHandler
  :: Vessel PrivateVK Proxy
  -> Pg (PrivateChatV Identity)
privateQueryHandler _ = pure emptyV

-- | This uses a data structure which is Compose (MonoidalMap (Id Account) x)
-- | which represents what data each concurrent request by a given account will receive.
-- | The compose is only necessary for the proxy => identity logic to compile
personalQueryHandler
  :: (forall x. x -> proxy x -> identity x)
  -> Vessel PersonalVK (Compose (Map.MonoidalMap (Id Account)) proxy)
  -> Pg (Vessel PersonalVK (Compose (Map.MonoidalMap (Id Account)) identity))
personalQueryHandler _buildOld _ = pure emptyV
