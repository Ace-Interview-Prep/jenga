{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE RecordWildCards #-}
-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Backend where

import Common.Schema (Db)
import Common.Route
import qualified Backend.Workers.SendEmail
import Backend.Websockets.Notify (notifyHandler)
import Backend.Websockets.Request (requestHandler)
import Backend.Websockets.View (fullQueryHandler)
import Backend.Config
import Backend.DB.Migrations

import Landing
import qualified Jenga.Backend.Handlers.Auth.Login as Auth.Login
import qualified Jenga.Backend.Handlers.Auth.ResetPassword as Auth.ResetPassword
import Jenga.Backend.Utils.HasConfig
import Jenga.Backend.Utils.HttpJson
import Jenga.Backend.Utils.Email
import Jenga.Backend.Utils.Snap
import Jenga.Common.Auth
import Jenga.Common.BeamExtras

import Gargoyle.PostgreSQL.Connect
import Data.Vessel
import Obelisk.Backend
import Obelisk.Route hiding ((:.), decode)
import qualified Obelisk.ExecutableConfig.Lookup as Cfg
import Obelisk.Configs
import Rhyolite.Email (EmailConfig(..))
import qualified Rhyolite.Backend.App as RhyoliteApp
import qualified Rhyolite.Account as RhyoliteAccount
import Reflex.Dom.Core as Rfx
import qualified Snap.Core as Snap
import Database.Beam.Postgres (Connection)

import Data.Pool (Pool, withResource)
import Control.Exception.Safe (finally)
import Data.Coerce (coerce)
import Control.Monad.IO.Class
import Control.Monad

import Network.Mail.Mime
import Data.Signed.ClientSession
import Web.ClientSession as CS
import qualified Network.URI as URI
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

import Jenga.Common.Errors
import GHC.Generics hiding (R)
import Data.Aeson

backend :: Backend BackendRoute FrontendRoute
backend =
  Backend
    { _backend_run = backendRun,
      _backend_routeEncoder = fullRouteEncoder
    }

-- | This Map represents all filepaths in our config/ dir
-- | for example common/route is a key
-- |
-- | NOTE: it's impossible to stop ourselves from accidentally
-- | providing bad configs but we can at least fail
-- | immediately
mkPureConfigsOrAbort
  :: (MonadIO m, MonadFail m)
  => Pool Connection
  -> Map.Map T.Text BS.ByteString
  -> m ConfigEnv
mkPureConfigsOrAbort dbPool configsDir = do
  let
    baseUri =
      fromMaybe (error "no proper URI")
      $ URI.parseURI
      $ T.unpack
      $ T.strip
      $ T.decodeUtf8
      $ fromMaybe (error "no route set")
      $ configsDir Map.!? "common/route"
    -- should be based on contents of route and
    getDomain :: URI.URI -> Maybe T.Text
    getDomain baseUri_ =
      (T.pack . URI.uriRegName)
      --- $ fromMaybe (error "no domain we can set cookies for")
      <$> URI.uriAuthority baseUri_
    -- should be based on content
    ngrokUrl = do
      fileCnts <- T.unpack . T.strip . T.decodeUtf8 <$> configsDir Map.!? "common/ngrokRoute"
      URI.parseURI fileCnts

    domain :: DomainOption
    domain =
      fromMaybe (error "no domain we can set cookies for") $
      if getDomain baseUri == Just "localhost"
         && (isJust ngrokUrl)
      then ProxiedDomain <$> (getDomain =<< ngrokUrl) <*> getDomain baseUri
      else DirectDomain <$> getDomain baseUri
  csk <- case CS.initKey <$> Map.lookup "backend/clientSessionKey" configsDir of
      Just (Right csk') -> return csk'
      Just (Left e) -> error e
      Nothing -> liftIO $ do
        error "<config/backend/clientSessionKey> does not exist. (try: dd if=/dev/urandom bs=96 count=1 of=config/backend/clientSessionKey)"
  adminAddress :: Address <- getJsonConfig "backend/adminAddress.json" configsDir
  emailConfig :: EmailConfig <- getJsonConfig "backend/smtp.json" configsDir
  pure $ ConfigEnv
    { _dbPool = dbPool
    , _clientSessionKey = csk
    , _adminEmail = adminAddress
    , _emailConfig = emailConfig
    , _routeEncoder = checkedFullRouteEncoder
    , _baseURI = baseUri
    , _domainName = domain
    }


data ContactUs = ContactUs
  { name :: T.Text
  , email :: T.Text
  , phone :: T.Text
  , message :: T.Text
  } deriving (Generic, Show)
instance FromJSON ContactUs
displayContactUs :: ContactUs -> Rfx.StaticWidget x ()
displayContactUs contact = do
  el "div" $ do
    el "h3" $ text "Contact Information"
    el "p" $ text $ "Name: " <> name contact
    el "p" $ text $ "Email: " <> email contact
    el "p" $ text $ "Phone: " <> phone contact
    el "div" $ do
      el "strong" $ text "Message:"
      el "p" $ text $ message contact
  
backendRun
  :: ((R BackendRoute -> Snap.Snap ()) -> IO a)
  -> IO a
backendRun = \serve -> Cfg.getConfigs >>= flip runConfigsT do
  cfgs <- getConfigs
  liftIO $ withDb "dbPath" $ \dbPool -> do
    -- We now have access to our database
    configEnv <- mkPureConfigsOrAbort dbPool cfgs
    let
      checkToken t = readSignedWithKey @(Id RhyoliteAccount.Account) (_clientSessionKey configEnv) t
    -- Run our database migrations
    withResource dbPool runMigrations
    -- Start up a worker: to send emails
    runEnvT configEnv $ Backend.Workers.SendEmail.sendEmailWorker
    -- Create our websocket environment
    (handleListen, wsFinalizer) <- liftIO $ RhyoliteApp.serveDbOverWebsockets
      (coerce dbPool)
      -- Requests through the JSON Websocket API
      (requestHandler configEnv)
      -- Db notifications: to send updates to the frontend
      (\nm q -> fromMaybe emptyV <$> mapDecomposedV (notifyHandler dbPool checkToken nm) q)
      -- Query Handler for type-safe DB queries from frontend
      (RhyoliteApp.QueryHandler $ \q -> fromMaybe emptyV <$> mapDecomposedV (fullQueryHandler checkToken dbPool) q)
      -- Where our scalability comes from
      RhyoliteApp.vesselFromWire
      RhyoliteApp.vesselPipeline
    flip finally wsFinalizer $ do
      liftIO $ serve $ \case
        Api_Email :/ () -> do
          runEnvT configEnv $ withPublicJSONRequestResponse @Db $ \(contact :: ContactUs) -> do
            liftIO $ print contact
            mails <- buildNewEmailHtml
              [ Address (Just "Ward Caven") "wardcaven@gmail.com"
              , Address (Just "Galen Sprout") "galen.sprout@gmail.com"
              ]
              "New Contact Us Submission"
              (displayContactUs contact)
                
            forM_ mails $ \m -> sendEmailIfNotLocal (_emailConfig configEnv) m
            pure $ (Right () :: Either (BackendError ()) ())
            --Auth.Login.loginHandler @Db email_pass
          
        BackendRoute_Missing :/ _ ->
          -- routeEncoder was not matched
          Snap.writeText "404 Page not found"
        BackendRoute_Listen :/ _ -> handleListen -- init websocket connection
        Api_Login :/ () -> do
          -- login -> set cookie
          withAuthdCORS $ runEnvT configEnv $ do
            withPublicJSONRequestResponse @Db $ \email_pass -> do
              Auth.Login.loginHandler @Db email_pass
        Api_ResetPassword :/ () -> do
          -- resetPassword -> set cookie
          withAuthdCORS $ runEnvT configEnv $ withPublicJSONRequestResponse @Db $ \token_newPass -> do
            Auth.ResetPassword.resetPasswordHandler @Db
              token_newPass
              chooseWelcomeLetter
        -- Serve pre-built Landing page as HTML
        LandingBase :/ () ->
          -- serve main landing page at /
          runEnvT configEnv $ getMainLandingPage
        LandingR :/ landingRoute ->
          -- serve all other landing pages under landing/
          runEnvT configEnv $ selectLandingPage landingRoute

-- -- Build a welcome letter as HTML through Reflex
-- chooseWelcomeLetter :: UserType -> Auth.ResetPassword.PasswordState -> (Auth.ResetPassword.Subject, StaticWidget x ())
-- chooseWelcomeLetter userType passState = case (passState, userType) of
--   (Auth.ResetPassword.HasPassword, _) ->
--     (Auth.ResetPassword.Subject "Youre password has been reset"
--     , divClass "Your password has been reset" $ do
--         text "Your Password was just reset, please let us know at \
--              \lauren@acetalent.io if you did not request this."
--     )
--   (Auth.ResetPassword.NoPassword, Self) ->
--     (Auth.ResetPassword.Subject "Welcome to Jenga", resetPasswordNewUserWelcomeLetter)
--   (Auth.ResetPassword.NoPassword, Admin) ->
--     (Auth.ResetPassword.Subject "Welcome to Jenga", text adminWelcomeLetter)
--   (Auth.ResetPassword.NoPassword, SuperUser) ->
--     (Auth.ResetPassword.Subject "Welcome to Jenga", text adminWelcomeLetter)

chooseWelcomeLetter :: UserType -> Auth.ResetPassword.PasswordState -> MkEmail x
chooseWelcomeLetter userType passState = case (passState, userType) of
  (Auth.ResetPassword.HasPassword, _) ->
    MkEmail
    { _mkEmail_subject = "Your password has been reset"
    , _mkEmail_body =
      divClass "Your password has been reset" $ do
        text "Your Password was just reset, please let us know at \
             \galen.sprout@gmail.com if you did not request this."
    }
  (Auth.ResetPassword.NoPassword, Self) ->
    MkEmail
    { _mkEmail_subject = "Welcome"
    , _mkEmail_body = resetPasswordNewUserWelcomeLetter
    }
  (Auth.ResetPassword.NoPassword, Admin) ->
    MkEmail
    { _mkEmail_subject = "Welcome"
    , _mkEmail_body = text adminWelcomeLetter
    }
  (Auth.ResetPassword.NoPassword, SuperUser) ->
    MkEmail
    { _mkEmail_subject = "Welcome"
    , _mkEmail_body = text adminWelcomeLetter
    }

adminWelcomeLetter :: T.Text
adminWelcomeLetter = "Welcome to your Jenga admin account!\n With your newly gained admin access, you are now equipped to enable user accounts, monitor signups, and track user progress—all from a user-friendly dashboard designed to simplify your user management tasks.\nShould you have any questions or require assistance, our dedicated support team is here to help. Feel free to reach out; Jenga is always ready to assist you.\nThank you for choosing Jenga."

resetPasswordNewUserWelcomeLetter :: DomBuilder t m => m ()
resetPasswordNewUserWelcomeLetter = do
  divClass "" $ do
    text "Welcome!"
