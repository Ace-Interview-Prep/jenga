module Backend.Websockets.Request where

import qualified Backend.Handlers.Names as Names
import Backend.Websockets.Notify ()
import Backend.Config
import Backend.Types
import Common.Request
import Common.Route
import Common.Schema

import qualified Jenga.Backend.Handlers.Auth.RequestPasswordReset as Auth.RequestPasswordReset
import qualified Jenga.Backend.Handlers.Auth.UserSignup as Auth.Signup
import Jenga.Backend.Utils.AuthHandlers (wsRequestAuth)
import Jenga.Backend.Utils.ErrorHandling
import Jenga.Backend.Utils.Email
import Jenga.Common.HasJengaConfig
import Jenga.Common.Auth

import Rhyolite.Api
import Rhyolite.Backend.App
import qualified Reflex.Dom.Core as Rfx

import Control.Monad.Trans.Reader

--experimental
-- import qualified Data.Text.Encoding as T
-- import Text.Email.Validate



requestHandler
  :: ConfigEnv
  -> RequestHandler
  (ApiRequest
    AuthToken
    PublicRequest
    PrivateRequest
  )
  IO
requestHandler cfgEnv = RequestHandler $ \case
  ApiRequest_Public pubRequest -> case pubRequest of
    PublicRequest_RequestResetPassword email -> do
      flip runReaderT cfgEnv $ do
        withErrorReporting @Db $ Auth.RequestPasswordReset.requestPasswordResetHandler
          @Db
          @BackendRoute
          @Notify
          FrontendRoute_ResetPassword
          (email)
    PublicRequest_Signup newUserEmail -> do
      flip runReaderT cfgEnv $ do
        
        withErrorReporting @Db $ Auth.Signup.userSignupHandler
          @Db
          @BackendRoute
          (newUserEmail)
          FrontendRoute_ResetPassword
          genericNewAccountMkEmail
  ApiRequest_Private authToken req -> do
    case req of
      PrivateRequest_Names -> do
        flip runReaderT cfgEnv $ do
          wsRequestAuth @Db authToken $ \user -> do
            Names.getDisplayNamesHandler user

genericNewAccountMkEmail :: Link -> MkEmail x
genericNewAccountMkEmail link_ = MkEmail
  { _mkEmail_subject = "New account creation requested"
  , _mkEmail_body = do
      Rfx.el "div" $ do
        Rfx.el "div" $ Rfx.text "New account creation requested"
        Rfx.el "div" $ Rfx.text "go the following address to set your password:"
        Rfx.el "div" $ Rfx.text $ getLink link_
        Rfx.el "br" Rfx.blank
        Rfx.el "div" $ Rfx.text "if you didn't request this action, please ignore this email."
  }
type Logger = SomeError -> IO ()
