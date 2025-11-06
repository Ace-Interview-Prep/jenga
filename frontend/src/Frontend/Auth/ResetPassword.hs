{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Auth.ResetPassword where

import Common.Route
import Common.Request
import Frontend.Auth.AuthShell

import Jenga.Frontend.Auth.ResetPassword
import Jenga.Common.Auth
import Jenga.Common.HasJengaConfig
import Classh
import Classh.Reflex (textS)
import Templates.Partials.Buttons
import Templates.Partials.Errors
import Templates.Types

import Obelisk.Route.Frontend
import Obelisk.Configs
import Rhyolite.Api (ApiRequest(..))
import Rhyolite.Account (PasswordResetToken)
import Reflex.Dom.Core

import Control.Monad.Trans.Reader
import Control.Monad.Fix
import Data.Functor.Identity
import Data.Signed (Signed)

resetPassword ::
  ( MonadFix m
  , MonadHold t m
  , DomBuilder t m
  , PostBuild t m
  , HasConfigs (Client m)
  , Routed t (Signed PasswordResetToken) m
  , Requester t m
  , Prerender t m
  , Request m ~ ApiRequest token PublicRequest PrivateRequest
  , Response m ~ Identity
  , HasConfig cfg (FullRouteEncoder BackendRoute FrontendRoute)
  , HasConfig cfg BaseURL
  )
  => ReaderT cfg m (Event t (AuthToken, UserType))
resetPassword = mdo
  domData <- resetPassword_TMPL domCfg
  domCfg <- resetPassword_FRP @FrontendRoute (Api_ResetPassword :/ ()) domData
  pure $ _resetPasswordConfig_result domCfg

resetPassword_TMPL
  :: Template t m
  => ResetPasswordConfig t
  -> ReaderT cfg m (ResetPasswordData t m)
resetPassword_TMPL cfg =
  authFormTemplate hectorRecommendation $ do
    elClass "h1" shell_1 $
      textS reset_password "Reset Password"
    password <- elClass "div" shell_2 $ mdo
      elClass "div" "" $ textS "" "New Password"
      inputElement $ def
        & initialAttributes .~ ("type" =: "password")
    confirmPassword <- elClass "div" shell_3 $ mdo
      elClass "div" "" $ textS "" "Confirm Password"
      inputElement $ def
        & initialAttributes .~ ("type" =: "password")
    maybeDisplay errorMessage $ _resetPasswordConfig_errors cfg
    submit <- primaryButton "Confirm"
    return $ ResetPasswordData
      { _resetPassword_password = password
      , _resetPassword_confirmPassword = confirmPassword
      , _resetPassword_submit = submit
      }
  where
    shell_1 = $(classh' [mt .~~ TWSize 12])
    shell_2 = $(classh' [mt .~~ TWSize 4, custom .~ "flex flex-col"])
    shell_3 = $(classh' [mt .~~ TWSize 8, custom .~ "flex flex-col"])

    reset_password = $(classh' [ text_font .~~ Font_Custom "Karla"
                               , text_weight .~~ Bold
                               ])
