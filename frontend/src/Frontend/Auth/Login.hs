{-# LANGUAGE OverloadedStrings #-}
module Frontend.Auth.Login where

import Jenga.Frontend.Auth.Login
import Frontend.Auth.AuthShell
import Jenga.Common.Auth
  ( AuthToken
  , UserType(..)
  )

import Classh
import Classh.Reflex
import Common.Route
import Templates.Partials.Buttons
import Templates.Partials.Errors
import Templates.Types
import Jenga.Common.HasJengaConfig

import Obelisk.Route.Frontend
import Obelisk.Configs
import Reflex.Dom.Core

import Control.Monad.Trans.Reader
import Control.Monad.Fix

-- data LoginConfig t = LoginConfig
--   { _loginConfig_errors :: Dynamic t (Maybe T.Text)
--   , _loginConfig_submitEnabled :: Dynamic t Bool
--   }

-- data Login t m = Login
--   { _login_username :: InputEl t m
--   , _login_password :: InputEl t m
--   , _login_submit :: Event t ()
--   , _login_forgotPassword :: Event t ()
--   }


login ::
  forall cfg t m.
  ( MonadFix m
  , MonadHold t m
  , DomBuilder t m
  , PostBuild t m
  , SetRoute t (R FrontendRoute) m
  , HasConfigs (Client m)
  , Prerender t m
  , Requester t m
  -- , Request m ~ ApiRequest token PublicRequest PrivateRequest
  -- , Response m ~ Identity
  , HasConfig cfg (FullRouteEncoder BackendRoute FrontendRoute)
  , HasConfig cfg BaseURL
  )
  => ReaderT cfg m (Event t (AuthToken, UserType))
login = mdo
  loginData <- loginTemplate' loginCfg
  loginCfg <- login_FRP (Api_Login :/ ()) (FrontendRoute_RequestNewPassword :/ ()) loginData
  pure $ _loginConfig_token loginCfg

loginTemplate'
  :: Template t m
  => LoginConfig t
  -> ReaderT cfg m (LoginData t m)
loginTemplate' cfg = authFormTemplate hectorRecommendation $ do
  authFormTitle "Log In"
  username <- authFormRow $ do
    authFormLabel "Email"
    authFormTextInput "email" "Enter your email"
  password <- authFormRow $ do
    authFormLabel "Password"
    authFormTextInput "password" "Enter your password"
  maybeDisplay errorMessage $ _loginConfig_errors cfg
  submit <- primaryButton' (_loginConfig_submitEnabled cfg) "Log in"
  forgotPassword <- fmap (domEvent Click . fst) $ do
    elClass' "div" $(classh' [w .~~ TWSize_Full
                             , custom .~ "text-center"
                             , mt .~~ TWSize 8
                             ]) $
      textS $(classh' [ text_font .~~ Font_Custom "Sarabun"
                      , text_color .|~ [White, White, Gray C500]
                      , text_size .~~ Base
                      , custom .~ "cursor-pointer"
                      ]) "Forgot password?"
  return $ LoginData username password submit forgotPassword
