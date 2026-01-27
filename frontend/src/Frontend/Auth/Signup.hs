{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Frontend.Auth.Signup where

import Common.Route

import Jenga.Frontend.Auth.UserSignup
import Classh
import Classh.Reflex
import Templates.Partials.Buttons
import Templates.Partials.Errors
import Templates.Partials.Checkbox
import Templates.Partials.Modal
import Templates.Types
import Frontend.Auth.AuthShell
import Common.Request

import Rhyolite.Api (ApiRequest(..))
import Obelisk.Route.Frontend
import Obelisk.Generated.Static
import Reflex
import Reflex.Dom hiding (checkbox, Checkbox(..), CheckboxConfig(..))

import Control.Monad.Fix
import Data.Functor.Identity

newSignup
  :: ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , Prerender t m
     , MonadFix m
     , PostBuild t m
     , MonadHold t m
     , Requester t m
     , Request m ~ ApiRequest token PublicRequest PrivateRequest
     , Response m ~ Identity
     )
  => m ()
newSignup = mdo
  domData <- newSignup_TMPL domCfg
  domCfg <- newUserSignup_FRP (ApiRequest_Public . PublicRequest_Signup) domData
  pure () --- $ _resetPasswordConfig_result domCfg

newSignup_TMPL
  :: ( DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , Prerender t m
     , Template t m
     )
  => UserSignupConfig t
  -> m (UserSignupData t m)
newSignup_TMPL cfg = authFormTemplate hectorRecommendation $ do
  authFormTitle "Create your account"
  elClass "p" "text-xl font-[Sarabun] mb-10 text-gray-400 text-center" $ text "Let's get started!"

  email <- authFormRow $ do
    authFormLabel "Email"
    authFormTextInput "email" "Enter your email"
  emailConfirm <- authFormRow $ do
    authFormLabel "Confirm your email"
    authFormTextInput "email" "Confirm your email"
  agreeToTerms <- elClass "div" "flex items-center mt-8" $ do
    let
      termsAttrs =
        ("type" =: "checkbox")
        <> ("class" =: "h-6 w-6")
    cbox <- checkbox False def { _checkboxConfig_attributes = constDyn termsAttrs }
    (termsEl, _) <- elClass' "div" "ml-2 mt-1 text-gray-700 flex-grow font-[Sarabun] text-lg hover:text-cyan-600 hover:underline" $ do
      text "Agree to Terms and Privacy Policy"

    _ <- modal $(static "images/obelisk.jpg") (domEvent Click termsEl) $ do
      seeTerms
    pure cbox

  submit <- elClass "div" "grid justify-items-center pt-4" $ do
    elClass "div" "flex items-center pt-10" $ primaryButton "Next"

  row [y .~~ TWSize 8] $ elClass "div" "text-2xl text-center" $ flip maybeDisplay (_signupConfig_errors cfg) text

  elClass "div" "mt-8 text-center text-gray-700 font-[Sarabun] text-xl" $ do
      routeLink (FrontendRoute_Login :/ ()) $ text "Already have an account?"

  elClass "div" "mt-4 text-center text-gray-700 font-[Sarabun] text-xl" $ do
      routeLink (FrontendRoute_RequestNewPassword :/ ()) $ text "Forgot password?"


  pure $ UserSignupData email emailConfirm agreeToTerms submit
