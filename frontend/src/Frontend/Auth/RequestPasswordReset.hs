{-# LANGUAGE OverloadedStrings #-}

module Frontend.Auth.RequestPasswordReset where --ResetPasswordAce where

import Jenga.Frontend.Auth.RequestPasswordReset

import Frontend.Auth.AuthShell
import Templates.Partials.Errors
import Common.Request

import Rhyolite.Api (ApiRequest(..))
import Reflex.Dom.Core

import Control.Monad.Fix
import Data.Functor.Identity
import qualified Data.Text as T

primaryButton :: DomBuilder t m => T.Text -> m (Event t ())
primaryButton buttonText = do
  (e, _) <- elClass' "button" classes $ text buttonText
  pure $ domEvent Click e
  where
    classes =
      "focus:outline-none w-full p-4 mt-16 shadow-button bg-primary \
      \ font-facit font-bold text-white text-body text-center rounded \
      \ hover:bg-primary-rich active:bg-primary-desaturated \
      \ focus:ring-4 ring-primary ring-opacity-50"
requestPasswordReset :: ( DomBuilder t m
                        , PostBuild t m
                        , MonadFix m
                        , MonadHold t m
                        , Request m ~ ApiRequest token PublicRequest PrivateRequest
                        , Response m ~ Identity
                        , Requester t m
                        ) => m ()
requestPasswordReset = mdo
  rpData <- requestPasswordReset_TMPL rpCfg
  rpCfg <- requestPasswordReset_FRP (ApiRequest_Public . PublicRequest_RequestResetPassword) rpData
  pure ()


requestPasswordReset_TMPL :: ( DomBuilder t m
                             , PostBuild t m
                             , MonadFix m
                             , MonadHold t m
                             )
                          => EmailForPasswordResetConfig t
                          -> m (EmailForPasswordResetData t m)
requestPasswordReset_TMPL cfg = do
  authFormTemplate hectorRecommendation $ do
    authFormTitle "Recover Password"
    email <- authFormRow $ do
      authFormLabel "Email"
      authFormTextInput "email" "Enter your email"
    emailConfirm <- authFormRow $ do
      authFormLabel "Confirm your email"
      authFormTextInput "email" "you@example.com"

    maybeDisplay errorMessage $ _emailForPasswordResetConfig_errors cfg
    submit <- primaryButton "Reset Password"

    pure $ EmailForPasswordResetData email emailConfirm submit
