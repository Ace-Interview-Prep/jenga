{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Frontend.Auth.Authentication where

import Control.Category
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.IO.Class
import Data.Vessel
import Obelisk.Route
import Obelisk.Route.Frontend
import Prelude hiding ((.), id)
import Reflex.Dom.Core
import Rhyolite.Frontend.App
import Rhyolite.Frontend.Auth.App
import Rhyolite.Vessel.AuthenticatedV
import Rhyolite.Vessel.ErrorV

import Common.Route
import Common.View
import Common.Types (UserType)
import Common.ChatSchema (AuthToken)

authenticateWithToken
  :: ( MonadHold t m
     , MonadFix m
     , PostBuild t m
     , DomBuilder t m
     , SetRoute t (R FrontendRoute) m
     , Ref m ~ Ref (Performable m)
     , PerformEvent t m
     , MonadIO (Performable m)
     , TriggerEvent t m
     )
  => Dynamic t (Maybe (AuthToken, UserType))
  -> RoutedT t r (AuthAppWidget RhyoliteExampleApp t m) (Event t a)
  -> RoutedT t r (FullAppWidget RhyoliteExampleApp t m) (Event t a)
authenticateWithToken mToken page = do
  pb <- delay 0.1 =<< getPostBuild
  fmap switchDyn $ widgetHold (pure never) $ ffor (leftmost [tag (current mToken) pb, updated mToken]) $ \case
    Nothing -> do
      r <- delay 0.1 =<< getPostBuild -- TODO: dodgy
      setRoute $ FrontendRoute_Login :/ () <$ r -- TODO: Super dodgy

      display mToken
      pure never
    Just (token, _) -> do
      fmap switchDyn $ mapRoutedT (authenticatedWidget (Proxy :: Proxy RhyoliteExampleApp) token) $ handleAuthFailure renderInvalid page

-- TODO: something like this belongs in rhyolite-frontend
handleAuthFailure
  :: (PostBuild t m, MonadFix m, MonadHold t m, Adjustable t m)
  => RoutedT t r (AuthErrorAppWidget RhyoliteExampleApp t m) a
  -> RoutedT t r (AuthAppWidget RhyoliteExampleApp t m) a
  -> RoutedT t r (AuthErrorAppWidget RhyoliteExampleApp t m) (Dynamic t a)
handleAuthFailure placeholder authenticatedChild = do
  pb <- getPostBuild
  errView <- eitherDyn . fmap observeErrorV =<< mapRoutedT (RhyoliteWidget . withQueryT disperseAuthenticatedErrorV) askQueryResult
  widgetHold placeholder $ ffor (leftmost [tag (current errView) pb, updated errView]) $ \case
    Left _ -> placeholder
    Right _ -> mapRoutedT (RhyoliteWidget . withQueryT (disperseAuthenticatedErrorV . unsafeProjectV) . unRhyoliteWidget) authenticatedChild


renderInvalid
  :: (DomBuilder t m)
  => m (Event t x)
renderInvalid = do
  el "p" $ text $ "" -- "Your token is invalid." -- TODO(galen): this shows up during go to mock
  pure never
