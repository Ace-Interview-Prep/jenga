{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

module Frontend where

import Frontend.Auth.Login
import Frontend.Auth.Signup
import Frontend.Auth.ResetPassword
import Frontend.Auth.RequestPasswordReset

import Common.Route
import Common.View (JengaApp)
import Common.Constants (authCookieName, userTypeCookieName)

import Jenga.Frontend.Platform (RouteFilePath(..), readJengaBaseURL, chooseJengaRouteFile)
import Jenga.Common.HasJengaConfig
import Jenga.Common.Auth
import Jenga.Common.Cookie
import Rhyolite.Frontend.App
import Rhyolite.Frontend.Auth.App
import Rhyolite.Frontend.Cookie (base64Decode)
import Obelisk.Route.Frontend
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Generated.Static
import Reflex.Dom.Core hiding (Request)
import Language.Javascript.JSaddle (liftJSM, js, js1, jsg)

import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Trans.Reader
import Data.Maybe
import qualified Witherable as W
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.List as L
import qualified Data.ByteString as BS

data MyFrontendConfig = MyFrontendConfig
  { _baseURL :: BaseURL
  , _routeEncoder :: FullRouteEncoder BackendRoute FrontendRoute
  , _authCookieName :: AuthCookieName
  , _userTypeCookieName :: UserTypeCookieName
  , _routeFile :: RouteFilePath
  }
instance HasConfig MyFrontendConfig BaseURL where
  fromCfg = _baseURL
instance HasConfig MyFrontendConfig (FullRouteEncoder BackendRoute FrontendRoute) where
  fromCfg = _routeEncoder
instance HasConfig MyFrontendConfig AuthCookieName where
  fromCfg = _authCookieName
instance HasConfig MyFrontendConfig UserTypeCookieName where
  fromCfg = _userTypeCookieName
instance HasConfig MyFrontendConfig RouteFilePath where
  fromCfg = _routeFile
type JengaWidget = FullAppWidget JengaApp
-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Jenga Minimal Example"
      elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1, shrink-to-fit=no") blank

      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "js/lib.js")) blank
      elAttr "link" ("href" =: $(static "css/styles.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
      elAttr "script" ("src" =: "https://cdn.tailwindcss.com") blank
  , _frontend_body = mdo
      url <- fromMaybe (error "invalid base url") <$> readJengaBaseURL
      routeFile <- chooseJengaRouteFile
      let frontendConfig = MyFrontendConfig
            { _baseURL = url
            , _routeEncoder = checkedFullRouteEncoder
            , _authCookieName = AuthCookieName Common.Constants.authCookieName
            , _userTypeCookieName = UserTypeCookieName Common.Constants.userTypeCookieName
            , _routeFile = routeFile
            }

      runJengaWidget frontendConfig $ \cfg -> mdo
        mAuthCookie <- flip runReaderT cfg $ listenBrowserCookieState authChange
        authChange <- fmap switchDyn $ appRouter cfg mAuthCookie
        pure ()
  }

listenBrowserCookieState
  :: ( HasConfig cfg AuthCookieName
     , HasConfig cfg UserTypeCookieName
     , HasCookies m
     , MonadHold t m
     )
  => Event t (Maybe (AuthToken, UserType))
  -> ReaderT cfg m (Dynamic t (Maybe (AuthToken, UserType)))
listenBrowserCookieState authChange = do
  authCookieName_ <- getAuthCookieName <$> asksM
  userTypeCookieName_ <- getUserTypeCookieName <$> asksM

  cookies <- askCookies
  let
    --pad x = "\"" <> x <> "\""
    eithToMaybe = \case { Right x -> Just x ; Left _ -> Nothing }
    authCookieEntry :: Maybe BS.ByteString
    authCookieEntry = L.lookup (T.encodeUtf8 authCookieName_) cookies
    baseAuth = keylessFrom =<< authCookieEntry
    uType = (\x -> eithToMaybe . base64Decode $ x) =<< L.lookup (T.encodeUtf8 userTypeCookieName_) cookies
    uType' :: Maybe UserType
    uType' = (A.decodeStrict =<< uType)
    mAuthCookie0 = (,) <$> baseAuth <*> uType'
  mAuthCookie :: Dynamic t (Maybe (AuthToken, UserType)) <- holdDyn mAuthCookie0 authChange
  pure mAuthCookie

redirectIfAuthenticated
  :: ( PostBuild t m
     , SetRoute t (R FrontendRoute) m
     )
  => Dynamic t (Maybe (AuthToken, UserType))
  -> ReaderT cfg m ()
redirectIfAuthenticated mAuthCookie = do
  pb <- getPostBuild
  let hasAuth = W.catMaybes $ leftmost
        [ tag (current mAuthCookie) pb
        , updated mAuthCookie
        ]

  setRoute $ ffor hasAuth $ \case
    _ -> FrontendRoute_Main :/ ()
    -- Fronten
    -- (_, Self) -> FrontendRoute_Pages :/ Homepage :/ ()
    -- (_, Admin) -> FrontendRoute_Pages :/ AdminRoute :/ AdminHome :/ ()

appRouter
  :: ( HasConfigs m
     , SetRoute t (R FrontendRoute) m
     , Prerender t m
     , DomBuilder t m
     , ObeliskWidget t (R FrontendRoute) m
     )
  => MyFrontendConfig
  -> Dynamic t (Maybe (AuthToken, UserType))
  -> RoutedT t (R FrontendRoute) (JengaWidget t m) (Dynamic t (Event t (Maybe (AuthToken, UserType))))
appRouter cfg mAuthCookie = cssContext $ do
  subRoute $ \case
    FrontendRoute_Login -> flip runReaderT cfg $ do
      token <- login
      redirectIfAuthenticated mAuthCookie
      pure $ Just <$> token
    FrontendRoute_ResetPassword -> flip runReaderT cfg $ do
      token <- resetPassword
      redirectIfAuthenticated mAuthCookie
      pure $ Just <$> token
    FrontendRoute_RequestNewPassword -> do
      requestPasswordReset
      pure never
    FrontendRoute_Signup -> do
      newSignup
      pure never
    FrontendRoute_Main -> do
      el "h1" $ text "Welcome to Jenga!"
      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void
        $ jsg ("window" :: T.Text)
        ^. js ("skeleton_lib" :: T.Text)
        ^. js1 ("log" :: T.Text) ("Hello, World!" :: T.Text)
      --elAttr "img" ("src" =: $(static "jenga.jpg")) blank
      el "div" $ do
        let
          cfgPath = "common/example"
          path = "config/" <> cfgPath
        Obelisk.Configs.getConfig cfgPath >>= \case
          Nothing -> text $ "No config file found in " <> path
          Just bytes -> case T.decodeUtf8' bytes of
            Left ue -> text $ "Couldn't decode " <> path <> " : " <> T.pack (show ue)
            Right s -> text s

      return never

cssContext :: DomBuilder t m => m a -> m a
cssContext = divClass "fixed top-0 left-0 w-full h-full overflow-auto"


runJengaWidget
  :: ( DomBuilder t m
     , HasConfigs m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadHold t m
     , PostBuild t m
     , MonadFix m
     , Prerender t m
     )
  => MyFrontendConfig
  -> (MyFrontendConfig -> RoutedT t
       (R FrontendRoute)
       (JengaWidget t m)
       a
     )
  -- ^ Child widget
  -> RoutedT t (R FrontendRoute) m a
runJengaWidget cfg w = do
  --path <- chooseRouteFile
  fmap snd . runObeliskRhyoliteWidget
    vesselToWire
    (getRouteFile . _routeFile $ cfg)
    checkedFullRouteEncoder
    (BackendRoute_Listen :/ ())
    $
    (w cfg)
