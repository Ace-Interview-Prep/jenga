{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Frontend.Lib (module Frontend.Lib) where

-- import Classh as C
-- import Classh.Reflex as C
-- import Jenga.Common.Errors
-- import Common.Route

-- import Web.Cookie
-- import Obelisk.Route.Frontend
-- import Obelisk.Configs
-- import Obelisk.Generated.Static
-- import Reflex.Class
-- import Reflex.Dom.Core hiding (checkbox, Checkbox(..), CheckboxConfig(..))
-- import qualified GHCJS.DOM.Types as GHCJS
-- import qualified GHCJS.DOM.Element as Element
-- import qualified GHCJS.DOM.Types as DOM
-- import Data.Proxy
-- import Control.Monad (void, forM)
-- import Control.Monad.Fix
-- import Control.Monad.IO.Class
-- import Control.Concurrent
-- import Data.Default
-- import Data.Maybe (fromMaybe, fromJust)
-- import qualified Data.Map as Map
-- import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
-- import qualified Data.ByteString.Lazy as LBS
-- import Data.Time.Clock

-- import Language.Javascript.JSaddle
-- import Data.Aeson

-- import Data.Typeable
-- import Data.Bifunctor
-- import Control.Lens ((^.), (%~))
-- import qualified Data.Aeson.Types as Aeson
-- import qualified Data.Aeson.KeyMap as Aeson
-- import qualified Data.Aeson as Aeson
-- import qualified Data.Aeson.Key as Aeson

-- import Control.Applicative (some, (<|>))
-- import Data.Functor.Identity

-- import Language.Haskell.TH
-- import Text.Parsec (ParsecT, Stream, Parsec, parse, char, alphaNum) --  (<|>))
-- import Control.Applicative (liftA2)
-- import System.Random

-- staticVideo, staticImg :: FilePath -> Q Exp
-- staticImg img = static $ "images/" <> img
-- staticVideo video = static $ "videos/" <> video

-- -- | We must use baseRoute here for mobile to work, otherwise it will think we are
-- -- | trying to lookup a local file

-- getBaseURL :: HasConfigs m => m T.Text
-- getBaseURL = T.strip . T.decodeUtf8 . fromMaybe "" <$> getConfig "common/route"

-- renderAPI :: R BackendRoute -> T.Text
-- renderAPI route = renderBackendRoute checkedFullRouteEncoder route

-- -- | Useful if we are on a mobile client
-- renderAPIFull :: HasConfigs m => R BackendRoute -> m T.Text
-- renderAPIFull route = do
--   baseRoute <- getBaseURL
--   pure $ baseRoute <> renderBackendRoute checkedFullRouteEncoder route

-- eitherDecodeText :: FromJSON a => T.Text -> Either String a
-- eitherDecodeText = eitherDecode . LBS.fromStrict . T.encodeUtf8

-- -- | Convenience function to decode JSON-encoded responses.
-- -- | Note that the function this is adapted from (decodeXhrResponse also uses _xhrResponse_responseText
-- decodeXhrResponse' :: FromJSON a => XhrResponse -> Either String a
-- decodeXhrResponse' = (fromMaybe $ Left "no response text") . fmap eitherDecodeText  . _xhrResponse_responseText

-- type RunAPI t m =
--   ( HasConfigs (Client m)
--   , Prerender t m
--   , Applicative m
--   , DomBuilder t m
--   , MonadHold t m
--   , MonadFix m
--   , PostBuild t m
--   )
-- -- | Generic Req -> Response function
-- runAPI
--   :: forall toJson fromJson t m err.
--      ( ToJSON toJson
--      , FromJSON fromJson
--      , Typeable fromJson
--      , Typeable err
--      , FromJSON err
--      , HasConfigs (Client m)
--      , Prerender t m
--      , Applicative m
--      , DomBuilder t m
--      )
--   => R BackendRoute
--   -> Event t toJson
--   -> m (Event t (RequestError err), Event t fromJson)
-- runAPI route evPayload = do
--   fmap fanResponse' $ runRequest $ performJSONRequestResponseAnnotated route evPayload

-- -- | Mutually exclusive to when you would use runAPIResponseGated
-- runAPIPostBuild :: forall toJson fromJson t m err.
--      ( ToJSON toJson
--      , FromJSON err
--      , FromJSON fromJson
--      , Typeable fromJson
--      , Typeable err
--      , HasConfigs (Client m)
--      , Prerender t m
--      , Applicative m
--      , DomBuilder t m
--      )
--   => R BackendRoute
--   -> (Event t () -> Event t toJson)
--   -> m (Event t (RequestError err), Event t fromJson)
-- runAPIPostBuild route withPb = do
--   fmap fanResponse' $ runRequest' $ performWithPb
--   where
--     runRequest' req = fmap switchDyn $ prerender (pure never) $ getPostBuild >>= req
--     performWithPb = performJSONRequestResponseAnnotated route . withPb

-- -- | We should apply this to lower level funcs such as performRequestAsync and make a PR into reflex-dom
-- -- |
-- -- | Mutually exclusive to when you would use runAPIPostBuild
-- runAPIResponseGated
--   :: forall toJson fromJson t m err.
--      ( ToJSON toJson
--      , FromJSON err
--      , FromJSON fromJson
--      , Typeable fromJson
--      , Typeable err
--      , HasConfigs (Client m)
--      , Prerender t m
--      , DomBuilder t m
--      , MonadHold t m
--      , MonadFix m
--      )
--   => R BackendRoute
--   -> Event t toJson
--   -> m (Event t (RequestError err), Event t fromJson)
-- runAPIResponseGated route evPayload = mdo
--   shouldFire <- holdDyn True $ leftmost [ False <$ evPayload , True <$ res, True <$ err ]
--   (err, res) <- runAPI route $ gate (current shouldFire ) evPayload
--   pure (err,res)

-- -- | This probably should be deprecated in favor of performJSONRequestResponseAnnotated
-- performJSONRequestResponse :: ( HasConfigs m
--                               , MonadJSM (Performable m)
--                               , PerformEvent t m
--                               , TriggerEvent t m
--                               , ToJSON toJson
--                               , FromJSON fromJson
--                               )
--                            => R BackendRoute
--                            -> Event t toJson
--                            -> m (Event t (Either String fromJson))
-- performJSONRequestResponse route jsonEv = do
--   (fmap . fmap) decodeXhrResponse' $ performJSONRequest route jsonEv

-- -- | Includes a timeout
-- -- | Updates read error to look like haskell compiler type error
-- -- | TODO: send this error to the Server to be emailed and logged
-- -- | TODO: timing out -> Request_ErrorRead
-- performJSONRequestResponseAnnotated
--   :: forall t m toJson fromJson.
--      ( HasConfigs m
--      , MonadJSM (Performable m)
--      , PerformEvent t m
--      , TriggerEvent t m
--      , ToJSON toJson
--      , FromJSON fromJson
--      , Typeable fromJson
--      )
--   => R BackendRoute
--   -> Event t toJson
--   -> m (Event t (Either T.Text fromJson))
-- performJSONRequestResponseAnnotated route jsonEv = do
--   evXhrResponse :: Event t XhrResponse <- performJSONRequest route jsonEv
--   routeText <- (renderAPIFull route)
--   pure $ leftmost
--     [ decodeXhrAnnotate routeText <$> evXhrResponse
--     ]

-- decodeXhrAnnotate
--   :: forall fromJson.
--   ( Typeable fromJson
--   , FromJSON fromJson
--   ) => T.Text -> XhrResponse -> Either T.Text fromJson
-- decodeXhrAnnotate routeString xhr =
--   let
--     annotate e = annotateError
--       routeString
--       (fromMaybe "" $ _xhrResponse_responseText xhr)
--       (T.pack . show $ typeRep (Proxy :: Proxy fromJson))
--       e
--   in
--     mapLeft annotate . decodeXhrResponse' $ xhr



-- annotateError :: T.Text -> T.Text -> T.Text -> String -> T.Text
-- annotateError routeStr actualBody expectedType baseError =
--   "Request Error from: {"
--   <> routeStr
--   <> "} Expected("
--   <> expectedType
--   <> ") but Received("
--   <> determineActual actualBody
--   <> ")"
--   <> "During base error:"
--   <> T.pack baseError
--   where
--     determineActual jsonString =
--       case Aeson.decode (LBS.fromStrict . T.encodeUtf8 $ jsonString)  :: Maybe Aeson.Value of
--         Nothing -> jsonString
--         Just val_ -> case val_ of
--           Array _  -> "Array"
--           String _ -> "String"
--           Number _ -> "Number"
--           Bool _   -> "Bool"
--           Null     -> "Null"
--           Aeson.Object keyMap_ -> case Aeson.lookup (Aeson.fromString "Right") keyMap_ of
--             -- note that we can assume left will work because even if it was encoded with another type for Right
--             -- that evidence doesn't exist in the string
--             Nothing -> T.pack . show $ keyMap_
--             Just rightCase -> case rightCase of
--               Aeson.Object keyMap__ ->
--                 let
--                   parser :: Stream s Identity Char => Parsec s u String
--                   parser = char '_' *> some alphaNum <* char '_'
--                   titlize s_ = T.toUpper (T.take 1 s_) <> (T.drop 1 s_)
--                 in
--                   case Aeson.keys keyMap__ of
--                     [] -> T.pack . show $ keyMap__
--                     (k:_) -> case parse parser "" (Aeson.toString k) of
--                       Left _ -> "An Object With Key:" <> Aeson.toText k
--                       Right parent -> titlize . T.pack $ parent
--               _ -> T.pack . show $ rightCase


-- showType :: forall a. Typeable a => a -> String
-- showType _ = show $ typeRep (Proxy :: Proxy a)

-- performJSONRequest
--   :: ( MonadJSM (Performable m)
--      , TriggerEvent t m
--      , PerformEvent t m
--      , ToJSON json
--      , HasConfigs m
--      )
--   => R BackendRoute
--   -> Event t json
--   -> m (Event t XhrResponse)
-- performJSONRequest route jsonEv = do
--   routeText <- (renderAPIFull route)
--   performRequestAsync $ withCred <$> postJson routeText <$> jsonEv

-- withCred :: XhrRequest a -> XhrRequest a
-- withCred xhr = xhr & xhrRequest_config . xhrRequestConfig_withCredentials .~ True

-- toEith :: Maybe a -> Either T.Text a
-- toEith = \case { Just a -> Right a ; Nothing -> Left "unable to parse response, please report this error" }

-- timeoutEvent
--   :: ( PerformEvent t m
--      , TriggerEvent t m
--      , MonadIO (Performable m)
--      , Trace t m
--      )
--   => NominalDiffTime
--   -> Event t a
--   -> Event t b
--   -> m (Event t ())
-- timeoutEvent timeAllowed waitingFor start = do
--   timeoutCond <- delay timeAllowed start
--   timeoutRan <- holdDyn False $ True <$ timeoutCond
--   -- todo: use start == waitingFor +1 not 0
--   (sC :: Dynamic t Int, wC :: Dynamic t Int) <- (,) <$> count start <*> count waitingFor
--   let hasResponded = (==) <$> sC <*> wC
--   traceDyn' hasResponded
--   pure $ fmap (const ()) $ ffilter id $ leftmost
--     [ gate (not <$> current timeoutRan) $ False <$ waitingFor
--     , gate (not <$> current hasResponded) $ True <$ timeoutCond
--     ]

-- runRequest :: ( Prerender t m
--               , Applicative m
--               ) => Client m (Event t r) -> m (Event t r)
-- runRequest req = fmap switchDyn $ prerender (pure never) ( comment "runRequest:Hydrated" >> req )

-- type ApiBE e a = Either (BackendError e) a

-- fanResponse
--   :: Reflex t
--   => Event t (Either String (Either (BackendError e) a))
--   -> (Event t String, Event t (BackendError e), Event t a)
-- fanResponse res =
--   let
--     (errRead, apiResult) = fanEither res
--     (errApi, good) = fanEither apiResult
--   in (errRead, errApi, good)

-- fanResponse'
--   :: Reflex t
--   => Event t (Either T.Text (Either (BackendError e) a))
--   -> (Event t (RequestError e), Event t a)
-- fanResponse' res =
--   let
--     (errRead, apiResult) = fanEither res
--     (errApi, good) = fanEither apiResult
--     err = leftmost [ Request_ErrorAPI <$> errApi
--                    , Request_ErrorRead <$> errRead
--                    ]
--   in
--     (err, good)









-- newtype Promise =
--   UnsafeToPromise JSVal
--   deriving newtype (MakeObject)

-- unsafeToPromise :: JSVal -> Promise
-- unsafeToPromise = UnsafeToPromise

-- handlePromise :: Promise -> (JSVal -> JSM b) -> (JSVal -> JSM a) -> JSM (Either a b)
-- handlePromise (UnsafeToPromise val_) thenHandler catchHandler = do
--   mvar <- liftIO $ newEmptyMVar
--   nextVal <- val_ ^. js1 ("then" :: T.Text) (fun $ \_ _ v -> thenHandler (head v) >>= liftIO . putMVar mvar . Right)
--   _ <- nextVal ^. js1 ("catch" :: T.Text) (fun $ \_ _ err -> catchHandler (head err) >>= liftIO . putMVar mvar . Left)
--   liftIO $ takeMVar mvar

-- promiseMaybe :: Promise -> (JSVal -> JSM a) -> JSM (Maybe a)
-- promiseMaybe promise thenHandler = do
--   eitherToMaybe <$> handlePromise promise thenHandler (\_ -> pure ())

-- eitherToMaybe :: Either a b -> Maybe b
-- eitherToMaybe (Right x_) = Just x_
-- eitherToMaybe _ = Nothing

-- -- | Stop event propogation on click
-- greedyButton :: forall t m a. DomBuilder t m => m a -> m (Event t (), a)
-- greedyButton c = do
--   let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
--             & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const stopPropagation)
--   (e, a) <- element "button" cfg c
--   return (domEvent Click e, a)

-- greedyButton_ :: forall t m a. DomBuilder t m => m a -> m (Event t ())
-- greedyButton_ c = fmap fst $ greedyButton c

-- -- | Simplified interface to running JS and returning it as a wrapped value
-- -- | We use either here to deal with the initial case. We could have used Maybe instead
-- -- | however this is just to clarify the use of this function.
-- -- | It is also built in this way so that you must handle what happens in the Left case / before the JS has executed
-- runJS :: (MonadJSM jsm, Applicative m, Prerender t m, Client m ~ jsm)
--       => jsm a
--       -> m (Dynamic t (Either String a))
-- runJS jsStatement = prerender (pure $ Left "Couldn't get JS value from its context") (Right <$> jsStatement) -- (do { x <- js; pure $ Right x })

-- -- | Simplified interface to running JS and returning it as an event
-- -- | The output event will only fire when the JS has been executed, this doesn't mean it will have been successful
-- -- | as this is some javascript effect which can fail for various reasons. Therefore in complex cases, you should
-- -- | probably be returning an (Event t (Either c b)) that tells you what went wrong, based on your own code/some test
-- runJSWhen :: ( Applicative m
--              , Prerender t m
--              )
--            => Event t a
--            -> (a -> JSM b)
--            -> m (Event t b)
-- runJSWhen event jsFunc = fmap switchDyn $ prerender (pure never) $ performEvent $ ffor event $ liftJSM . jsFunc

-- runJSWhenPb :: ( Prerender t m
--                , PostBuild t m
--                , PerformEvent t m
--                , TriggerEvent t m
--                , MonadIO (Performable m)
--                , MonadHold t m
--                )
--             => (JSM b)
--             -> m (Event t b)
-- runJSWhenPb jsStatement = do
--   pBuild <- getPostBuild
--   -- At ten seconds a timeout error is thrown so only until 9 is necessary
--   pbDs <- forM renderTimes $ \t_ -> do
--     delay t_ pBuild
--   output <- runJSWhen (leftmost pbDs) $ \_ -> jsStatement
--   headE output

-- redirectToHome
--   :: ( Applicative m
--      , Prerender t m
--      , PostBuild t m
--      , PerformEvent t m
--      , TriggerEvent t m
--      , MonadIO (Performable m)
--      , MonadHold t m
--      , HasConfigs m
--      )
--   => m ()
-- redirectToHome = do
--   baseRoute <- T.unpack . T.decodeUtf8 . fromMaybe "https://acetalent.io" <$> getConfig "common/route"
--   let path = renderBackendRoute checkedFullRouteEncoder $ LandingBase :/ ()
--   _ <- runJSWhenPb $ do
--     jsg (s "window") ^. js (s "location") ^. jss (s "href") (s $ baseRoute <> T.unpack path)
--   pure ()

-- runJSWhenTest :: (Prerender t m, DomBuilder t m) => m ()
-- runJSWhenTest = do
--   click <- button "click me"
--   _ <- runJSWhen click $ \_ -> clog' ("x" :: T.Text)
--   pure ()
--   where

--     clog' :: (ToJSVal a) => a -> JSM ()
--     clog' a = do
--       _ <- jsg ("console" :: T.Text) ^. js1 ("log" :: T.Text) a
--       pure ()

-- clog :: (MonadJSM m, ToJSVal a) => a -> m ()
-- clog a = do
--   _ <- liftJSM $ jsg ("console" :: T.Text) ^. js1 ("log" :: T.Text) a
--   pure ()

-- instance Show GHCJS.Blob where
--   show _ = "BLOB"

-- instance Show Language.Javascript.JSaddle.Object where
--   show _ = "OBJECT"

-- -- clogSend :: (MonadJSM m, ToJSVal a) => a -> m ()
-- -- clogSend a = do
-- --   clog a
-- --   let logRoute = renderBackendRoute checkedFullRouteEncoder (Api_SendLogs :/ ())
-- --   req <- postJson logRoute . show <$> liftJSM (valToStr a)
-- --   _ <- newXMLHttpRequestWithError req $ \err ->
-- --     let
-- --       showXhr = \case
-- --         Left e -> show $ ( Left e :: Either XhrException String)
-- --         Right res -> show $ ( Right $ _xhrResponse_responseText res  :: Either XhrException (Maybe T.Text))
-- --     in
-- --       clog $ showXhr err
-- --   pure ()

-- timer :: ( PerformEvent t m
--          , MonadIO (Performable m)
--          , TriggerEvent t m
--          , MonadFix m
--          , MonadHold t m
--          )
--       => Event t ()
--       -> Event t ()
--       -> Event t ()
--       -> m (Dynamic t NominalDiffTime)
-- timer start stop reset = do
--   start' <- countTimeFrom 0.01 start
--   timeEvent <- switchHold ((Just 0) <$ start) $ leftmost
--     [ (Just <$> start') <$ start
--     , ((Just 0 <$ never) <$ stop)
--     , (Just 0 <$ never) <$ reset
--     ]
--   timeDyn <- foldDyn (\newTime _ -> maybe 0 id newTime) 0 $ leftmost [ timeEvent, Nothing <$ reset ]
--   pure timeDyn

-- tickLossyFrom'' :: ( PerformEvent t m
--                    , MonadIO (Performable m)
--                    , TriggerEvent t m
--                    , MonadFix m
--                    ) => NominalDiffTime -> Event t a -> m (Event t TickInfo)
-- tickLossyFrom'' nomnom ev = do
--   eventTime <- performEvent $ liftIO getCurrentTime <$ ev
--   tickLossyFrom' $ (nomnom,) <$> eventTime

-- countTimeFrom :: ( PerformEvent t m
--                  , MonadIO (Performable m)
--                  , TriggerEvent t m
--                  , MonadFix m
--                  , MonadHold t m
--                  ) => NominalDiffTime -> Event t a -> m (Event t NominalDiffTime)
-- countTimeFrom interval ev = do
--   eventTime <- performEvent $ liftIO getCurrentTime <$ ev
--   eventTimeDyn <- foldDyn const undefined {-never evals, this fires once-} eventTime
--   tick <- tickLossyFrom' $ (interval,) <$> eventTime
--   pure $ attachWith (\start now_ -> diffUTCTime (_tickInfo_lastUTC now_) start) (current eventTimeDyn) tick

-- elStyle :: DomBuilder t m =>
--            T.Text
--         -> T.Text
--         -> m a
--         -> m a
-- elStyle etag styleString inner = elAttr etag ("style" =: styleString) inner

-- elDynStyle :: (PostBuild t m, DomBuilder t m) => T.Text -> Dynamic t T.Text -> m a -> m a
-- elDynStyle etag styleString inner = elDynAttr etag ((=:) "style" <$> styleString) inner

-- -- | TODO: take an element config for the options elements as well
-- -- | TODO: clean up and upstream to reflex-dom-contrib
-- dropdown'
--   :: ( MonadFix m
--      , DomBuilder t m
--      )
--   => Map.Map a T.Text
--   -> SelectElementConfig er t (DomBuilderSpace m)
--   -> m (Dynamic t a)
-- dropdown' options cfg' = mdo
--   let class' = "w-full px-4 py-3 border border-gray-300 rounded-lg focus:outline-none focus:border-[#00B9DA] font-[Sarabun] text-lg mb-5 bg-white"
--   let safeInitial = (snd . head $ Map.toList options)
--   let cfg = cfg'
--             & selectElementConfig_initialValue .~ safeInitial
--             & selectElementConfig_setValue .~ optionsEvent
--             & selectElementConfig_elementConfig . elementConfig_initialAttributes .~ ("class" =: class')
--   (selectEl, optionsEvent) <- selectElement cfg $ do
--     optionsEv <- mapM makeOpt $ fmap snd $ Map.toList options
--     pure $ leftmost optionsEv
--   let options' = Map.fromList $ fmap flipTup $ Map.toList options
--   pure $ fmap (\v -> fromJust $ Map.lookup v options') $ _selectElement_value selectEl
--   where
--     flipTup (a_,b_) = (b_,a_)
--     makeOpt optText = do
--       (e, _) <- elAttr' "option" ("value" =: optText) $ text optText
--       pure $ optText <$ domEvent Click e

-- dropdownWithDefault
--   :: ( MonadFix m
--      , DomBuilder t m
--      )
--   => Map.Map a T.Text
--   -> T.Text
--   -> SelectElementConfig er t (DomBuilderSpace m)
--   -> m (Dynamic t a)
-- dropdownWithDefault options start cfg' = mdo
--   let class' = $(classh' [ w .~~ TWSize_Full, px .~~ TWSize 4, py .~~ TWSize 3
--                          , bw .~~ B1
--                          , bc .~ [("def",Gray C300), ("focus", hex "00B9DA")]
--                          , br .~~ R_Lg
--                          , border . bStyle .~ [("focus",BNone)]
--                          , custom .~ "focus:outline-none focus:border-"
--                          , mb .~~ TWSize 5
--                          , bgColor .~~ White
--                          ])
--   let safeInitial = start
--   let cfg = cfg'
--             & selectElementConfig_initialValue .~ safeInitial
--             & selectElementConfig_setValue .~ optionsEvent
--             & selectElementConfig_elementConfig . elementConfig_initialAttributes .~ ("class" =: class')
--   (selectEl, optionsEvent) <- selectElement cfg $ do
--     optionsEv <- mapM makeOpt $ fmap snd $ Map.toList options
--     pure $ leftmost optionsEv
--   let options' = Map.fromList $ fmap flipTup $ Map.toList options
--   pure $ fmap (\v -> fromJust $ Map.lookup v options') $ _selectElement_value selectEl
--   where
--     flipTup (a_,b_) = (b_,a_)
--     makeOpt optText = do
--       (e, _) <- elAttr' "option" ("value" =: optText) $ textS $(classh' [text_font .~~ Font_Custom "Sarabun", text_size .~~ LG]) optText
--       pure $ optText <$ domEvent Click e

-- -- | TODO: upstream to Reflex Dom
-- -- | Instead of HTMLRef -> use Element
-- -- |
-- -- | Add an HTML event listener that provides a notification in Haskell land when it fires
-- -- | Prior to this, it was hard to deal with setting specific events in a functional manner
-- -- | For example: This could be used to refactor how we get our blob from the Recorder which is
-- -- | put to an array we've held reference to, however this could simply fire the blob event as
-- -- | if it was some simple click
-- type HTMLRef = JSVal
-- type HTMLEventType = T.Text
-- type HTMLEvent = JSVal
-- addHTMLEventListener ::
--   ( TriggerEvent t m
--   , Prerender t m
--   )
--   => HTMLRef
--   -> HTMLEventType
--   -> (HTMLEvent -> JSM a)
--   -> m (Event t a)
-- addHTMLEventListener htmlRef htmlEventType actn = do
--   (event, trigger) <- newTriggerEvent
--   prerender_ blank $ void $ liftJSM $ htmlRef ^. js2 ("addEventListener" :: T.Text) htmlEventType
--     (fun $ \_ _ e -> do
--         out <- actn $ head e
--         (liftIO $ trigger out)
--         pure ()
--     )
--   pure event

-- -- | Figuring this out likely takes a great deal of understanding DomBuilder generalization via classes
-- -- | ; we know this works when { DomSpace d where d is GhcjsDomSpace } but not generically for DomSpace
-- -- |
-- addHTMLEventListener' ::
--   ( TriggerEvent t m
--   , MonadJSM m
--   )
--   => Element er GhcjsDomSpace t
--   -> HTMLEventType
--   -> (HTMLEvent -> JSM a)
--   -> m (Event t a)
-- addHTMLEventListener' elementReflex htmlEventType actn = do
--   (event, trigger) <- newTriggerEvent
--   let raw = GHCJS.unElement . _element_raw $ elementReflex
--   void $ liftJSM $ raw ^. js2 ("addEventListener" :: T.Text) htmlEventType
--     (fun $ \_ _ e -> do
--         out <- actn $ head e
--         (liftIO $ trigger out)
--         pure ()
--     )
--   pure event

-- type Trigger a = (a -> IO ())

-- -- | We give you the trigger to run (presumably) conditionally. If the trigger is not called, the resulting
-- -- | event == never :: Event t a
-- -- | Eg: \e trigger -> if e == _ then getThing >>= liftIO $ trigger else pure ()
-- addHTMLEventListener'' ::
--   ( TriggerEvent t m
--   , MonadJSM m
--   )
--   => Element er GhcjsDomSpace t
--   -> HTMLEventType
--   -> (HTMLEvent -> Trigger a -> JSM ())
--   -> m (Event t a)
-- addHTMLEventListener'' elementReflex htmlEventType actn = do
--   (event, trigger) <- newTriggerEvent
--   let raw = GHCJS.unElement . _element_raw $ elementReflex
--   void $ liftJSM $ raw ^. js2 ("addEventListener" :: T.Text) htmlEventType
--     (fun $ \_ _ e -> do
--         actn (head e) trigger
--         pure ()
--     )
--   pure event

-- emptyEvent :: HTMLEvent -> JSM ()
-- emptyEvent = pure . const ()

-- tagDyn :: Reflex t => Dynamic t a -> Event t b -> Event t a
-- tagDyn d e = tag (current d) e

-- fanMaybe :: Reflex t => Event t (Maybe a) -> (Event t (), Event t a)
-- fanMaybe evMaybe = fanEither $ ffor evMaybe $ \case
--   Nothing -> Left ()
--   Just a -> Right a

-- fanBool :: Reflex t => Event t Bool -> (Event t (), Event t ())
-- fanBool e = (() <$ ffilter not e, () <$ ffilter id e)

-- data CheckboxConfig t = CheckboxConfig
--   { _checkboxConfig_setValue :: Event t Bool
--   , _checkboxConfig_attributes :: Dynamic t (Map.Map T.Text T.Text)
--   }

-- instance Reflex t => Default (CheckboxConfig t) where
--   {-# INLINABLE def #-}
--   def = CheckboxConfig { _checkboxConfig_setValue = never
--                        , _checkboxConfig_attributes = constDyn mempty
--                        }

-- data Checkbox t
--    = Checkbox { _checkbox_value :: Dynamic t Bool
--               , _checkbox_change :: Event t Bool
--               }

-- instance HasValue (Checkbox t) where
--   type Value (Checkbox t) = Dynamic t Bool
--   value = _checkbox_value

-- checkbox :: (DomBuilder t m, PostBuild t m) => Bool -> CheckboxConfig t -> m (Checkbox t)
-- checkbox checked config = do
--   let permanentAttrs = "type" =: "checkbox"
--       dAttrs = Map.delete "checked" . Map.union permanentAttrs <$> _checkboxConfig_attributes config
--   modifyAttrs <- dynamicAttributesToModifyAttributes dAttrs
--   i <- inputElement $ def
--     & inputElementConfig_initialChecked .~ checked
--     & inputElementConfig_setChecked .~ _checkboxConfig_setValue config
--     & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ Map.mapKeys (AttributeName Nothing) permanentAttrs
--     & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
--   return $ Checkbox
--     { _checkbox_value = _inputElement_checked i
--     , _checkbox_change = _inputElement_checkedChange i
--     }

-- type Consumable a = Maybe [a]

-- -- | This is part of an Event framework, consider if you used holdDyn for an event
-- -- | That starts as empty because it hasnt been received yet (ie. Nothing)
-- -- | and will be consumed or in some way the Dynamic will become empty again because
-- -- | of usage. So here Just [] or justNull means it has been used up.
-- -- | Typically that means that we should gate the action of asking for more information like from the
-- -- | server
-- justNull :: Consumable a -> Bool
-- justNull = \case
--   Just [] -> True
--   _ -> False

-- depleted :: Consumable a -> Bool
-- depleted = justNull

-- holdConsumable :: (Reflex t, MonadHold t m) => Event t [a] -> m (Dynamic t (Consumable a))
-- holdConsumable e = holdDyn Nothing $ Just <$> e

-- foldConsumable :: (Reflex t, MonadHold t m, MonadFix m) => Event t [a] -> m (Dynamic t (Consumable a))
-- foldConsumable e = foldDyn f Nothing e
--   where
--     f new_ = \case
--       Nothing -> Just new_
--       Just old -> Just $ old <> new_

-- currentWindowUnchecked :: MonadJSM m => m JSVal -- no type equivalent for Window and Document
-- currentWindowUnchecked = liftJSM $ jsg ("window" :: T.Text)

-- currentDocumentUnchecked :: MonadJSM m => m GHCJS.Document
-- currentDocumentUnchecked = liftJSM $ fmap GHCJS.Document $ jsg ("document" :: T.Text)

-- mapLeft :: (a -> b) -> Either a c -> Either b c
-- mapLeft = flip bimap Prelude.id

-- renderTimes :: [NominalDiffTime]
-- renderTimes = [0.0001, 0.5, 1.0, 2.0, 5.0 , 7.0 , 10.0, 20.0 ]

-- getPostBuildDelayed
--   :: ( PostBuild t m
--      , PerformEvent t m
--      , TriggerEvent t m
--      , MonadIO (Performable m)
--      , MonadHold t m
--      )
--   => m (Event t ())
-- getPostBuildDelayed = do
--   pBuild <- getPostBuild
--   -- At ten seconds a timeout error is thrown so only until 9 is necessary
--   pbDs <- forM renderTimes $ \t_ -> do
--     delay t_ pBuild
--   fmap (() <$) $ headE $ leftmost pbDs
--   --pure $ () <$ x

-- traceDyn' :: (Trace t m, Show a) => Dynamic t a -> m ()
-- traceDyn' d = do
--   let ev = updated d
--   traceEvent' "traceDyn" ev

-- type Trace t m = (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)

-- traceEvent' :: (Trace t m, Show a) => T.Text -> Event t a -> m ()
-- traceEvent' label ev = do
--   el "div" $ do
--     el "div" $ text label
--     vals <- foldDyn (\a_ b_ -> a_ : b_) [] ev
--     el "div" $ dynText $ T.pack . show <$> vals

-- --TH testing
-- -- xx x = "This is a test String without the characterX" <> x
-- -- yy = "hey"
-- -- | Runs at compile time
-- -- | TODO: report warning for certain stylistic oddities that kinda crossover
-- checkValidStyle :: String -> Q Exp
-- checkValidStyle str =
--   if 'X' `elem` str
--     then [| () |] -- If 'X' is in the string, compilation proceeds with no operation.
--     else fail "The string does not contain the character 'X'" -- Compilation will fail.

-- checkAtCompile :: Bool -> Q Exp
-- checkAtCompile condition =
--   if condition
--     then [| _ |] -- If 'X' is in the string, compilation proceeds with no operation.
--     else fail "oopsies"

-- scrollToTop :: MonadJSM m => m ()
-- scrollToTop = void $ liftJSM $ jsg (s "window") ^. js2 (s "scrollTo") (0 :: Int) (0 :: Int)

-- scrollElem :: MonadJSM m => JSVal -> (Float,Float) -> m ()
-- scrollElem e (x_,y_) = void $ liftJSM $ e ^. js1 (s "scrollBy") opts
--   where opts = do
--           o <- create
--           o ^. jss (s "top") y_
--           o ^. jss (s "left") x_
--           o ^. jss (s "behavior") (s "smooth")
--           pure o

-- s :: String -> String
-- s = id

-- type StaticDom = PostBuildT DomTimeline (StaticDomBuilderT DomTimeline (PerformEventT DomTimeline DomHost))

-- between' :: ParsecT s u m open -> ParsecT s u m end -> ParsecT s u m a -> ParsecT s u m [a]
-- between' open close inside = open *> (fst <$> manyTill_ inside close)

-- manyTill_ :: ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m ([a], end)
-- manyTill_ parser end = go
--   where
--     go = (([],) <$> end) <|> liftA2 (\x_ (xs, y_) -> (x_ : xs, y_)) parser go

-- between1 :: ParsecT s u m open -> ParsecT s u m end -> ParsecT s u m a -> ParsecT s u m a
-- between1 open end matchP = open *> matchP <* end

-- -- | Parses an input as email, and checks if it's valid and if it has the
-- -- obsidian.systems domain.
-- emailParse :: T.Text -> Either T.Text Bool
-- emailParse _ = Right True

-- -- TODO move to reflex-dom-core and maybe explain why/when this is necessary
-- onRender :: (Prerender t m, Monad m) => m (Event t ())
-- onRender = fmap updated (prerender blank blank)

-- getDimensions :: MonadJSM m => T.Text -> m (Float, Float)
-- getDimensions idTag = liftJSM $ do
--   doc <- jsg (s "document" )
--   idEl <- doc ^. js1 (s "getElementById" ) idTag
--   width_ <- fromJSVal =<< idEl ^. js (s "clientWidth")
--   height_ <- fromJSVal =<< idEl ^. js (s "clientHeight")
--   clog (width_,height_)
--   pure (fromMaybe 500 width_, fromMaybe 500 height_)



-- -- IO so that we can get randomization
-- shuffle :: [a] -> IO [a]
-- shuffle [] = pure []
-- shuffle xs = do
--   picked <- randomRIO (1, length xs) -- so that the length of the first is always 1+
--   let (pre, secnd) = splitAt picked xs -- picked is also the length of the first
--   xs' <- shuffle $ secnd <> tail pre
--   pure $ head pre : xs'

-- shuffleWell :: [a] -> IO [a]
-- shuffleWell (a_:b_:[]) = randomRIO (1,10) >>= \x_ -> if (x_ :: Int) < 6 then pure (a_:b_:[]) else pure (b_:a_:[] )
-- shuffleWell xs = shuffle =<< pure . reverse =<< shuffle xs



-- elDynHtmlAttr_
--   :: ( DomBuilder t m
--      , PostBuild t m
--      , PerformEvent t m
--      , DOM.MonadJSM (Performable m)
--      , Element.IsElement (RawElement (DomBuilderSpace m))
--      )
--   => T.Text
--   -> Map.Map T.Text T.Text
--   -> Dynamic t T.Text
--   -> m (Element EventResult (DomBuilderSpace m) t)
-- elDynHtmlAttr_ elementTag attrs_ html = do
--   let cfg = def & initialAttributes .~ Map.mapKeys (AttributeName Nothing) attrs_
--   (e, _) <- element elementTag cfg $ return ()
--   postBuild <- getPostBuild
--   performEvent_ $ liftJSM . Element.setInnerHTML (_element_raw e) <$> leftmost [updated html, tag (current html) postBuild]
--   return e
