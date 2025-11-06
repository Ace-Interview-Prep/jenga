module Landing.Impl where

import Landing.Static
import Lamarckian.Compiler
import Common.Route

import Language.Haskell.TH
import Obelisk.Route
import System.FilePath
import System.Directory
import qualified Data.ByteString as BS
import Lamarckian.Types

type Compiler = Q

atCompileTime :: a -> Compiler a
atCompileTime = pure


getPlainTextPageCompiled :: R MainLandingRoute -> IO BS.ByteString -> Q Exp
getPlainTextPageCompiled route mkWidget = do
  widget <- runIO mkWidget
  compilePlainTextStaticSite siteCfg route widget

getPageCompiled :: R MainLandingRoute -> StaticWidget' MainLandingRoute t () -> Q Exp
getPageCompiled route widget =
  compileStaticSite siteCfg route widget

siteCfg :: StaticSite MainLandingRoute
siteCfg = StaticSite
  -- NOTE: base folder ("staticSite") must match a static assets dir
  { --_staticSite_baseFilePath = "staticSite" </> "src" </> "html"
    _staticSite_baseWritableFolder = "staticSite" </> "src"
    -- TODO: split up head and body... OF THE DOM I MEAN!!
    --, _staticSite_router = staticRouter'
  , _staticSite_staticFilePath = Landing.Static.staticFilePath
  , _staticSite_routeEncoder = \r -> renderBackendRoute checkedFullRouteEncoder $ LandingR :/ r
  , _staticSite_subFolder = Just "html"
  , _staticSite_localHint = doesDirectoryExist "config"
  }


getMainPageCompiled :: R BackendRoute -> StaticWidget' BackendRoute t () -> Q Exp
getMainPageCompiled route widget =
  compileStaticSite site route widget
  where
     site = StaticSite
       -- NOTE: base folder ("staticSite") must match a static assets dir
       { _staticSite_baseWritableFolder = "staticSite" </> "src"
         -- </> "html"
       -- TODO: split up head and body... OF THE DOM I MEAN!!
       --, _staticSite_router = staticRouter'
       , _staticSite_staticFilePath = Landing.Static.staticFilePath
       , _staticSite_routeEncoder = renderBackendRoute checkedFullRouteEncoder
       , _staticSite_subFolder = Just "html"
       , _staticSite_localHint = doesDirectoryExist "config"
       }


-- getPageCompiled :: R MainLandingRoute -> Q Exp
-- getPageCompiled route =
--   compileStaticSite site route
--   where
--      site = StaticSite
--        -- NOTE: base folder ("staticSite") must match a static assets dir
--        { _staticSite_baseFilePath = "staticSite" </> "src" </> "html"
--        -- TODO: split up head and body... OF THE DOM I MEAN!!
--        , _staticSite_router = staticRouter'
--        , _staticSite_getFromFile = staticFilePath
--        , _staticSite_routeEncoder = \r -> renderBackendRoute checkedFullRouteEncoder $ LandingR :/ r
--        }

-- getPageCompiled' :: StaticWidget' r t () -> Q Exp
-- getPageCompiled' route =
--   compileStaticSite site page
--   where
--      site = StaticSite
--        -- NOTE: base folder ("staticSite") must match a static assets dir
--        { _staticSite_baseFilePath = "staticSite" </> "src" </> "html"
--        -- TODO: split up head and body... OF THE DOM I MEAN!!
--        , _staticSite_router = page --staticRouter'
--        , _staticSite_getFromFile = staticFilePath
--        , _staticSite_routeEncoder = \r -> renderBackendRoute checkedFullRouteEncoder $ LandingR :/ r
--        }
