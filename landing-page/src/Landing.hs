{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Landing where

--import Text.IStr

import Landing.Impl
import Landing.Utils
import Landing.Markdown
import Landing.Pages.Markdown.W1
import Landing.Pages.Markdown.FirstProgram
--import Landing.Pages.Html.About
import Landing.Pages.Html.Blog
import Landing.Pages.Html.Index

import Lamarckian.Template
import Lamarckian.MMark
import Lamarckian.Snap

import Common.Route
import Obelisk.Route

import Snap
import Control.Monad.IO.Class
import Control.Monad (void)
--import Control.Monad.IO.Class
import qualified Data.Map as Map
import qualified Data.Text as T
--import Data.Char
-- | Route Values/Literals still need to operate independently of each other


data ReadMode = Local | Prod

-- | This is on a seperate route tree so we need a second config
getMainLandingPage :: MonadSnap m => m ()
getMainLandingPage = void . serveCompressed $
  $(getMainPageCompiled (LandingBase :/ ()) $ indexHs)

-- | Idea, $(getPagesCompiled :: [page] -> [filepath]) -- or better data structure, then you can pattern match on them
-- | and then we would have more power over certain things
selectLandingPage :: MonadSnap m => R MainLandingRoute -> m ()
selectLandingPage r = case r of
  Blog :/ () ->
    serve $(getPageCompiled (Blog :/ ()) $ blog)
  AboutUs :/ () -> do
    --- $(getPageCompiled (AboutUs :/ ()) aboutPage)
    -- let x :: T.Text
    --     x = $(immarkFile "/home/lazylambda/code/opensource/jenga/README.md")
    -- -- liftIO $ print x
    (liftIO $ readIMMarkFile "tutorials.md") >>= \case
      Left e -> writeText $ T.pack e
      Right x -> do
        liftIO $ writeFile "out.html" $ T.unpack x
        let styleEl css = "<style>" <> css <> "</style>"
        let output =
              unTemplate
              (Map.fromList
                [( "readme", T.unpack x)
                , ("style", styleEl $ mconcat
                    [ "body { background-color: #00B9DA; }"
                    , "pre { background-color: white; }"
                    , "code { background-color: white; }"
                    ]
                  )
                ]
              )
              "<html><head>{{::=style}}</head><body>{{::=readme}}</body></html>"
        writeText $ T.pack output
  BlogR :/ blogRoute -> case blogRoute of
    FirstProgram :/ () ->
      serve $(markdownRoute (BlogR :/ FirstProgram :/ ()) firstProgramMd)
    Wildcard :/ wcRoute -> case wcRoute of
      "FutureHiring" ->
        serve $(markdownRoute (wc "FutureHiring") futureHiring)
      _unmatched ->
        serve $(getPageCompiled (Blog :/ ()) $ blog) -- Go to blog Mainpage

  where
    serve :: MonadSnap m => FilePath -> m ()
    serve = void . serveCompressed

readIMMarkFile :: FilePath -> IO (Either String T.Text)
readIMMarkFile fp = do
  _contents <- liftIO $ readFile fp
--   _ <- error $ show $ Prelude.length _contents
  case MMark.parse fp $ T.pack _contents of
    Left (e) -> pure . Left $ MP.errorBundlePretty e
    Right bundle -> pure . Right $ LT.toStrict . renderText $ MMark.render $ useExtensions extensions bundle
