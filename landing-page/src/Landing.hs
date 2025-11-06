{-# LANGUAGE QuasiQuotes #-}
module Landing where

import Landing.Impl
import Landing.Utils
import Landing.Markdown
import Landing.Pages.Markdown.W1
import Landing.Pages.Markdown.FirstProgram
import Landing.Pages.Html.About
import Landing.Pages.Html.Blog
import Landing.Pages.Html.Index

import Lamarckian.Snap

import Common.Route
import Obelisk.Route

import Snap
import Control.Monad (void)

-- | Route Values/Literals still need to operate independently of each other


data ReadMode = Local | Prod

-- | This is on a seperate route tree so we need a second config
getMainLandingPage :: MonadSnap m => m ()
getMainLandingPage = void . serveCompressed $
  $(getMainPageCompiled (LandingBase :/ ()) $ indexHs)

-- | Idea, $(getPagesCompiled :: [page] -> [filepath]) -- or better data structure, then you can pattern match on them
-- | and then we would have more power over certain things
selectLandingPage :: MonadSnap m => R MainLandingRoute -> m ()
selectLandingPage r = serve $ case r of
  Blog :/ () ->
    $(getPageCompiled (Blog :/ ()) $ blog)
  AboutUs :/ () ->
    $(getPageCompiled (AboutUs :/ ()) aboutPage)
  BlogR :/ blogRoute -> case blogRoute of
    FirstProgram :/ () ->
      $(markdownRoute (BlogR :/ FirstProgram :/ ()) firstProgramMd)
    Wildcard :/ wcRoute -> case wcRoute of
      "FutureHiring" ->
        $(markdownRoute (wc "FutureHiring") futureHiring)
      _unmatched ->
        $(getPageCompiled (Blog :/ ()) $ blog) -- Go to blog Mainpage

  where
    serve :: MonadSnap m => FilePath -> m ()
    serve = void . serveCompressed
