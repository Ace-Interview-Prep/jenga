module Landing.Playground (editorCanvas) where

-- this module specifically exports nothing besides editorCanvas
-- this module is a page to mess with code in local development
-- TODO: route should be __host__/a/c/e?is=great

import Landing.Pages.ContactUsForm


editorCanvas :: DomBuilder t m => Shell m a
editorCanvas dom = do 
  placeCenterWidth [("def", pct 50)] $ do
    el "x" $ do
      elClass "div" "bg-gray-200" $ do
        dom




