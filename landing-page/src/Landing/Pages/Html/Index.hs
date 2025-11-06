module Landing.Pages.Html.Index where

import Classh

import Common.Route
import Obelisk.Route
import Landing.Pages.Html.Elems
import Landing.Pages.Html.Head
import Landing.Pages.Html.Scripts
import Reflex.Dom.Core

indexHs :: DomBuilder t m => m ()
indexHs = do
  commonShell
    indexPageHead
    mkSectionElement
    indexScript

mkSectionElement :: DomBuilder t m => m ()
mkSectionElement = do
  elClass "div" $(classh' [bgColor .~~ hex "50d71e"]) $ do
    el "p" $ text "hello"
    el "p" $ text "hey"
    el "p" $ text "hi"
  elAttr "a" ("href" =: (renderRouteBE (LandingR :/ Blog :/ ()))) $ do
    elClass "div" "" $ do
      el "p" $ text "hello"
      el "p" $ text "hey"
      el "p" $ text "hi"
