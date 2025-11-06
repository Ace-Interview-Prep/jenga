module Landing.Pages.Html.About (aboutPage) where

import Landing.Pages.Html.Scripts
import Landing.Pages.Html.Elems
import Landing.Pages.Html.Head
import Reflex.Dom.Core



aboutPage :: DomBuilder t m => m ()
aboutPage =  do
  commonShell
    aboutPageHead
    (pure ())
    aboutUsScript
