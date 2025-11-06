module Landing.Pages.Html.Blog where

--import Landing.Classh
import Classh
import Classh.Reflex

import Landing.Pages.Html.Head
import Landing.Pages.Html.Scripts
import Landing.Pages.Html.Elems
  ( commonShell,
  )
import Reflex.Dom.Core

blog :: DomBuilder t m => m ()
blog = do
  commonShell
    blogPageHead
    blogBody
    blogScript

blogBody :: DomBuilder t m => m ()
blogBody = do
  text "blog page!"
  gridCol Col12 $ do
    col [12,12,6] $ do
      $(textClassh' [text_color .~~ Black, text_size .|~ [LG, XL, XL2]]) "read about haskell!"
