
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}


module Landing.Pages.Html.Elems where

-- import Landing.Pages.Elems
--import Landing.Pages.Html.Scripts
import Common.Route
import Obelisk.Route
import Reflex.Dom.Core
import Data.Text as T

import System.IO.Unsafe

import Lamarckian.JS
import Classh as C

-- | idea: just carries name to function made for notifying system of event
-- buttonStaticFRP :: T.Text -> JSRef
-- button


-- | The key detail is that there's no real effect on page structure
-- | therefore it allows us to configure the site more easily
commonShell :: DomBuilder t m => m () -> m () -> Script -> m ()
commonShell headContent content (Script txt) = elAttr "html" ("lang" =: "en") $ do
  el "head" headContent
  elAttr "body" ("class" =: "bg-white") $ do
    content
    el "script" $ text $ T.pack txt
type Src = T.Text



-- Fixes
  -- Images

  -- Links
  -- Buttons
  -- Text
   -- size must be larger than 12px (@ 6% , need 60%)
zipSizes :: [a] -> WhenTW a
zipSizes = zipScreens

-- Note: this should only ever be used in a static dom builder Template Haskell context
has3 :: [JSFunc] -> (JSFunc,JSFunc,JSFunc)
has3 (x_:y_:z_:[]) = (x_,y_,z_)
has3 _ = do
  unsafePerformIO $ do
    appendFile "exception.txt" "EXCEPTION"
    error "incorrect number of dom setters"


-- -- | A dumb shell which does nothing but visually wrap a DomBuilder
-- -- | is for documentation purposes
type DomShell m a = m a -> m a

--renderRouteBE :: R (FullRoute BackendRoute FrontendRoute) -> T.Text
renderLandingR :: R MainLandingRoute -> T.Text
renderLandingR = renderRouteBE . ((:/) LandingR)

renderLandingR_ :: MainLandingRoute () -> T.Text
renderLandingR_ =
  renderRouteBE
  . (\route_ -> LandingR :/ route_ :/ ())


-- --renderRouteBE :: R (FullRoute BackendRoute FrontendRoute) -> T.Text
-- renderRouteBE :: R BackendRoute -> T.Text
-- renderRouteBE = Common.Elems.renderRouteBE

-- renderRouteFE :: R FrontendRoute -> T.Text
-- renderRouteFE = Common.Elems.renderRouteFE

-- simpleLinkBE :: DomBuilder t m => R BackendRoute -> m a -> m a
-- simpleLinkBE = Common.Elems.simpleLinkBE

-- simpleLinkFE :: DomBuilder t m => R FrontendRoute -> m a -> m a
-- simpleLinkFE = Common.Elems.simpleLinkFE



--------------------------------------------------------------------------------
-- Color Palette
--------------------------------------------------------------------------------

navy, navyDark, navyLight, gold, goldLight, ivory, slate :: Text
navy      = "0F1C2E"
navyDark  = "0a1421"
navyLight = "1a2a3f"
gold      = "D4A84B"
goldLight = "E8C876"
ivory     = "F8F5F0"
slate     = "64748B"
