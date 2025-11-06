{-# LANGUAGE OverloadedStrings #-}
module Landing.Utils where

-- probably mainly JS stuff
--import Landing.Pages.
import Landing.Static
import Common.Route
import Obelisk.Route.Frontend
import Language.Haskell.TH (Q, Exp)
import qualified Data.Text as T

-- runEmailWidgetWithRefs :: (HTemplateRefs -> StaticWidget' x a) -> IO BS.ByteString
-- runEmailWidgetWithRefs =
--   fmap snd . renderStatic . flip runRouteToUrlT renderRouteForEmail . runSetRouteT


laurenLink :: T.Text
laurenLink = "https://meetings.hubspot.com/lauren974?uuid=5a7f3042-7c6f-4822-9b3a-b9dc41f441ac"

laurenLinkDevs :: T.Text
laurenLinkDevs = "https://share.hsforms.com/1rzTZxmpSQSiM-cuwE92I8Qsmgty"

-- | Find a more logical module for these
staticVideo, staticImg :: FilePath -> Q Exp
staticImg img = static $ "images/" <> img
staticVideo video = static $ "videos/" <> video

renderRouteForEmail :: R FrontendRoute -> T.Text
renderRouteForEmail = ("" <>) . renderFrontendRoute checkedFullRouteEncoder

mdLink :: BlogRoute () -> T.Text
mdLink r = renderBackendRoute checkedFullRouteEncoder (LandingR :/ BlogR :/ r :/ ())

renderLocalLink :: BlogRoute () -> T.Text -> T.Text
renderLocalLink r id_ =
  renderBackendRoute checkedFullRouteEncoder
  (LandingR :/ BlogR :/ r :/ ())
  <>
  "#"
  <>
  id_

wc :: T.Text -> R MainLandingRoute
wc x = BlogR :/ Wildcard :/ x
