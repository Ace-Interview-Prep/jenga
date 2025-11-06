{-# Language ScopedTypeVariables #-}
{-# Language ConstraintKinds #-}
module Common.Route where

import Database.Beam.Schema
import Control.Category as Cat
import Control.Lens (iso, Iso', coerced)
import Control.Monad.Except
import Data.Coerce
import Data.Functor.Identity
import Data.Int
import Database.Beam.Backend.SQL.Types
import Data.Text (Text)
import Data.Signed (Signed(..))
import Obelisk.Route
import Obelisk.Route.TH
import Prelude hiding ((.), id)
import Rhyolite.Account (PasswordResetToken)

renderRouteBE :: R BackendRoute -> Text
renderRouteBE = renderBackendRoute checkedFullRouteEncoder

renderRouteFE :: R FrontendRoute -> Text
renderRouteFE = renderFrontendRoute checkedFullRouteEncoder

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_Login :: FrontendRoute ()
  FrontendRoute_ResetPassword :: FrontendRoute (Signed PasswordResetToken)
  FrontendRoute_RequestNewPassword :: FrontendRoute ()
  FrontendRoute_Signup :: FrontendRoute ()
--Signed PasswordResetToken)
data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Listen :: BackendRoute ()
  BackendRoute_Missing :: BackendRoute ()
  LandingBase :: BackendRoute ()
  LandingR :: BackendRoute (R MainLandingRoute)
  Api_Login  :: BackendRoute ()
  Api_ResetPassword  :: BackendRoute ()

-- | We have other landing pages, so this is our main one
data MainLandingRoute :: * -> * where
  AboutUs :: MainLandingRoute ()
  Blog :: MainLandingRoute ()
  BlogR :: MainLandingRoute (R BlogRoute)

data BlogRoute :: * -> * where
  Wildcard :: BlogRoute Text
  FirstProgram :: BlogRoute ()

requestPasswordRoute :: Text
requestPasswordRoute = renderFrontendRoute checkedFullRouteEncoder $
  FrontendRoute_RequestNewPassword :/ ()

checkedEncoder :: Applicative check => Encoder check Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedEncoder = either (error "checkEncoder failed") id $ checkEncoder fullRouteEncoder

plainR
  :: ( MonadError Text parse
     , Applicative check
     )
  => Text -> SegmentResult check parse ()
plainR path = PathSegment path $ unitEncoder mempty

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  -- When unrecognized route, send to landing page
  (FullRoute_Backend LandingBase :/ ())
  (\case
      Api_Login -> plainR "authenticate"
      Api_ResetPassword -> plainR "resetApi"
      LandingBase -> PathEnd $ unitEncoder mempty
      BackendRoute_Missing -> PathSegment "land" $ unitEncoder mempty
      BackendRoute_Listen -> PathSegment "listen" $ unitEncoder mempty
      LandingR -> PathSegment "landing" $ pathComponentEncoder $ \case
        AboutUs -> plainR "aboutMyApp"
        Blog -> plainR "blog"
        BlogR -> PathSegment "Blog" $ pathComponentEncoder $ \case
          FirstProgram -> plainR "FirstProgram"
          Wildcard -> PathSegment "post" singlePathSegmentEncoder
  )
  (\case
      FrontendRoute_Main -> plainR "main"
      FrontendRoute_Login -> plainR "login"
      FrontendRoute_ResetPassword -> PathSegment "reset" $ signedEncoder
      FrontendRoute_RequestNewPassword -> plainR "recover"
      FrontendRoute_Signup -> plainR "signup"
  )

checkedFullRouteEncoder :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedFullRouteEncoder = case checkEncoder fullRouteEncoder of
  Left e -> error $ show e
  Right x -> x

idEncoder :: forall check parse x.
  (MonadError Text parse, Applicative check, Coercible (PrimaryKey x Identity) (SqlSerial Int64))
  => Encoder check parse (PrimaryKey x Identity) Text
idEncoder = unsafeTshowEncoder
  . viewEncoder (coerced :: Iso' (PrimaryKey x Identity) Int64)

signedEncoder :: (MonadError Text parse, Applicative check) => Encoder check parse (Signed a) PageName
signedEncoder = singlePathSegmentEncoder . viewEncoder (iso unSigned Signed)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''MainLandingRoute
  , ''BlogRoute
  ]
