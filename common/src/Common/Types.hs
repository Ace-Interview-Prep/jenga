{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.Types where

--experimental
--import Data.String (IsString)

-- import Database.Beam
-- import Data.Aeson.TH
-- #ifndef ghcjs_HOST_OS
-- import Data.Text.Normalize
-- #endif
-- import Text.Email.Validate
import Jenga.Common.Errors
import qualified Data.Text as T
import GHC.Generics
-- import Data.Aeson
-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Base64 as B64
-- import qualified Data.ByteString.Lazy as LBS
-- import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Text.Email.Validate
import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE

instance ToJSON BS.ByteString where
  toJSON = toJSON . TE.decodeUtf8

instance FromJSON BS.ByteString where
  parseJSON = withText "ByteString" (pure . TE.encodeUtf8)

-- -- TODO: consolidate, I know that we have this in backend

-- instance ToJSON ByteString where
--     toJSON = toJSON . decodeUtf8 . B64.encode

-- instance FromJSON ByteString where
--     parseJSON o = either fail return . B64.decode . encodeUtf8 =<< parseJSON o

-- instance ToJSON LBS.ByteString where
--     toJSON = toJSON . decodeUtf8 . B64.encode . LBS.toStrict

-- instance FromJSON LBS.ByteString where
--     parseJSON o = either fail (return . LBS.fromStrict) . B64.decode . encodeUtf8 =<< parseJSON o

-- instance ToJSONKey ByteString
-- instance FromJSONKey ByteString

-- -- Move to Auth lib
-- data UserType = Self | Admin deriving (Eq, Show, Read, Generic)
-- newtype AcctID = AcctID { getAcctID :: Int } deriving (Eq, Show, Generic)
-- instance ToJSON AcctID
-- instance FromJSON AcctID
data NameError = NotFound | NameError T.Text deriving (Show, Eq, Generic)
-- data SignupError
--   = FailedMakeResetToken
--   | AccountExists
--   | FailedSendEmail
--   deriving (Eq,Show,Generic)
-- instance ToJSON SignupError
-- instance FromJSON SignupError
-- instance ShowUser SignupError where
--   showUser FailedMakeResetToken = "Couldn't create one-time token for account setup"
--   showUser AccountExists = "Account already exists"
--   showUser FailedSendEmail = "Could not send email with setup link"
-- data ResetPasswordError
--   = InvalidToken
--   | Unknown T.Text
--   | CouldntRetrieveAccount
--   | NoEmailSent
--   | Reset_NoUserTypeFound
--   deriving (Eq,Show,Generic)
-- instance FromJSON ResetPasswordError
-- instance ToJSON ResetPasswordError
-- instance ShowUser ResetPasswordError where
--   showUser InvalidToken = "Invalid token, this link has probably expired, please request a new one"
--   showUser (Unknown err) =  "Unknown Error on reset password" <> err
--   showUser CouldntRetrieveAccount = "Could not find account when trying to reset password, this error has been reported"
--   showUser Reset_NoUserTypeFound = "No user type found"
--   showUser NoEmailSent = "Unable to send email"

instance ToJSON EmailAddress
instance FromJSON EmailAddress
-- data RequestPasswordResetError
--   = InvalidEmail
--   | NotSignedUp
--   | FailedMakeNonce
--   deriving (Eq,Show,Generic)
-- instance FromJSON RequestPasswordResetError
-- instance ToJSON RequestPasswordResetError
-- instance ShowUser RequestPasswordResetError where
--   showUser InvalidEmail = "Invalid Email"
--   showUser NotSignedUp = "An account with that email does not exist, please signup first"
--   showUser FailedMakeNonce = "Failed to make one-time link"

-- data LoginError
--   = UnrecognizedEmail T.Text
--   | IncorrectPassword
--   | NoUserTypeFound
--   | DomainNotSet
--   | ExpiredSubscription T.Text
--   | ExpiredFreeTrial T.Text
--   deriving (Eq,Show,Generic)
-- instance FromJSON LoginError
-- instance ToJSON LoginError
-- instance ShowUser LoginError where
--   showUser (UnrecognizedEmail _) = "An account with this email doesn't exist"
--   showUser IncorrectPassword =  "Incorrect password, please try again"
--   showUser NoUserTypeFound =  "No UserType found"
--   showUser (ExpiredSubscription subscribeURL) =  "Your subscription is no longer active. Please visit " <> subscribeURL
--   showUser (ExpiredFreeTrial userEmail) = "The free trial for " <> userEmail <> " is expired"
--   showUser DomainNotSet = "Admin never set a valid URI"
-- --- Move to auth lib

-- --- Move to Markdown Lib
-- data MarkdownError
--   = MarkdownError T.Text
--   deriving (Eq,Show,Generic)
-- instance FromJSON MarkdownError
-- instance ToJSON MarkdownError
-- instance ShowUser MarkdownError where
--   showUser (MarkdownError t) = t
-- ----

-- -- | Move to lib-thunk
-- data LogItem
--   = SomeLog T.Text
--   | SomeError T.Text
--   --- | UserRequestedLesson (Maybe Email) LessonId
--   | LandingPageRequested T.Text
--   -- TODO: if we add more cases, or remove cases, we should
--   -- either turn this to a PgEnum wrapped value in the table
--   -- or make a custom instance of FromField
--   deriving (Show,Eq,Ord,Generic)

-- -- instance Enum LogItem where
-- --   toEnum S

-- instance ToJSON LogItem
-- instance FromJSON LogItem
-- ----------



-- --- Various backend errors
-- class SpecificError a

-- instance (TypeError ('Text "Instances of IsString are not allowed, please use a handler-specific enum")) => SpecificError (BackendError String)
-- instance (TypeError ('Text "Instances of IsString are not allowed, please use a handler-specific enum")) => SpecificError (BackendError T.Text)
-- instance SpecificError (BackendError LoginError)
-- instance SpecificError (BackendError ResetPasswordError)
-- instance SpecificError (BackendError RequestPasswordResetError)
-- instance SpecificError (BackendError SignupError)
instance SpecificError (BackendError NameError)
--   -- noString _ = error "unreachable"
-- -- | Route-Empty error
-- instance SpecificError (BackendError ())
-- instance ShowUser a => ShowUser (BackendError a) where
--   showUser NoAuth = "No Auth Token found"
--   showUser (BUserError e) = showUser e
--   showUser (BCritical e) = "Something has gone wrong with your request and has been automatically reported, we apologize for the inconvenience ... ERROR=" <> showUser e
--   showUser (BException e) = "Something has gone wrong with your request and has been automatically reported, we apologize for the inconvenience ... ERROR=" <> e

-- -- type ErrorRead = String
-- -- type ApiResponse a = Either ErrorRead (Either ApiError a)

-- data FrontendError
--   = forall err. ErrorRequirements err => Req (RequestError err)
--   | UserError T.Text
--   | JSError T.Text
--   | Unexpected T.Text

-- type ErrorRequirements err = (Show err, ShowUser err, ToJSON err, FromJSON err, Typeable err)
-- req_ :: RequestError () -> FrontendError
-- req_ = Req

-- data RequestError e
--   = Request_ErrorRead T.Text
--   -- ^ Invalid JSON or different XhrResponseBody than expected
--   | Request_ErrorAPI (BackendError e)

--   | Request_ErrorTimeout T.Text
--   deriving (Show, Eq)

-- -- data FrontendError
-- --   = Req RequestError
-- --   | UserError T.Text
-- --   | JSError T.Text
-- --   | Unexpected T.Text
-- --   deriving (Show, Eq)

-- -- data RequestError
-- --   = Request_ErrorRead T.Text
-- --   | Request_ErrorAPI T.Text
-- --   | Request_ErrorTimeout T.Text
-- --   deriving (Show, Eq)

-- -- data BackendError e = BackendError e
-- -- instance Show e => Show (BackendError e) where

-- data BackendError e
--   = NoAuth
--   | BUserError e
--   | BCritical e
--   | BException T.Text
--   deriving (Eq, Generic)

-- instance ToJSON e => ToJSON (BackendError e)
-- instance FromJSON e => FromJSON (BackendError e)

-- instance Functor BackendError where
--   fmap _ NoAuth = NoAuth
--   fmap f (BUserError e) = BUserError (f e)
--   fmap f (BCritical e) = BCritical (f e)
--   fmap _ (BException s) = BException s

-- deriving instance Show e => Show (BackendError e)

-- class ShowUser a where
--   showUser :: a -> T.Text

-- instance ShowUser () where
--   showUser _ = "Empty Error"

-- instance ShowUser e => ShowUser (RequestError e) where
--   showUser (Request_ErrorRead msg) = msg
--   showUser (Request_ErrorAPI msg) = showUser msg
--   showUser (Request_ErrorTimeout msg) = msg

-- instance ShowUser FrontendError where
--   showUser = \case
--     Req r -> showUser r
--     UserError msg -> msg
--     Unexpected msg -> msg
--     JSError msg -> msg

-- -- | Used for password reset
-- newtype Email = Email { unEmail :: T.Text } deriving (Show, Eq, Ord, Generic)

-- data Password = Password { unPassword :: T.Text } deriving Generic

-- toText :: Password -> T.Text
-- toText = unPassword

-- fromText :: T.Text -> Password
-- fromText = Password
-- #ifndef ghcjs_HOST_OS
--   . normalize NFKC
-- #endif

-- instance ToJSON Password where
--   toJSON = toJSON . toText
-- instance FromJSON Password where
--   parseJSON = fmap fromText . parseJSON

-- instance FromJSON EmailAddress
-- instance ToJSON EmailAddress

-- deriveJSON defaultOptions ''Email
-- deriveToJSON defaultOptions ''FrontendError
-- deriveJSON defaultOptions ''RequestError
