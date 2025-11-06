{-# Language ConstraintKinds #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.Request where

import Jenga.Common.Errors
import Jenga.Common.Auth

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH

import Common.Types
--import Text.Email.Validate

-- | 'PrivateRequest's are those which require the user to be logged in.
data PrivateRequest a where
  PrivateRequest_Names
    :: PrivateRequest (Either (BackendError NameError) (Email))

instance ToJSON NameError
instance FromJSON NameError

type AuthPayload = (AuthToken, UserType)

-- Public requests are those which do not require the user to already be logged in.
data PublicRequest a where
  --TODO(galen) Use AuthCreds type
  -- PublicRequest_Login :: Text -> Text -> PublicRequest (Either (BackendError LoginError) AuthPayload)
  -- PublicRequest_ResetPassword
  --   :: Signed PasswordResetToken
  --   -> Text
  --   -> PublicRequest (Either (BackendError ResetPasswordError) AuthToken)
  PublicRequest_RequestResetPassword
    :: Email
    -> PublicRequest (Either (BackendError RequestPasswordResetError) ())
  PublicRequest_Signup :: Email -> PublicRequest (Either (BackendError UserSignupError) ())

deriveArgDict ''PrivateRequest
deriveJSONGADT ''PrivateRequest
deriveArgDict ''PublicRequest
deriveJSONGADT ''PublicRequest
