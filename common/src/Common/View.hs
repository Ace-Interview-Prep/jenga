{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# Language ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Common.View where

import Common.Schema
import Data.Aeson.GADT.TH
import Data.Constraint.Extras.TH
import Data.Vessel
import Data.GADT.Show.TH
import Data.GADT.Compare.TH
import Data.Signed

import Rhyolite.Account

import Prelude hiding ((.), id)
import Data.Vessel.Void
import Rhyolite.Vessel.App
import Common.Request

data PrivateVK a where
  Private_Example :: PrivateVK (IdentityV ())
  -- Authenticated queries here

deriving instance Show (PrivateVK a)

data PersonalVK a where
  Personal_Example :: PersonalVK (IdentityV ())
  -- Authenticated + User-Based queries here

deriving instance Show (PersonalVK a)

type PrivateChatV = Vessel PrivateVK
type PersonalChatV = Vessel PersonalVK
type PublicChatV = VoidV

type ChatAuthCredential = Signed (PrimaryKey Account Identity)

data JengaApp

instance RhyoliteAuthApp JengaApp where
  type AuthCredential JengaApp = Signed (Id Account)

  type PublicApi JengaApp = PublicRequest
  type PrivateApi JengaApp = PrivateRequest

  type PrivateV JengaApp = PrivateChatV
  type PersonalV JengaApp = PersonalChatV
  type PublicV JengaApp = PublicChatV

concat <$> sequence
  [ deriveArgDict ''PrivateVK
  , deriveJSONGADT ''PrivateVK
  , deriveGEq ''PrivateVK
  , deriveGShow ''PrivateVK
  , deriveGCompare ''PrivateVK
  , deriveArgDict ''PersonalVK
  , deriveJSONGADT ''PersonalVK
  , deriveGEq ''PersonalVK
  , deriveGShow ''PersonalVK
  , deriveGCompare ''PersonalVK
  ]
