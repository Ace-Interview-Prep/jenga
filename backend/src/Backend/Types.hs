{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module Backend.Types where

import Data.Pool (Pool)
import Database.Beam.Postgres (Connection)

import Rhyolite.Account (Account)
import Jenga.Common.Schema
import Common.Schema
import Data.Constraint.Extras.TH
import Rhyolite.DB.NotifyListen.Beam
import Data.Aeson.GADT.TH






data Notify a where
  Notify_Account :: Notify (Id Account)
  Notify_SendEmail :: Notify (Id SendEmailTask)
deriveArgDict ''Notify
deriveJSONGADT ''Notify

instance HasNotification Notify Account where notification _ = Notify_Account

instance HasNotification Notify SendEmailTask where
  notification _ = Notify_SendEmail

microsecondsInSecond :: Num a => a
microsecondsInSecond = 1000000

type ApiE a = Either String a

type ConnectionPool = Pool Connection
