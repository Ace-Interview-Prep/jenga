{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# Language DeriveAnyClass #-}


module Common.Schema (module Common.Schema, module X) where

import Jenga.Common.Schema
import Common.SchemaHelpers as X

import Rhyolite.Account
import Database.Beam

data Db f = D
  { -- Auth Table
    _db_accounts :: f (TableEntity Account)
    -- Emailing Tasks
  , _db_sendEmailTask :: f (TableEntity SendEmailTask)
  , _db_reporting :: f (TableEntity LogItemRow)
  , _db_validOrgEmails :: f (TableEntity OrganizationEmails)
  , _db_userType :: f (TableEntity UserTypeTable)
  } deriving (Generic)

instance Database be Db
