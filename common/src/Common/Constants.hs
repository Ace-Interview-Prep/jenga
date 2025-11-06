{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Common.Constants where

import Data.Text as T

authCookieName :: T.Text
authCookieName = "cookie_ip"

userTypeCookieName :: T.Text
userTypeCookieName = "utype"
