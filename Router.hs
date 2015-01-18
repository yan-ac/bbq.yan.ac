{-# LANGUAGE OverloadedStrings #-}
module Router where

import Control.Applicative  (Applicative, Alternative)
import Control.Monad
import Control.Monad.Trans
import Happstack.Server
import Happstack.Server.RqData (getDataFn)
import Text.Blaze           ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import AcidProvider
import CheckUserAuth

import Page.Index
import Page.Register ( aboutRegister )
import Page.Login ( aboutLogin )
import Page.List
import Page.E404

route runApp' acid = do
  authResult <- checkUserAuth acid
  let runApp = runApp' acid
  msum [
      runApp (indexPage authResult)
    , aboutRegister authResult runApp
    , aboutLogin authResult runApp
    , dir "list"   $ runApp (listPage authResult)
    , dir "public" $ serveDirectory DisableBrowsing ["index.html"] "public"
    , runApp (e404Page authResult)
    ]
