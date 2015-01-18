{-# LANGUAGE OverloadedStrings #-}
module Router where

import Control.Applicative  (Applicative, Alternative, (<$>))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad
import Happstack.Server 
import Text.Blaze           ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.BBQ
import Acid.BBQ
import Layout.Basic
import AcidProvider

import Page.Index
import Page.Register ( aboutRegister )
import Page.E404

route :: (App Response -> ServerPartT IO Response) -> ServerPartT IO Response
route runApp = do
  msum [
      runApp indexPage
    , aboutRegister runApp
    , dir "public" $ serveDirectory DisableBrowsing ["index.html"] "public"
    , runApp e404Page
    ]

page :: App Response
page = do
  nullDir
  list <- query ListByEmail
  let result = show list
  ok $ toResponse $ H.toHtml result
