{-# LANGUAGE OverloadedStrings #-}

module Page.Index where

import Happstack.Server
import Control.Monad
import Control.Monad.Trans
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.BBQ
import Layout.Basic
import AcidProvider

indexPage :: Maybe AccountId -> App Response
indexPage authResult = do
  nullDir
  ok $ toResponse $ basic
    authResult
    "首页"
    (H.h1 $ do "第一届烧烤节语言学友谊赛")
