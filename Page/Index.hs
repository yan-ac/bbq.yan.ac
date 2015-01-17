{-# LANGUAGE OverloadedStrings #-}

module Page.Index where

import Happstack.Server
import Control.Monad
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Layout.Basic
import AcidProvider

indexPage :: App Response
indexPage = do
  nullDir
  ok $ toResponse $ basic
    "首页"
    (H.h1 $ do "第一届烧烤节语言学友谊赛")
