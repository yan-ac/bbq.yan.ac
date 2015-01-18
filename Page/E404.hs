{-# LANGUAGE OverloadedStrings #-}
module Page.E404 where

import Happstack.Server
import Control.Monad
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (customAttribute, customParent)

import Data.BBQ
import Layout.Basic
import AcidProvider

e404Page :: Maybe AccountId -> App Response
e404Page authResult = notFound $ toResponse $ basic
  authResult
  "页面不存在"
  (H.h1 $ do "您所请求的页面不存在")
