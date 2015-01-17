{-# LANGUAGE OverloadedStrings #-}
module Page.E404 where

import Happstack.Server
import Control.Monad
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (customAttribute, customParent)

import Layout.Basic
import AcidProvider

e404Page :: App Response
e404Page = notFound $ toResponse $ basic
  "页面不存在"
  (H.h1 $ do "您所请求的页面不存在")
