{-# LANGUAGE OverloadedStrings #-}
module Layout.Result where

import Control.Monad
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H

import Layout.Basic

result :: String -> H.Html
result resultMsg = basic
  resultMsg
  ( H.h1 $ do H.toHtml resultMsg )
