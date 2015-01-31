{-# LANGUAGE OverloadedStrings #-}
module Page.List where

import Happstack.Server
import Control.Monad
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (customAttribute, customParent)

import Data.RequestState
import Data.BBQ
import Acid.BBQ
import Acid.VCodePool
import qualified Config

showDatabase :: Handler Response
showDatabase = msum [
    dir "accounts" $ dir Config.superUserPassword $ do
      list <- query ListByEmail
      let result = show list
      ok $ toResponse $ H.toHtml result
  , dir "vcodes"   $ dir Config.superUserPassword $ do
      pool <- query GetPools
      let result = show pool
      ok $ toResponse $ H.toHtml result
  ]
