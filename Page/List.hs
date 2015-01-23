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

showDatabase :: Handler Response
showDatabase = msum [
    dir "0.6077480115927756OR0.8599326119292527" $ do
      list <- query ListByEmail
      let result = show list
      ok $ toResponse $ H.toHtml result
  , dir "0.4999181595630944OR0.0781512644607573" $ do
      pool <- query GetPools
      let result = show pool
      ok $ toResponse $ H.toHtml result
  ]
