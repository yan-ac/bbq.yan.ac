{-# LANGUAGE OverloadedStrings #-}
module Page.List where

import Happstack.Server
import Control.Monad
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (customAttribute, customParent)

import Layout.Basic
import AcidProvider
import Data.BBQ
import Acid.BBQ
import Acid.VCodePool

listPage :: App Response
listPage = do 
  msum [
         dir "accounts" $ do
           list <- query ListByEmail
           let result = show list
           ok $ toResponse $ H.toHtml result
       , dir "vcode"     $ do
           pool <- query GetPool
           let result = show pool
           ok $ toResponse $ H.toHtml result
       ]
