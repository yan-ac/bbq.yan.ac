{-# LANGUAGE OverloadedStrings #-}
module Layout.Basic (mkBasicTemplate, basicTemplate) where

import Control.Monad
import Data.RequestState

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (customAttribute, customParent, stringValue)

import Layout.Ming

mkBasicTemplate :: (Maybe a) -> String -> H.Html -> H.Html
mkBasicTemplate authResult title body =
  case authResult of
    Nothing -> ming [] [] False title body
    Just _  -> ming [] [] True title body

basicTemplate :: Bool -> String -> H.Html -> H.Html
basicTemplate auth title body = ming [] [] auth title body
