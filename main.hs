{-# LANGUAGE OverloadedStrings #-}
module Main where

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

main :: IO ()
main =
  withAcid Nothing $ \acid ->
    simpleHTTP nullConf $ do
      msum [ 
          dir "hello"  $ runApp acid page
        , dir "public" $ serveDirectory DisableBrowsing ["index.html"] "public"
        , runApp acid e404Page
        ]

page :: App Response
page = do
  nullDir
  list <- query ListByEmail
  let result = show list
  ok $ toResponse $ H.toHtml result

e404Page :: App Response
e404Page = do
  ok $ toResponse $ basic "页面不存在" (H.h1 $ "页面不存在")
