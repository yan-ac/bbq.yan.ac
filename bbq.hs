{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad           (msum)
import Happstack.Server
import Web.Routes.Happstack    (implSite)

import BBQ.Route

main :: IO ()
main = simpleHTTP nullConf $ msum
  [ dir "images" $ serveDirectory DisableBrowsing [] "images"
  , dirs "static/css" $ serveDirectory DisableBrowsing [] "views/css"
  , dirs "static/js"  $ serveDirectory DisableBrowsing [] "views/js"
  , implSite "http://localhost:8000" "" site
  , notFound $ toResponse ("resource not found" :: String)
  ]
