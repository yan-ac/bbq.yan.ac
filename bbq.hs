{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad           (msum)
import Happstack.Server
import Web.Routes.Happstack    (implSite)

import BBQ.Route
import Data.Accounts
import Data.RecordPool
import Data.Sheets
import Data.AppConfig


withAcid path action =
  openAccountsState path $ \st1 ->
  openRecordPools   path $ \st2 ->
  openSheetsState   path $ \st3 ->
    action $ AppConfig st1 st2 st3

main :: IO ()
main = do
  putStrLn "BBQ is listening on 8000."
  withAcid  "_state" $ \acid -> simpleHTTP nullConf $ runApp acid server

server :: App Response
server = msum
  [ dir "images" $ serveDirectory DisableBrowsing [] "images"
  , dirs "static/css" $ serveDirectory DisableBrowsing [] "views/css"
  , dirs "static/js"  $ serveDirectory DisableBrowsing [] "views/js"
  , implSite "https://bbq.yan.ac" "" site
  , notFound $ toResponse ("resource not found" :: String)
  ]
