module Main where

import Happstack.Server 
import AcidProvider
import Router

main :: IO ()
main =
  withAcid Nothing $ \acid ->
    simpleHTTP nullConf $ route runApp acid
