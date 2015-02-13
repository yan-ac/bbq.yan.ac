{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

module BBQ.Problems where

import Happstack.Server
import Web.Routes              (RouteT, showURL)
import Control.Monad.Trans.Either
import Control.Monad.Trans     (MonadIO(..), lift)
import Text.Hamlet

import BBQ.JSOrder
import BBQ.Sitemap
import BBQ.Common

import Data.ValidatableForm
import Data.Accounts
import Data.RecordPool
import Data.Sheets
import Data.AppConfig

problemPage :: ProblemId -> RouteT Sitemap App Response
problemPage (ProblemId id) = do
  let e1 = $(hamletFile "views/hamlets/problems/easy-1.hamlet")
  let e2 = $(hamletFile "views/hamlets/problems/easy-2.hamlet")
  let n1 = $(hamletFile "views/hamlets/problems/normal-1.hamlet")
  let n2 = $(hamletFile "views/hamlets/problems/normal-2.hamlet")
  let h1 = $(hamletFile "views/hamlets/problems/hard-1.hamlet")
  let h2 = $(hamletFile "views/hamlets/problems/hard-2.hamlet")
  let b1 = $(hamletFile "views/hamlets/problems/bonus-1.hamlet")
  let b2 = $(hamletFile "views/hamlets/problems/bonus-2.hamlet")
  let b3 = $(hamletFile "views/hamlets/problems/bonus-3.hamlet")

  let p = case id of
            1 -> e1
            2 -> e2
            3 -> n1
            4 -> n2
            5 -> h1
            6 -> h2
            7 -> b1
            8 -> b2
            9 -> b3

  routeFn <- askRouteFn'
  if and [id /= 1, id /= 9]
  then ok $ toResponse $ siteLayout' "言韵·友谊赛" p [] [] routeFn
  else case id of
        1 -> serveFile (asContentType "application/pdf") "images/easy-1.pdf"
        9 -> serveFile (asContentType "application/pdf") "images/bonus-3.pdf"
