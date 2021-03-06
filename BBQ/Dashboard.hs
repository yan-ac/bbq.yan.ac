{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

module BBQ.Dashboard where

import Happstack.Server
import Web.Routes              (RouteT, showURL)
import Control.Monad.Trans.Either
import Control.Monad.Trans     (MonadIO(..), lift)
import Text.Hamlet

import BBQ.JSOrder
import BBQ.Sitemap
import BBQ.Common
import BBQ.Problems

import Data.ValidatableForm
import Data.Accounts
import Data.RecordPool
import Data.Sheets
import Data.AppConfig

notStartPage :: RouteT Sitemap App Response
notStartPage = do
  routeFn <- askRouteFn'
  let page = $(hamletFile "views/hamlets/not-start-bbq.hamlet")
  lift $ ok $ toResponse $ siteLayout "个人中心 | 言韵" page routeFn

finishedPage :: RouteT Sitemap App Response
finishedPage = do
  routeFn <- askRouteFn'
  let page = $(hamletFile "views/hamlets/finished-bbq.hamlet")
  lift $ ok $ toResponse $ siteLayout "个人中心 | 言韵" page routeFn

inProgressPage :: RouteT Sitemap App Response
inProgressPage = do
  routeFn <- askRouteFn'
  let page = $(hamletFile "views/hamlets/in-progress-bbq.hamlet")
  lift $ ok $ toResponse $ siteLayout' "个人中心 | 言韵" page [] ["/static/js/upload.js"] routeFn

startBBQ :: AccountId -> RouteT Sitemap App Response
startBBQ id = do
  now <- lift $ expireIn' 7200
  lift $ update $ StartBBQ id now
  dashboardURL <- showURL Dashboard
  lift $ ok $ toJSResponse $ JSOrderRedirect dashboardURL
