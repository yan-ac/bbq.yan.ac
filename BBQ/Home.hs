{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module BBQ.Home (page) where

import Data.Text        (Text)
import Text.Hamlet
import Happstack.Server (Response, ServerPartT, ok, toResponse)
import Web.Routes       (RouteT)
import Web.Routes.Happstack ()

import BBQ.Sitemap
import BBQ.RegisterLoginForm
import BBQ.Problems

import Data.Accounts
import Data.RecordPool
import Data.AppConfig

page :: RouteT Sitemap App Response
page = do
  routeFn <- askRouteFn'
  let introBBQ = $(hamletFile "views/hamlets/home/intro-bbq.hamlet")
  let introOL  = $(hamletFile "views/hamlets/home/intro-ol.hamlet")
  let rules    = $(hamletFile "views/hamlets/home/rules.hamlet")

  let registerLogin = $(hamletFile "views/hamlets/home/register-or-login.hamlet")
  ok $ toResponse $ siteLayout' "言韵·友谊赛" ([hamlet|
    ^{introBBQ}
    <div class="home-article-container">
      ^{introOL}
    <div class="home-article-container">
      ^{rules}
    ^{registerLogin}
    |]) ["/static/css/homepage.css"] [] routeFn
