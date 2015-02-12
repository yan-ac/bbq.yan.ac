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

page :: RouteT Sitemap (ServerPartT IO) Response
page = do
  routeFn <- askRouteFn'
  let umbuUngu = $(hamletFile "views/hamlets/problems/umbu-ungu.hamlet")

  let introBBQ = $(hamletFile "views/hamlets/home/intro-bbq.hamlet")
  let introOL  = $(hamletFile "views/hamlets/home/intro-ol.hamlet")
  let rules    = $(hamletFile "views/hamlets/home/rules.hamlet")
  let aboutUs  = $(hamletFile "views/hamlets/home/about-us.hamlet")

  let registerLogin = $(hamletFile "views/hamlets/home/register-or-login.hamlet")
  ok $ toResponse $ siteLayout' "言韵·友谊赛" ([hamlet|
    ^{introBBQ}
    ^{introOL}
    ^{rules}
    ^{aboutUs}
    ^{registerLogin}
    |]) ["/static/css/homepage.css"] [] routeFn
