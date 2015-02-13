{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

module BBQ.Authentication where

import Happstack.Server
import Web.Routes              (RouteT, showURL)
import Control.Monad.Trans.Either
import Control.Monad.Trans     (MonadIO(..), lift)
import Text.Hamlet

import BBQ.JSOrder
import BBQ.Sitemap
import BBQ.RegisterLoginForm
import BBQ.SendEmail
import BBQ.Common

import Data.ValidatableForm
import Data.Accounts
import Data.RecordPool
import Data.AppConfig

import qualified Data.Char
import Crypto.BCrypt

-- /auth --
auth :: RouteT Sitemap App Response
auth = do
  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
  postData <- runForm safeLoginForm

  result <- lift $ runEitherT $ do
    (email, pswd) <- extractEither postData
    passed <- lift $ query $ Authenticate email pswd
    if not passed
    then left "邮箱或密码错误"
    else do
      (Right id) <- lift $ query $ GetAccountId email
      right id

  case result of
    Left errMsg -> unauthorized $ toJSResponse $ JSOrderError errMsg
    Right id    -> do
      lift $ setAuthCookie id
      dashboardURL <- showURL Dashboard
      ok $ toJSResponse $ JSOrderRedirect $ dashboardURL

loginPage :: RouteT Sitemap App Response
loginPage = do
  routeFn <- askRouteFn'
  ok $ toResponse $ siteLayout "登录 | 言韵" ([hamlet|
  <h1>登录</h1>
  <p>由于长时间没有活动，你需要重新登录。这是出于安全考虑，对此造成的不便，我们表示抱歉。</p>
  <form id="login-form" class="js-valform" action=@{Authentication} method="post">
    ^{formItems}
  |]) routeFn
  where formItems = getFormItems safeLoginForm
