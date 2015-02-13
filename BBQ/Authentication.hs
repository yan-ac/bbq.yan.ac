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
import BBQ.SendEmail
import BBQ.Common

import Data.ValidatableForm
import Data.Accounts
import Data.RecordPool
import Data.AppConfig

import qualified Data.Char
import Crypto.BCrypt

-- /auth --
safeLoginForm :: (Monad m, TypesafeForm m) => m (Email, Password)
safeLoginForm = do
  email    <- askEmail "email" "注册邮箱"
  password <- askPassword "password" "账户密码"
  addButton "login" "登录"
  return (email, password)

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
      return id

  case result of
    Left errMsg -> unauthorized $ toJSResponse $ JSOrderError errMsg
    Right (AccountId id) -> do
      homeURL <- showURL Home
      ok $ toJSResponse $ JSOrderRedirect $ homeURL
