{-# LANGUAGE OverloadedStrings #-}
module Page.Authenticate (Page.Authenticate.authenticate) where

import Happstack.Server
import Happstack.Server.RqData (getDataFn)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Either
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (stringValue)

import Data.RequestState
import Data.BBQ
import Data.VCodePool
import Acid.BBQ
import Acid.VCodePool
import Middleware.KeyHolder
import Middleware.SendEmail

import Crypto.BCrypt
import Text.Email.Validate
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe
import Data.ByteString.Base64 (encode, decode)

authenticate = do
  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
  msum [
      dir "logout"          $ nullDir >> method  GET >> handleLogout
    , dir "login"           $ nullDir >> method POST >> handleLogin
    , dir "forget-password" $ nullDir >> method POST >> handleForgetPassword
    , dir "reset-password"  $ nullDir >> method  GET >> newResetPasswordPage
    , dir "reset-password"  $ nullDir >> method POST >> handleResetPassword
    ]

simpleResponse template msg = toResponse $ template msg ( H.h1 $ do H.toHtml msg)

thenThrowError cond err = if cond then left err else right ()

-- Login Handler --
handleLogin = do
  loginTpl <- askLoginTemplate
  template <- askBasicTemplate
  email    <- body $ look "email"
  password <- body $ look "password"
  result   <- query $ Authenticate (Email email, Password password)
  case result of
    Left errMsg     -> forbidden $ toResponse $ template "登录失败" ( H.h1 $ do H.toHtml errMsg )
    Right accountId -> do
      accessKey  <- liftIO $ getNextVCode
      expireTime <- liftIO $ expireIn (Second 900)
      update $ InsertCookie accountId accessKey expireTime
      addCookie Session (mkCookie "accountId" (show accountId))
      addCookie Session (mkCookie "accessKey" (unVCode accessKey))
      ok $ simpleResponse loginTpl "登录成功"

-- Logout Handler --
handleLogout = do
  plainTpl <- askPlainTemplate
  expireCookie "accountId"
  expireCookie "accessKey"
  ok $ simpleResponse plainTpl "登出成功"

-- Forget Password Handler --
handleForgetPassword = do
  template  <- askBasicTemplate
  email     <- body $ look "email"
  isExisted <- query $ GetAccountId (Email email)

  case isExisted of
    Left _  -> badRequest $ simpleResponse template "用户不存在"
    Right _ -> do
      vcode      <- liftIO $ getNextVCode
      expireTime <- liftIO $ expireIn (Second 3600)
      update     $ InsertResetPasswd (Email email) vcode expireTime
      liftIO $ sendNotification (Email email) "BBQ.Yan.ac 密码重置"
        (  "有人试图在 BBQ.Yan.ac 上重置该账户的密码。"
        ++ "如果这不是你本人的行为，请忽略此邮件。"
        ++ "否则，请点击链接完成密码重置："
        ++ "https://bbq.yan.ac/reset-password?email=" ++ email ++ "&vcode=" ++ (unVCode vcode)
        )
      ok $ simpleResponse template "请登录邮箱以完成密码重置"

-- New Reset Password Page --
newResetPasswordPage = do
  template <- askBasicTemplate
  email    <- queryString $ look "email"
  vcode    <- queryString $ look "vcode"
  now      <- liftIO $ getCurrentTimeInSecond
  let actionUrl = "/reset-password?email=" ++ email ++ "&vcode=" ++ vcode

  result <- query $ VerifyResetPasswd (Email email) (VCode vcode) (ExpireTime now)
  case result of
    Left errMsg -> badRequest $ simpleResponse template errMsg
    Right _     -> ok $ toResponse $ template
      "设置你的新密码"
      ( do
          H.h1 $ do "设置你的新密码"
          H.form ! A.action (stringValue actionUrl)
                 ! A.method "post" $ do
            "新密码"
            H.br
            H.input ! A.type_ "password"
                    ! A.name "password"
            H.button ! A.type_ "submit"
                     ! A.name "reset" $ do "重置密码"
      )

-- Reset Password Handler --
handleResetPassword = do
  template <- askBasicTemplate
  email    <- queryString $ look "email"
  vcode    <- queryString $ look "vcode"
  now      <- liftIO $ getCurrentTimeInSecond
  password <- body $ look "password"

  result <- runEitherT $ do
    lift $ query $ VerifyResetPasswd (Email email) (VCode vcode) (ExpireTime now)
    thenThrowError ((length password) < 12 || (length password) > 24) "密码应当为 12—24 位"
    bcryptedPwd <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy (pack password)
    let finalPwd = unpack . fromJust $ bcryptedPwd
    lift $ update $ ResetPassword (Email email, Password finalPwd)
    lift $ update $ DeleteResetPasswdRecord (Email email)

  case result of
    Left errMsg -> forbidden $ simpleResponse template errMsg
    Right _     -> ok $ simpleResponse template "密码重置成功"
