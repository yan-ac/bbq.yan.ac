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
import           Text.Blaze.Internal (customAttribute, customParent, stringValue)

import Data.RequestState
import Data.MaybeFail
import Data.BBQ
import Data.VCodePool
import Acid.BBQ
import Acid.VCodePool
import KeyHolder

import Crypto.BCrypt
import Text.Email.Validate
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe
import Data.ByteString.Base64 (encode, decode)

authenticate = do
  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
  msum [
      dir "logout" $ do
        nullDir
        method GET
        handleLogout
    , dir "login" $ do
        nullDir
        method POST
        handleLogin
    , dir "register" $ do
        nullDir
        method POST
        handleRegister
    , dir "finish-registration" $ do
        nullDir
        method GET
        handleFinishRegistration
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
    Fail _  -> forbidden $ toResponse $ template
                 "登录失败"
                 ( H.h1 $ do "密码不正确或者该邮箱尚未注册" )
    Success accountId -> do
      accessKey  <- liftIO $ getNextVCode
      expireTime <- liftIO $ expireIn (Second 900)
      update $ NewAccountVCode accountId accessKey expireTime
      addCookie Session (mkCookie "accountId" (show accountId))
      addCookie Session (mkCookie "accessKey" (unVCode accessKey))
      ok $ simpleResponse loginTpl "登录成功"

-- Logout Handler --
handleLogout = do
  plainTpl <- askPlainTemplate
  expireCookie "accountId"
  expireCookie "accessKey"
  ok $ simpleResponse plainTpl "登出成功"

-- Register Handler --
mkVerfLink :: String -> String -> String -> String
mkVerfLink email pwd vcode =
  "/finish-registration?email=" ++ email ++ "&password=" ++ pwd ++ "&vcode=" ++ vcode

handleRegister = do
  template <- askBasicTemplate
  email    <- body $ look "email"
  password <- body $ look "password"

  result <- runEitherT $ do
    thenThrowError (not . isValid . pack $ email) "错误的邮箱地址格式"
    thenThrowError ((length password) < 12 || (length password) > 24) "密码应当为 12—24 位"
    isRegistered <- lift $ query $ IsEmailRegisterd (Email email)
    thenThrowError isRegistered "该邮箱已被注册"

    bcryptedPwd <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy (pack password)
    let finalPwd = unpack . encode . fromJust $ bcryptedPwd    
    vcode       <- liftIO $ getNextVCode
    expireTime  <- liftIO $ expireIn (Second 900)
    lift $ update $ NewEmailVCode (Email email) vcode expireTime
    let url = mkVerfLink email finalPwd (unVCode vcode)
    right url

  case result of
    Left errMsg -> badRequest $ simpleResponse template errMsg
    Right url   -> ok $ toResponse $ template
      "校验邮箱"
      ( do
          H.h1 $ do "请登录邮箱以完成注册"
          H.p $ do
            "请点击"
            H.a ! A.href (stringValue url) $ do "链接"
            "以完成注册。"
      )

-- Finish Registration Handler --
handleFinishRegistration = do
  template <- askBasicTemplate
  email    <- queryString $ look "email"
  password <- queryString $ look "password"
  vcode    <- queryString $ look "vcode"
  now      <- liftIO $ getCurrentTimeInSecond
  veResult <- query $ VerifyEmailVCode (Email email) (VCode vcode) (ExpireTime now)

  result <- runEitherT $ do
    case veResult of
      Fail "EXPIRED RECORD" -> left "验证信息已过期"
      Fail _                -> left "验证失败"
      Success _             -> right ()

    let maybePwd = decode . pack $ password
    thenThrowError (isLeft maybePwd) "验证失败"
    let (Right packedPwd) = maybePwd
    let bcryptedPwd       = unpack packedPwd
    tryToCreateAccount   <- lift $ update $ NewAccount (Email email, Password bcryptedPwd)
    case tryToCreateAccount of
      Fail    _ -> left "该链接已注册，请直接登录您的账户"
      Success _ -> do
        lift $ update $ DeleteEmailVCode (Email email)
        right "注册成功"

  case result of
    Left errMsg -> forbidden $ simpleResponse template errMsg
    Right msg   -> ok $ simpleResponse template msg
