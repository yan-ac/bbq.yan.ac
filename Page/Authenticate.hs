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
import KeyHolder
import Middleware.SendEmail

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
    lift $ update $ InsertNewAccount (Email email) vcode expireTime
    let url = mkVerfLink email finalPwd (unVCode vcode)
    right url

  case result of
    Left errMsg -> badRequest $ simpleResponse template errMsg
    Right url   -> do
      liftIO $ sendNotification email "BBQ.Yan.ac 账户注册"
        ( "有人使用该邮箱地址在 BBQ.Yan.ac 上注册账户。" ++
          "如果这不是你本人的行为，请忽略此邮件。" ++
          "否则，请点击链接完成你的注册：" ++ url
        )

      ok $ simpleResponse template "请登录邮箱以完成注册"

-- Finish Registration Handler --
handleFinishRegistration = do
  template <- askBasicTemplate
  email    <- queryString $ look "email"
  password <- queryString $ look "password"
  vcode    <- queryString $ look "vcode"
  now      <- liftIO $ getCurrentTimeInSecond

  result <- runEitherT $ do
    lift $ query $ VerifyNewAccount (Email email) (VCode vcode) (ExpireTime now)
    let maybePwd = decode . pack $ password
    thenThrowError (isLeft maybePwd) "无效的验证信息"
    let (Right packedPwd) = maybePwd
    let bcryptedPwd       = unpack packedPwd
    lift $ update $ NewAccount (Email email, Password bcryptedPwd)
    lift $ update $ DeleteNewAccountRecord (Email email)
    right "注册成功"

  case result of
    Left errMsg -> forbidden $ simpleResponse template errMsg
    Right msg   -> ok $ simpleResponse template msg
