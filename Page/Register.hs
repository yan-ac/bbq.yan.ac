{-# LANGUAGE OverloadedStrings #-}
module Page.Register where

import Happstack.Server
import Control.Monad
import Control.Monad.Trans
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (customAttribute, customParent, stringValue)

import Layout.Basic
import Layout.Result
import AcidProvider
import Data.MaybeFail
import Data.BBQ
import Acid.BBQ
import Data.VCodePool
import Acid.VCodePool
import KeyHolder

import Crypto.BCrypt
import Text.Email.Validate
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe
import Data.ByteString.Base64 (encode, decode)

registerPage :: App Response
registerPage = ok $ toResponse $ basic
  "注册"
  ( do
      H.h1 $ do "注册友谊赛"
      H.form ! A.action "/register"
             ! A.method "post" $ do
        "邮箱"
        H.br
        H.input ! A.type_ "text"
                ! A.name "email"
        "密码（12—24 位）"
        H.br
        H.input ! A.type_ "password"
                ! A.name "password"
        H.button ! A.type_ "submit"
                 ! A.name "register" $ do "注册"
  )

registerHandler :: App Response
registerHandler = do
  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
  email    <- body $ look "email"
  password <- body $ look "password"
  if not (isValid (pack email))
    then badRequest $ toResponse $ Layout.Result.result "错误的邮箱地址格式"
    else if (length password) < 12 || (length password) > 24
      then badRequest $ toResponse $ Layout.Result.result "密码应当为 12—24 位"
      else do
        isRegistered <- query $ IsEmailRegisterd (Email email)
        bcryptedPwd <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy (pack password)
        let finalPwd = unpack . encode . fromJust $ bcryptedPwd
        case isRegistered of
          True  -> forbidden $ toResponse $ Layout.Result.result "该邮箱已被注册"
          False -> do
            vcode      <- liftIO $ getNextVCode
            expireTime <- liftIO $ expireIn (Second 900)
            update $ NewEmailVCode (Email email) vcode expireTime
            let url = "/finish-registration?email=" ++ email ++
                      "&password=" ++ finalPwd ++
                      "&vcode=" ++ (unVCode vcode)
            ok $ toResponse $ basic
              "校验邮箱"
              ( do
                  H.h1 $ do "请登录邮箱以完成注册"
                  H.p $ do
                    "请点击"
                    H.a ! A.href (stringValue url) $ do "链接"
                    "以完成注册。"
              )

finishRegistration :: App Response
finishRegistration = do
  email    <- queryString $ look "email"
  password <- queryString $ look "password"
  vcode    <- queryString $ look "vcode"
  now      <- liftIO $ getCurrentTimeInSecond
  result   <- query $ VerifyEmailVCode (Email email) (VCode vcode) (ExpireTime now)
  case result of
    Success _ -> do
      case decode . pack $ password of
        Left _          -> forbidden $ toResponse $ Layout.Result.result "错误的消息参数"
        Right packedPwd -> do
          let bcryptedPwd = unpack packedPwd
          creationResult <- update $ NewAccount (Email email, Password bcryptedPwd)
          case creationResult of
            Fail _    -> forbidden $ toResponse $ Layout.Result.result "注册失败，内部错误"
            Success _ -> do
              update $ DeleteEmailVCode (Email email)
              ok $ toResponse $ Layout.Result.result "注册成功"
    Fail "EXPIRED RECORD" -> unauthorized $ toResponse $ Layout.Result.result "验证信息已过期"
    Fail _                -> unauthorized $ toResponse $ Layout.Result.result "验证失败"

aboutRegister runApp = do
  msum [
      dir "register" $ do
        nullDir
        method GET
        runApp registerPage
    , dir "register" $ do
        nullDir
        method POST
        runApp registerHandler
    , dir "finish-registration" $ runApp finishRegistration
    ]
