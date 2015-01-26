{-# LANGUAGE OverloadedStrings #-}
module Page.Registration (entry) where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (stringValue)
import Crypto.BCrypt
import Text.Email.Validate

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either

import Happstack.Server

import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe
import Data.Char

import Data.RequestState
import Data.BBQ
import Data.VCodePool
import Acid.BBQ
import Acid.VCodePool
import KeyHolder
import Middleware.SendEmail

entry = do
  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
  msum [
      dir "register"  $ nullDir >> method  GET >> mkRegisterPage
    , dir "register"  $ nullDir >> method POST >> sendVerificationLink
    , dir "fill-info" $ nullDir >> method  GET >> mkFillInfoPage
    , dir "fill-info" $ nullDir >> method POST >> mkNewAccount
    ]

-- Helper Functions --
inRange lo hi val = val >= lo && val <= hi
isHanChar = inRange 0x4e00 0x9fff . ord

simpleResponse template msg = toResponse $ template msg ( H.h1 $ do H.toHtml msg)

vrEmail = isValid . pack
vrPassword = inRange 12 24 . length

elseThrowError cond err = if cond then right () else left err
extractEither e = case e of
  Left a  -> left a
  Right b -> right b
-- Make /Register Page --
dashboardURI :: String
dashboardURI = "/dashboard"

loginedMsg :: String
loginedMsg = "您已登陆"

mkRegisterPage = do
  template   <- askBasicTemplate
  authResult <- askAuthResult
  case authResult of
    Just _  -> seeOther dashboardURI (toResponse loginedMsg)
    Nothing -> ok $ toResponse $ template "注册" registerPage

-- Send Verification Link --
mkFillInfoLink email vcode = "/fill-info?email=" ++ email ++ "&vcode=" ++ vcode

sendVerificationLink = do
  template  <- askBasicTemplate
  email'    <- fetchEmail
  case email' of
    Left errMsg -> badRequest $ simpleResponse template errMsg
    Right email -> do
      vcode      <- liftIO $ getNextVCode
      expireTime <- liftIO $ expireIn (Second 3600)
      update $ InsertNewAccount (Email email) vcode expireTime
      let url = "https://bbq.yan.ac" ++ (mkFillInfoLink email (unVCode vcode))
      liftIO $ sendNotification email "BBQ.Yan.ac 账户注册"
        (  "有人使用该邮箱地址在 BBQ.Yan.ac 上注册账户。"
        ++ "如果这不是你本人的行为，请忽略此邮件。"
        ++ "否则，请点击链接完成你的注册：" ++ url
        )
      ok $ simpleResponse template "请登录邮箱以完成注册"


-- Make Fill Info Page --
mkFillInfoPage = do
  template   <- askBasicTemplate
  authResult <- fetchEmailVCode
  case authResult of
    Left errMsg          -> forbidden $ simpleResponse template errMsg
    Right (email, vcode) -> ok $ toResponse $ template "填写信息" (fillInfoPage email vcode)

-- Make New Account --
mkNewAccount = do
  template      <- askBasicTemplate
  authResult    <- fetchEmailVCode
  personalInfo' <- fetchPersonalInfoForm
  password'     <- fetchPassword

  result <- runEitherT $ do
    (email, _)   <- extractEither authResult
    personalInfo <- extractEither personalInfo'
    plainPwd     <- extractEither password'
    cryptedPwd   <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy (pack plainPwd)
    let finalPwd = unpack . fromJust $ cryptedPwd
    lift $ update $ NewAccount (Email email, Password finalPwd) personalInfo

  case result of
    Left errMsg -> badRequest $ simpleResponse template errMsg
    Right _     -> ok $ simpleResponse template "注册成功"

-- Verification --
fetchEmailVCode = do
  email      <- queryString $ look "email"
  vcode      <- queryString $ look "vcode"
  now        <- liftIO $ getCurrentTimeInSecond
  authResult <- query $ VerifyNewAccount (Email email) (VCode vcode) (ExpireTime now)
  case authResult of
    Left errMsg -> return (Left errMsg)
    Right _     -> return (Right (email, vcode))

fetchEmail = do
  email      <- body $ look "email"
  runEitherT $ do
    elseThrowError (vrEmail email) "错误的邮箱地址"
    isRegistered <- lift $ query $ IsEmailRegisterd (Email email)
    elseThrowError (not isRegistered) "该邮箱已被注册"
    right email

fetchPassword  = do
  password <- body $ look "password"
  if vrPassword password
    then return (Right password)
    else return (Left "密码应当为 12—24 位")

fetchPersonalInfoForm = do
  realname <- body $ look "realname"
  school   <- body $ look "school"
  grade    <- body $ look "grade"
  let test lo hi x = (all isHanChar x) && (inRange lo hi (length x))
  runEitherT $ do
    elseThrowError (test 2 6 realname) "姓名格式不正确"
    elseThrowError (test 4 20 school)  "校名格式不正确"
    elseThrowError (grade `elem` ["初一", "初二", "初三", "高一", "高二", "高三"]) "年级格式不正确"
    right (realname, school, grade)

-- Pages --
registerPage = do
  H.h1 "填写你的邮件地址"
  H.form ! A.action "/register"
         ! A.method "post" $ do
    H.label ! A.for "email" $ do "邮件地址"
    H.input ! A.type_ "email"
            ! A.name  "email"
    H.button ! A.type_ "submit"
             ! A.name "register" $ do "发送验证邮件"

fillInfoPage email vcode = do
  H.h1 "填写密码及个人信息"
  H.form ! A.action (stringValue $ mkFillInfoLink email vcode)
         ! A.method "post" $ do
    H.label ! A.for "password" $ do "密码（12—24 位）"
    H.input ! A.type_ "password"
            ! A.name  "password"
    H.label ! A.for "realname" $ do "真实姓名"
    H.input ! A.type_ "text"
            ! A.name "realname"
    H.label ! A.for "school" $ do "学校"
    H.input ! A.type_ "text"
            ! A.name "school"
    H.label ! A.for "grade" $ do "年级"
    H.input ! A.type_ "text"
            ! A.name "grade"
    H.button ! A.type_ "submit"
             ! A.name "register" $ do "完成注册"
