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
import Data.ValidatableForm
import Data.BBQ
import Data.VCodePool
import Acid.BBQ
import Acid.VCodePool
import Middleware.KeyHolder
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

thenThrowError cond err = if cond then left err else right ()
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
mkFillInfoLink (Email email) (VCode vcode) = "/fill-info?email=" ++ email ++ "&vcode=" ++ vcode

sendVerificationLink = do
  template <- askBasicTemplate
  postData <- getValidatorFromValidatableForm mkSafeRegisterForm

  result <- runEitherT $ do
    email        <- extractEither postData
    isRegistered <- lift $ query $ IsEmailRegisterd email
    thenThrowError isRegistered "该邮箱已被注册"

    vcode      <- liftIO $ getNextVCode
    expireTime <- liftIO $ expireIn (Second 3600)
    lift $ update $ InsertNewAccount email vcode expireTime
    let url = "https://bbq.yan.ac" ++ (mkFillInfoLink email vcode)
    liftIO $ sendNotification email "BBQ.Yan.ac 账户注册"
      (  "有人使用该邮箱地址在 BBQ.Yan.ac 上注册账户。"
      ++ "如果这不是你本人的行为，请忽略此邮件。"
      ++ "否则，请点击链接完成你的注册：" ++ url
      )

  case result of
    Left errMsg -> badRequest $ simpleResponse template errMsg
    Right _     -> ok $ simpleResponse template "请登录邮箱以完成注册"

-- Make Fill Info Page --
mkFillInfoPage = do
  template   <- askBasicTemplate
  authResult <- fetchEmailVCode
  case authResult of
    Left errMsg          -> forbidden $ simpleResponse template errMsg
    Right (email, vcode) -> ok $ toResponse $ template "填写信息" (fillInfoPage email vcode)

-- Make New Account --
mkNewAccount = do
  template   <- askBasicTemplate
  authResult <- fetchEmailVCode
  postData   <- getValidatorFromValidatableForm mkSafeFillInfoForm

  result <- runEitherT $ do
    (email, _)  <- extractEither authResult
    (pwd, info) <- extractEither postData
    cryptedPwd  <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy (pack . unPassword $ pwd)
    let finalPwd = Password $ unpack . fromJust $ cryptedPwd
    lift $ update $ NewAccount (email, finalPwd) info

  case result of
    Left errMsg -> badRequest $ simpleResponse template errMsg
    Right _     -> ok $ simpleResponse template "注册成功"

-- Verification --
fetchEmailVCode = do
  email'     <- queryString $ look "email"
  vcode'     <- queryString $ look "vcode"
  let email  = Email email'
  let vcode  = VCode vcode'
  now        <- liftIO $ getCurrentTimeInSecond
  authResult <- query $ VerifyNewAccount email vcode (ExpireTime now)
  case authResult of
    Left errMsg -> return (Left errMsg)
    Right _     -> return (Right (email, vcode))

-- Pages --
mkSafeRegisterForm :: (Monad m, TypesafeForm m) => m Email
mkSafeRegisterForm = do
  email     <- askEmail "email" "填写你的邮件地址"
  addButton "register" "发送验证邮件"
  return email

registerPage = do
  H.h1 "注册友谊赛"
  getHtmlFromValidatableForm "/register" mkSafeRegisterForm

mkSafeFillInfoForm :: (Monad m, TypesafeForm m) => m (Password, (String, String, String))
mkSafeFillInfoForm = do
  password  <- askPassword "password" "输入你的密码（12—24 位，任意字符均可）"
  repeat    <- askPassword "repeat"   "重复输入你的密码"
  realName  <- askText     "realname" "输入你的真实姓名，全汉字，不超过六个汉字"
  school    <- askText     "school"   "输入你的学校的中文全称，全汉字，不超过二十个汉字"
  grade     <- askChoice   "grade"    "选择你的年级" [(x, x) | x <- ["初一", "初二", "初三", "高一", "高二", "高三"]]
  addButton "submit" "完成注册"
  should (password == repeat) (Just "两次密码输入不一致")
  let test lo hi x = all isHanChar x && inRange lo hi (length x)
  should (test 2 6  realName) (Just "姓名格式不正确")
  should (test 4 20 school)   (Just "校名格式不正确")
  return (password, (realName, school, grade))

fillInfoPage email vcode = do
  H.h1 "填写密码及个人信息"
  getHtmlFromValidatableForm (stringValue $ mkFillInfoLink email vcode) mkSafeFillInfoForm
