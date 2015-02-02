{-# LANGUAGE OverloadedStrings #-}
module Page.Registration (entry) where

import Crypto.BCrypt

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either

import Happstack.Server

import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe

import Data.RequestState
import Data.ValidatableForm
import Data.BBQ
import Data.VCodePool
import Acid.BBQ
import Acid.VCodePool
import Middleware.KeyHolder
import Middleware.SendEmail

import Utils
import Layout.Basic
import Layout.ValidatableForm

entry = do
  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
  msum [
      dir "register"  $ nullDir >> method  GET >> mkRegisterPage
    , dir "register"  $ nullDir >> method POST >> sendVerificationLink
    , dir "fill-info" $ nullDir >> method  GET >> mkFillInfoPage
    , dir "fill-info" $ nullDir >> method POST >> mkNewAccount
    ]

-- Make /Register Page --
mkRegisterPage = do
  template   <- loadTmplWithAuth formTemplate
  authResult <- askAuthResult
  case authResult of
    Just _  -> seeOther dashboardURI (toResponse loginedMsg)
    Nothing -> ok $ toResponse $ template "注册友谊赛" "/register" $ getFormItems safeRegisterForm

sendVerificationLink = do
  template <- loadTmplWithAuth basicTemplate
  postData <- runForm safeRegisterForm

  result <- runEitherT $ do
    email        <- extractEither postData
    isRegistered <- lift $ query $ IsEmailRegisterd email
    thenThrowError isRegistered "该邮箱已被注册"

    vcode      <- liftIO $ getNextVCode
    expireTime <- liftIO $ expireIn (Second 3600)
    lift $ update $ InsertNewAccount email vcode expireTime
    liftIO $ sendNotification email "BBQ.Yan.ac 账户注册"
      (  "有人使用该邮箱地址在 BBQ.Yan.ac 上注册账户。"
      ++ "如果这不是你本人的行为，请忽略此邮件。"
      ++ "否则，请点击链接完成你的注册："
      ++ mkVerificationLink True "/fill-info" email vcode
      )

  case result of
    Left errMsg -> badRequest $ simpleResponse template errMsg
    Right _     -> ok $ simpleResponse template "请登录邮箱以完成注册"

-- Make Fill Info Page --
mkFillInfoPage = do
  basicTmpl  <- loadTmplWithAuth basicTemplate
  formTmpl   <- loadTmplWithAuth formTemplate
  authResult <- fetchEmailVCode VerifyNewAccount
  case authResult of
    Left errMsg          -> forbidden $ simpleResponse basicTmpl errMsg
    Right (email, vcode) -> ok $ toResponse $ formTmpl
      "填写密码及个人信息"
      (mkVerificationLink False "/fill-info" email vcode)
      (getFormItems safeFillInfoForm)

-- Make New Account --
mkNewAccount = do
  template   <- loadTmplWithAuth basicTemplate
  authResult <- fetchEmailVCode VerifyNewAccount
  postData   <- runForm safeFillInfoForm

  result <- runEitherT $ do
    (email, _)  <- extractEither authResult
    (pwd, info) <- extractEither postData
    cryptedPwd  <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy (pack . unPassword $ pwd)
    let finalPwd = Password $ unpack . fromJust $ cryptedPwd
    lift $ update $ NewAccount (email, finalPwd) info

  case result of
    Left errMsg -> badRequest $ simpleResponse template errMsg
    Right _     -> ok $ simpleResponse template "注册成功"


-- Pages --
safeRegisterForm :: (Monad m, TypesafeForm m) => m Email
safeRegisterForm = do
  email     <- askEmail "email" "填写你的邮件地址"
  addButton "register" "发送验证邮件"
  return email

safeFillInfoForm :: (Monad m, TypesafeForm m) => m (Password, (String, String, String))
safeFillInfoForm = do
  password  <- askPassword "password" "输入你的密码（12—24 位，任意字符均可）"
  repeat    <- askPassword "repeat"   "重复输入你的密码"
  realName  <- askText     "realname" "输入你的真实姓名，全汉字，不超过六个汉字"
  school    <- askText     "school"   "输入你的学校的中文全称，全汉字，不超过二十个汉字"
  grade     <- askChoice   "grade"    "选择你的年级" [(x, x) | x <- ["六年级", "初一", "初二", "初三", "高一", "高二", "高三"]]
  addButton "submit" "完成注册"
  should (password == repeat) (Just "两次密码输入不一致")
  let test lo hi x = all isHanChar x && inRange lo hi (length x)
  should (test 2 6  realName) (Just "姓名格式不正确")
  should (test 4 20 school)   (Just "校名格式不正确")
  return (password, (realName, school, grade))
