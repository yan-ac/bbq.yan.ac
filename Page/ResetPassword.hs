{-# LANGUAGE OverloadedStrings #-}
module Page.ResetPassword (entry) where

import Happstack.Server
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either (runEitherT)
import Data.Either

import Data.RequestState
import Data.BBQ
import Data.VCodePool
import Acid.BBQ
import Acid.VCodePool
import Middleware.SendEmail
import Middleware.KeyHolder
import Layout.Basic
import Layout.ValidatableForm
import Data.ValidatableForm

import Utils

import Crypto.BCrypt
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe

entry = do
  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
  msum [
      dir "forget-password" $ nullDir >> method  GET >> mkForgetPasswordPage
    , dir "forget-password" $ nullDir >> method POST >> handleForgetPassword
    , dir "reset-password"  $ nullDir >> method  GET >> mkResetPasswordPage
    , dir "reset-password"  $ nullDir >> method POST >> handleResetPassword
    ]

-- Forget Password Handler --
mkForgetPasswordPage = do
  template  <- loadTmplWithAuth formTemplate
  ok $ toResponse $ template "忘记密码" "/forget-password" $ getFormItems safeForgetPasswordForm

handleForgetPassword = do
  template  <- loadTmplWithAuth basicTemplate
  postData  <- runForm safeForgetPasswordForm

  result <- runEitherT $ do
    email     <- extractEither postData
    isExisted <- lift $ query $ GetAccountId email
    extractEither isExisted
    vcode      <- liftIO $ getNextVCode
    expireTime <- liftIO $ expireIn (Second 3600)
    lift $ update $ InsertResetPasswd email vcode expireTime
    liftIO $ sendNotification email "BBQ.Yan.ac 密码重置"
      (  "有人试图在 BBQ.Yan.ac 上重置该账户的密码。"
      ++ "如果这不是你本人的行为，请忽略此邮件。"
      ++ "否则，请点击链接完成密码重置："
      ++ mkVerificationLink True "/reset-password" email vcode
      )

  case result of
    Left errMsg -> forbidden $ simpleResponse template errMsg
    Right _     -> ok $ simpleResponse template "请登录邮箱以完成密码重置"

mkResetPasswordPage = do
  basicTmpl  <- loadTmplWithAuth basicTemplate
  formTmpl   <- loadTmplWithAuth formTemplate
  authResult <- fetchEmailVCode VerifyResetPasswd
  case authResult of
    Left errMsg          -> forbidden $ simpleResponse basicTmpl errMsg
    Right (email, vcode) -> ok $ toResponse $ formTmpl
      "设置你的新密码"
      (mkVerificationLink False "/reset-password" email vcode)
      (getFormItems safeResetPasswordForm)

handleResetPassword = do
  template   <- loadTmplWithAuth basicTemplate
  postData   <- runForm safeResetPasswordForm
  authResult <- fetchEmailVCode VerifyResetPasswd

  result <- runEitherT $ do
    Password password <- extractEither postData
    (email, _)  <- extractEither authResult
    bcryptedPwd <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy (pack password)
    let finalPwd = unpack . fromJust $ bcryptedPwd
    lift $ update $ ResetPassword (email, Password finalPwd)
    lift $ update $ DeleteResetPasswdRecord email

  case result of
    Left errMsg -> forbidden $ simpleResponse template errMsg
    Right _     -> ok $ simpleResponse template "密码重置成功"

-- Pages --
safeForgetPasswordForm :: (Monad m, TypesafeForm m) => m Email
safeForgetPasswordForm = do
  email <- askEmail "email" "填写你的邮件地址"
  addButton "submit" "重置密码"
  return email

safeResetPasswordForm :: (Monad m, TypesafeForm m) => m Password
safeResetPasswordForm = do
  password  <- askPassword "password" "输入你的新密码（12—24 位，任意字符均可）"
  repeat    <- askPassword "repeat"   "重复输入你的新密码"
  addButton "submit" "重置密码"
  should (password == repeat) (Just "两次密码输入不一致")
  return password
