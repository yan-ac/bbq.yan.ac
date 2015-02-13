{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

module BBQ.Registration where

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

-- /register/new --
safeRegisterForm :: (Monad m, TypesafeForm m) => m Email
safeRegisterForm = do
  email     <- askEmail "email" "填写你的邮件地址"
  addButton "submit" "获取验证邮件"
  return email

newRegistration :: RouteT Sitemap App Response
newRegistration = do
  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
  postData <- runForm safeRegisterForm

  result <- lift $ runEitherT $ do
    email <- extractEither postData
    check <- lift $ query $ CheckEmailAddress email
    extractEither check
    vcode <- lift $ askNewRecord $ NewAccountEmail email
    right (email, vcode)

  case result of
    Left errMsg -> forbidden $ toJSResponse $ JSOrderError errMsg
    Right (email, vcode) -> do
      let p1 = "有人使用该邮箱地址在 BBQ.Yan.ac 上注册账户。如果这不是你本人的行为，请忽略此邮件。否则，请点击链接完成你的注册："
      p2 <- showURL $ Registration vcode
      liftIO $ sendNotification email "BBQ.Yan.ac 账户注册" (p1 <> p2)
      ok $ toJSResponse $ JSOrderOK "请登录邮箱查收验证邮件"

-- register/{vcode} --
safeFillInfoForm :: (Monad m, TypesafeForm m) => m (Password, (String, String, String))
safeFillInfoForm = do
  let inRange lo hi val = val >= lo && val <= hi
  let isHanChar = inRange 0x4e00 0x9fff . Data.Char.ord
  password  <- askPassword "password" "输入你的密码（12—24 位，任意字符均可）"
  repeat    <- askPassword "repeat"   "重复输入你的密码"
  realName  <- askText     "realname" "输入你的真实姓名，全汉字，不超过六个汉字"
  school    <- askText     "school"   "输入你的学校的中文全称，全汉字，不超过二十个汉字"
  grade     <- askChoice   "grade"    "选择你的年级" [(x, x) | x <- ["小学生", "初一", "初二", "初三", "高一", "高二", "高三", "其它"]]
  addButton "submit" "完成注册"
  should (password == repeat) (Just "两次密码输入不一致")
  let test lo hi x = all isHanChar x && inRange lo hi (length x)
  should (test 2 6  realName) (Just "姓名格式不正确")
  should (test 4 20 school)   (Just "校名格式不正确")
  return (password, (realName, school, grade))

registrationGET :: VCode -> RouteT Sitemap App Response
registrationGET vcode = do
  email'  <- lift $ queryRecord NewAccountPool vcode
  routeFn <- askRouteFn'
  case email' of
    Nothing    -> forbidden $ toResponse ("无效的验证信息，输入错误或者已过期" :: String)
    Just email -> do
      let formItems = getFormItems safeFillInfoForm
      ok $ toResponse $ siteLayout "完善个人信息 | 言韵" ([hamlet|
        <h1>完善个人信息</h1>
        <form id="register-form" class="js-valform" action=@{Registration vcode} method="post">
          ^{formItems}
        |]) routeFn

registrationPOST :: VCode -> RouteT Sitemap App Response
registrationPOST vcode = do
  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
  postData <- runForm safeFillInfoForm

  result <- lift $ runEitherT $ do
    (Password plainPwd, info) <- extractEither postData
    (Just cryptedPwd) <- liftIO $ hashPasswordUsingPolicy fastBcryptHashingPolicy plainPwd
    let pswd = Password cryptedPwd

    email' <- lift $ queryRecord NewAccountPool vcode
    case email' of
      Nothing -> left ("无效的验证信息，输入错误或者已过期" :: String)
      Just (NewAccountEmail email) -> do
        lift $ deleteRecord NewAccountPool vcode
        (Right id')    <- lift $ update $ InsertNewAccount email pswd info
        let (AccountId id) = id'
        (VCode cookie) <- lift $ askNewRecord $ CookieAccountId id'
        lift $ addCookie Session (mkCookie "accountId" (show id))
        lift $ addCookie Session (mkCookie "accessKey" cookie)

  case result of
    Left errMsg -> forbidden $ toJSResponse $ JSOrderError errMsg
    Right _     -> do
      homeURL <- showURL Home
      ok $ toJSResponse $ JSOrderRedirect $ homeURL
