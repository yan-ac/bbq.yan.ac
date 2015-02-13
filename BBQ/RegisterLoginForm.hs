{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

module BBQ.RegisterLoginForm where

import Data.Accounts
import Data.ValidatableForm

import Text.Hamlet
import BBQ.Sitemap

safeRegisterForm :: (Monad m, TypesafeForm m) => m Email
safeRegisterForm = do
  email     <- askEmail "email" "填写你的邮件地址"
  addButton "submit" "获取验证邮件"
  return email

safeLoginForm :: (Monad m, TypesafeForm m) => m (Email, Password)
safeLoginForm = do
  email    <- askEmail "email" "注册邮箱"
  password <- askPassword "password" "账户密码"
  addButton "submit" "登录"
  return (email, password)

registerHamlet :: HtmlUrl Sitemap
registerHamlet = [hamlet|
  <form id=register-form class=js-valform action=@{NewRegistration} method=post>
    ^{formItems}
  |]
  where formItems = getFormItems safeRegisterForm

loginHamlet :: HtmlUrl Sitemap
loginHamlet = [hamlet|
  <form id=login-form class=js-valform action=@{Authentication} method=post>
    ^{formItems}
  |]
  where formItems = getFormItems safeLoginForm
