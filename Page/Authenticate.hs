{-# LANGUAGE OverloadedStrings #-}
module Page.Authenticate (Page.Authenticate.authenticate) where

import Happstack.Server
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
import Layout.Basic
import Layout.ValidatableForm
import Data.ValidatableForm
import Utils

authenticate :: Handler Response
authenticate = do
  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
  msum [
      dir "login"  $ nullDir >> method  GET >> mkLoginPage
    , dir "login"  $ nullDir >> method POST >> handleLogin
    , dir "logout" $ nullDir >> method  GET >> handleLogout
    ]

-- Login Handler --
mkLoginPage = do
  template   <- loadTmplWithAuth formTemplate'
  authResult <- askAuthResult
  case authResult of
    Just _  -> seeOther dashboardURI (toResponse loginedMsg)
    Nothing -> ok $ toResponse $ template "登录" "/login" html
  where html = do {
    sequence_ . formItemsToHtml $ getFormItems safeLoginForm;
    H.a ! A.href "/forget-password" 
        ! A.style "float: right" $ do "忘记密码？";
  }

handleLogin = do
  loginedTpl <- loadTmplAsLogined basicTemplate
  plainTpl   <- loadTmplAsPlain   basicTemplate
  postData   <- runForm safeLoginForm

  result <- runEitherT $ do
    (email, password) <- extractEither postData
    authResult <- lift $ query $ Authenticate (email, password)
    accountId  <- extractEither authResult
    accessKey  <- liftIO $ getNextVCode
    expireTime <- liftIO $ expireIn (Second 900)
    lift $ update $ InsertCookie accountId accessKey expireTime
    lift $ addCookie Session (mkCookie "accountId" (show accountId))
    lift $ addCookie Session (mkCookie "accessKey" (unVCode accessKey))

  case result of
    Left errMsg -> forbidden $ toResponse $ plainTpl "登录失败" ( H.h1 $ do H.toHtml errMsg )
    Right _     -> ok $ simpleResponse loginedTpl "登录成功"

safeLoginForm :: (Monad m, TypesafeForm m) => m (Email, Password)
safeLoginForm = do
  email    <- askEmail "email" "邮件地址"
  password <- askPassword "password" "密码"
  addButton "login" "登录"
  return (email, password)

-- Logout Handler --
handleLogout = do
  plainTpl <- loadTmplAsPlain basicTemplate
  expireCookie "accountId"
  expireCookie "accessKey"
  ok $ simpleResponse plainTpl "登出成功"
