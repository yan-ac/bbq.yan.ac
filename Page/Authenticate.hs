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
import Middleware.KeyHolder
import Middleware.SendEmail
import Layout.Basic

import Crypto.BCrypt
import Text.Email.Validate
import Data.ByteString.Char8 (pack, unpack)
import Data.Maybe
import Data.ByteString.Base64 (encode, decode)

authenticate = do
  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
  msum [
      dir "login"  $ nullDir >> method  GET >> mkLoginPage
    , dir "login"  $ nullDir >> method POST >> handleLogin
    , dir "logout" $ nullDir >> method  GET >> handleLogout
    ]

simpleResponse template msg = toResponse $ template msg ( H.h1 $ do H.toHtml msg)

thenThrowError cond err = if cond then left err else right ()

-- Login Handler --
handleLogin = do
  loginedTpl <- loadTmplAsLogined basicTemplate
  plainTpl   <- loadTmplAsPlain   basicTemplate

  email    <- body $ look "email"
  password <- body $ look "password"
  result   <- query $ Authenticate (Email email, Password password)
  case result of
    Left errMsg     -> forbidden $ toResponse $ plainTpl "登录失败" ( H.h1 $ do H.toHtml errMsg )
    Right accountId -> do
      accessKey  <- liftIO $ getNextVCode
      expireTime <- liftIO $ expireIn (Second 900)
      update $ InsertCookie accountId accessKey expireTime
      addCookie Session (mkCookie "accountId" (show accountId))
      addCookie Session (mkCookie "accessKey" (unVCode accessKey))
      ok $ simpleResponse loginedTpl "登录成功"

-- Logout Handler --
handleLogout = do
  plainTpl <- loadTmplAsPlain basicTemplate
  expireCookie "accountId"
  expireCookie "accessKey"
  ok $ simpleResponse plainTpl "登出成功"
