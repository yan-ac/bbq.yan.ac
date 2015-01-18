{-# LANGUAGE OverloadedStrings #-}
module Page.Login where

import Happstack.Server
import Control.Monad
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (customAttribute, customParent)

import Layout.Basic
import Layout.Result
import AcidProvider
import Data.BBQ
import Data.MaybeFail
import Acid.BBQ

loginPage :: App Response
loginPage = ok $ toResponse $ basic
  "登录"
  ( do
      H.h1 $ do "登录你的账户"
      H.form ! A.action "/login"
             ! A.method "post" $ do
        "邮箱"
        H.br
        H.input ! A.type_ "text"
                ! A.name "email"
        "密码"
        H.br
        H.input ! A.type_ "password"
                ! A.name "password"
        H.button ! A.type_ "submit"
                 ! A.name "login" $ do "登录"
  )

loginHandler :: App Response
loginHandler = do
  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
  email    <- body $ look "email"
  password <- body $ look "password"
  result   <- query $ Authenticate (Email email, Password password)
  case result of
    Fail _    -> forbidden $ toResponse $ basic
                   "登录失败"
                   ( H.h1 $ do "密码不正确或者该邮箱尚未注册" )
    Success _ -> ok $ toResponse $ Layout.Result.result "登录成功"

aboutLogin runApp = do
  msum [
      dir "login" $ do
        nullDir
        method GET
        runApp loginPage
    , dir "login" $ do
        nullDir
        method POST
        runApp loginHandler
    ]
