{-# LANGUAGE OverloadedStrings #-}
module Page.StaticPages (staticPages) where

import Happstack.Server
import Control.Monad
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.RequestState

staticPages = do
  template <- askBasicTemplate
  msum [
      indexPage template
    , dir "login"           $ loginPage template
    , dir "forget-password" $ forgetPasswordPage template
    , e404Page template
    ]

-- Static Pages --

indexPage template = do
  nullDir
  ok $ toResponse $ template
    "首页"
    ( H.h1 $ do "第一届烧烤节语言学友谊赛" )

e404Page template = notFound $ toResponse $ template
  "页面不存在"
  ( do
      H.h1 $ do "页面不存在"
      H.p $ do "您所请求的页面不存在。该页面可能尚未建立，请耐心等候。"
      H.p $ do "如果您自身拥有相关技能（JavaScript/HTML/CSS 或者 Haskell），并愿意帮助我们加速网站的建设工作，可以联系我们。"
  )

-- GET of Dynamic Pages --

dashboardURI :: String
dashboardURI = "/dashboard"

loginedMsg :: String
loginedMsg = "您已登陆"

loginPage template = do
  authResult <- askAuthResult
  case authResult of
    Just _  -> seeOther dashboardURI (toResponse loginedMsg)
    Nothing -> ok $ toResponse $ template
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
          H.a ! A.href "/forget-password" $ do "忘记密码？"
      )

forgetPasswordPage template = ok $ toResponse $ template
  "重置密码"
  ( do
    H.h1 $ do "重置密码"
    H.form ! A.action "/forget-password"
           ! A.method "post" $ do
      "输入你的邮箱"
      H.input ! A.type_ "email"
              ! A.name "email"
      H.button ! A.type_ "submit"
               ! A.name "reset-password" $ do "重置密码"
  )
