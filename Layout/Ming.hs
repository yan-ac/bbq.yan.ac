{-# LANGUAGE OverloadedStrings #-}
module Layout.Ming (ming) where

import Control.Monad
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (customAttribute, customParent, stringValue)

mkStyleSheetLink url = H.link ! A.href (stringValue url) ! A.rel "stylesheet"
mkScriptLink url = H.script ! A.src (stringValue url) $ do ""

mkNavLink :: (String, String, String) -> H.Html
mkNavLink (url, icon, text) = H.li $
  H.a ! customAttribute "data-scroll" ""
      ! A.href (stringValue url) $ do
        H.i ! A.class_ (stringValue $ "fa fa-" ++ icon) $ do H.toHtml $ " " ++ text

ming :: [String] -> [String] -> Bool -> String -> H.Html -> H.Html
ming styleList scriptList logined title body = 
  H.docTypeHtml ! A.lang "zh-cmn-Hans-CN" $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! customAttribute "http-equiv"
                "X-UA-Compatible"
             ! A.content "IE=edge,chrome=1"
      H.meta ! A.name "viewport"
             ! A.content "width=device-width, initial-scale=1.0"
      H.title (H.toHtml $ title ++ " | 全国校际语言学友谊赛")

      mapM_ mkStyleSheetLink $
            styleList ++ [
                "http://yan.ac/public/fonts/fonts.css"
              , "/public/images/vendors/bootstrap.min.css"
              , "http://libs.useso.com/js/font-awesome/4.2.0/css/font-awesome.min.css"
              , "/public/css/bbq.yan.ac.css"
              ]

      H.link ! A.rel "apple-touch-icon"
             ! A.href "/public/images/icon/apple-touch-icon.v01.png"
      H.link ! A.rel "shortcut icon"
             ! A.href "/public/images/icon/favicon.v01.png"

    H.body $ do
      H.nav ! A.class_ "navbar navbar-default navbar-fixed-top" $ do
        H.div ! A.class_ "container-fluid" $ do
          H.div ! A.class_ "navbar-header" $ do
            H.button ! A.type_ "button" ! A.class_ "navbar-toggle"
                     ! customAttribute "data-toggle" "collapse"
                     ! customAttribute "data-target" "navbarfixed" $ do
                       H.span ! A.class_ "icon-bar" $ do ""
                       H.span ! A.class_ "icon-bar" $ do ""
                       H.span ! A.class_ "icon-bar" $ do ""
            H.a ! A.class_ "navbar-brand" ! A.href "/" $ do "全国校际语言学友谊赛"
          H.div ! A.class_ "collapse navbar-collapse" ! A.id "navbarfixed" $ do
            H.ul ! A.class_ "nav navbar-nav" $ do
              mapM_ mkNavLink
                    [ ("/#cover-content", "home", "首页")
                    , ("/#about", "info", "关于我们")
                    , ("/#ol-intro", "graduation-cap", "语竞简介")
                    , ("/#rules", "file-text", "比赛规则")
                    ]
            H.ul ! A.class_ "nav navbar-nav navbar-right" $ do
              mapM_ mkNavLink $ if logined
                then [("/dashboard", "user", "个人中心"), ("/logout", "sign-out", "登出")]
                else [("/register", "user", "注册"), ("/login", "sign-in", "登录")]
      H.div ! A.class_ "container" 
            ! A.style "padding: 60px 0 50px" $ do
        body
      mapM_ mkScriptLink $
            scriptList ++ [
              "http://libs.useso.com/js/jquery/2.1.1/jquery.min.js"
            , "http://libs.useso.com/js/bootstrap/3.2.0/js/bootstrap.min.js"
            ]
