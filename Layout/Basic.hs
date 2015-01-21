{-# LANGUAGE OverloadedStrings #-}
module Layout.Basic (mkBasicTemplate) where

import Control.Monad
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (customAttribute, customParent, stringValue)

basicTemplate :: String -> [(String, String)] -> H.Html -> H.Html
basicTemplate title navList body = 
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.name "viewport"
             ! A.content "width=device-width, initial-scale=1.0"
      H.title (H.toHtml title)
      H.link ! A.rel "stylesheet"
             ! A.href "/public/css/foundation.css"
      H.script ! A.src "/public/js/modernizr.js" $ do ""
    H.body $ do
      H.nav ! A.class_ "top-bar"
            ! customAttribute "data-topbar" "" $ do
              H.ul ! A.class_ "title-area" $ do
                H.li ! A.class_ "name" $ do
                  H.h1 $ do H.a ! A.href "/" $ do "烧烤节友谊赛"
                H.li ! A.class_ "toggle-topbar menu-icon" $ do
                  H.a ! A.href "" $ do H.span $ do ""
              H.section ! A.class_ "top-bar-section" $ do
                H.ul ! A.class_ "right" $ do
                  mapM_ (\(url, title) -> sequence_
                    [ H.li ! A.class_ "divider" $ do ""
                    , H.li $ do H.a ! A.href (stringValue url) $ do (H.toHtml title)
                    ])
                    navList
      customParent "main" ! A.class_ "row" $ do
        H.div ! A.class_ "small-10 large-8 small-centered columns" $ do
          body

      mapM_ (\url -> H.script ! A.src (stringValue ("/public/js/" ++ url)) $ do "")
            ["jquery.js", "foundation.min.js", "yan.ac.js"]
      H.script $ do "$(document).foundation();"

mkBasicTemplate :: (Maybe a) -> String -> H.Html -> H.Html
mkBasicTemplate authResult title body = do
  let commonList = [("/rules", "比赛规则"), ("/faq", "FAQ"), ("/about-us", "关于我们")]
  let loginedList = commonList ++ [("/dashboard", "个人中心"), ("/logout", "登出")]
  let unlogedList = commonList ++ [("/register", "注册"), ("/login", "登录")]
  case authResult of
    Nothing -> basicTemplate title unlogedList body
    Just _  -> basicTemplate title loginedList body
