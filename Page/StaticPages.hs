{-# LANGUAGE OverloadedStrings #-}
module Page.StaticPages (staticPages) where

import Happstack.Server
import Control.Monad
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.RequestState
import Layout.Basic

staticPages = do
  template <- loadTmplWithAuth basicTemplate
  msum [
      indexPage template
    , e404Page template
    ]

-- Static Pages --

indexPage template = do
  nullDir
  serveFile (guessContentTypeM mimeTypes) "public/index.html"

e404Page template = notFound $ toResponse $ template
  "页面不存在"
  ( do
      H.h1 $ do "页面不存在"
      H.p $ do "您所请求的页面不存在。该页面可能尚未建立，请耐心等候。"
      H.p $ do "如果您自身拥有相关技能（JavaScript/HTML/CSS 或者 Haskell），并愿意帮助我们加速网站的建设工作，可以联系我们。"
  )
