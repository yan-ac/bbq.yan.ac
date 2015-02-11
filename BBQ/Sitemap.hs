{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TypeOperators, TemplateHaskell, OverloadedStrings #-}

module BBQ.Sitemap where

import Prelude           hiding (id, (.))
import Control.Category  (Category(id, (.)))

import Control.Monad     (liftM)
import Data.Data         (Data, Typeable)
import Data.Text         (Text)
import Text.Hamlet
import Text.Boomerang.TH (makeBoomerangs)
import Web.Routes        (PathInfo(..), MonadRoute, URL, askRouteFn)
import Web.Routes.Boomerang

newtype ArticleId = ArticleId { unArticleId :: Int }
  deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)

data Sitemap
  = Home
  | Article ArticleId
  | UserOverview
  | UserDetail Int Text
  deriving (Eq, Ord, Read, Show, Data, Typeable)

$(makeBoomerangs ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap =
  (  rHome
  <> rArticle . (lit "article" </> articleId)
  <> lit "users" . users
  )
  where
    users =  rUserOverview
      <> rUserDetail </> int . lit "-" . anyText

articleId :: Router () (ArticleId :- ())
articleId =
  xmaph ArticleId (Just . unArticleId) int

askRouteFn' :: MonadRoute m => m (URL m -> [(Text, Text)] -> Text)
askRouteFn' = liftM convert $ askRouteFn
  where convert routeFn = (\url params -> routeFn url $ map (\(t1, t2) -> (t1, Just t2)) params)

siteLayout' :: String -> HtmlUrl Sitemap -> [String] -> [String] -> HtmlUrl Sitemap
siteLayout' title body stylesheets' scripts' = $(hamletFile "views/hamlets/layout.hamlet")
  where stylesheets = "/static/css/general.css"
                    : stylesheets'
        scripts     = "static/js/plugins.js"
                    : scripts'

siteLayout title body = siteLayout' title body [] []
