{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

module BBQ.Sitemap where

import Prelude           hiding (id, (.))
import Control.Category  (Category(id, (.)))

import Control.Monad     (liftM)
import Data.Data         (Data, Typeable)
import Data.Text         (Text)
import qualified Data.Text as T
import Text.Hamlet
import Text.Boomerang.TH (makeBoomerangs)
import Web.Routes        (PathInfo(..), MonadRoute, URL, askRouteFn)
import Web.Routes.Boomerang

import Data.Accounts
import Data.RecordPool

deriving instance Read VCode
deriving instance PathInfo VCode

data Sitemap
  = Home
  | NewRegistration
  | Registration VCode
  deriving (Eq, Ord, Read, Show, Data, Typeable)

$(makeBoomerangs ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap =
  (  rHome
  <> lit "register" . registration
  )
  where registration = rNewRegistration
                    <> rRegistration </> vcodeParams

vcodeParams :: Router () (VCode :- ())
vcodeParams = xmaph (VCode . T.unpack) (Just . T.pack . unVCode) anyText

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
