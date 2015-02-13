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
import Data.Sheets

deriving instance Read VCode
deriving instance PathInfo VCode

data Sitemap
  = Home
  | NewRegistration
  | Registration VCode
  | Authentication
  | Dashboard
  | IgniteFire
  | Upload ProblemId
  | Problem ProblemId
  | ViewSheets ProblemId
  | ViewASheet ProblemId SheetId
  deriving (Eq, Ord, Read, Show, Data, Typeable)

$(makeBoomerangs ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap =
  (  rHome
  <> lit "register"  . registration
  <> lit "auth"      . rAuthentication
  <> lit "dashboard" . rDashboard
  <> lit "bbq"       . bbqOps 
  <> lit "problems"  . rProblem </> problemIdParams
  <> rViewASheet . ("sheet" </> problemIdParams </> sheetIdParams)
  )
  where registration = rNewRegistration </> "new"
                    <> rRegistration </> vcodeParams
        bbqOps = rIgniteFire </> "start" 
              <> rUpload     </> "upload" </> problemIdParams
              <> rViewSheets </> "sheets" </> problemIdParams

vcodeParams :: Router r (VCode :- r)
vcodeParams = xmaph (VCode . T.unpack) (Just . T.pack . unVCode) anyText

--problemIdParams :: Router () (ProblemId :- ())
problemIdParams :: Router r (ProblemId :- r)
problemIdParams = xmaph (ProblemId) pidToInt int
  where pidToInt (ProblemId x) = if and [x >= 1, x <= 9]
                                 then Just x
                                 else Nothing

sheetIdParams :: Router r (SheetId :- r)
sheetIdParams = xmaph (SheetId) (Just . unSheetId) int

askRouteFn' :: MonadRoute m => m (URL m -> [(Text, Text)] -> Text)
askRouteFn' = liftM convert $ askRouteFn
  where convert routeFn = (\url params -> routeFn url $ map (\(t1, t2) -> (t1, Just t2)) params)

siteLayout' :: String -> HtmlUrl Sitemap -> [String] -> [String] -> HtmlUrl Sitemap
siteLayout' title body stylesheets' scripts' = $(hamletFile "views/hamlets/layout.hamlet")
  where stylesheets = "/static/css/general.css"
                    : stylesheets'
        scripts     = "/static/js/plugins.js"
                    : scripts'

siteLayout title body = siteLayout' title body [] []
