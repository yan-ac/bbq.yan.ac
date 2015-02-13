module BBQ.Route where

import Happstack.Server     (Response)
import Web.Routes           (RouteT, runRouteT, Site, setDefault)
import Web.Routes.Boomerang (boomerangSite)

import           BBQ.Sitemap
import qualified BBQ.Home

import Data.Accounts
import Data.RecordPool
import Data.AppConfig

route :: Sitemap -> RouteT Sitemap App Response
route url = do
    case url of
      Home                  -> BBQ.Home.page

site :: Site Sitemap (App Response)
site = do
  setDefault Home $ boomerangSite (runRouteT route) sitemap
