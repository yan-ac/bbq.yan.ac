module BBQ.Route where

import Happstack.Server     (Response)
import Web.Routes           (RouteT, runRouteT, Site, setDefault)
import Web.Routes.Boomerang (boomerangSite)

import           BBQ.Sitemap
import qualified BBQ.Home
import qualified BBQ.Registration

import Data.Accounts
import Data.RecordPool
import Data.AppConfig

route :: Sitemap -> RouteT Sitemap App Response
route url = do
    case url of
      Home               -> BBQ.Home.page
      NewRegistration    -> BBQ.Registration.newRegistration
      Registration vcode -> BBQ.Registration.registraion vcode

site :: Site Sitemap (App Response)
site = do
  setDefault Home $ boomerangSite (runRouteT route) sitemap
