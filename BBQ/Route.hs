module BBQ.Route where

import Happstack.Server     (Response, ServerPartT)
import Web.Routes           (RouteT, runRouteT, Site, setDefault)
import Web.Routes.Boomerang (boomerangSite)

import           BBQ.Sitemap
import qualified BBQ.Home

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url =
    case url of
      Home                  -> BBQ.Home.page

site :: Site Sitemap (ServerPartT IO Response)
site =
  setDefault Home $ boomerangSite (runRouteT route) sitemap
