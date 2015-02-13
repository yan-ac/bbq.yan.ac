module BBQ.Route where

import Happstack.Server
import Web.Routes           (RouteT, runRouteT, Site, setDefault)
import Web.Routes.Boomerang (boomerangSite)
import Control.Monad        (msum)

import           BBQ.Sitemap
import qualified BBQ.Home
import qualified BBQ.Registration
import qualified BBQ.Authentication

import Data.Accounts
import Data.RecordPool
import Data.AppConfig

import Control.Monad.Trans     (MonadIO(..))

route :: Sitemap -> RouteT Sitemap App Response
route url = do
    case url of
      Home            -> BBQ.Home.page
      NewRegistration -> BBQ.Registration.newRegistration
      Registration vcode -> msum
        [ method  GET >>  BBQ.Registration.registrationGET  vcode
        , method POST >>  BBQ.Registration.registrationPOST vcode
        ]
      Authentication  -> BBQ.Authentication.auth

site :: Site Sitemap (App Response)
site = do
  setDefault Home $ boomerangSite (runRouteT route) sitemap
