module BBQ.Route where

import Happstack.Server
import Web.Routes           (RouteT, runRouteT, Site, setDefault, showURL)
import Web.Routes.Boomerang (boomerangSite)
import Control.Monad        (msum)
import Control.Monad.Trans  (lift)

import           BBQ.Sitemap
import qualified BBQ.Home
import qualified BBQ.Registration
import qualified BBQ.Authentication
import qualified BBQ.Dashboard
import qualified BBQ.Problems
import qualified BBQ.Upload

import BBQ.JSOrder

import Data.Accounts
import Data.RecordPool
import Data.Sheets
import Data.AppConfig

import Control.Monad.Trans     (MonadIO(..))

route :: Sitemap -> RouteT Sitemap App Response
route url = do
    case url of
      Home            -> BBQ.Home.page
      NewRegistration -> BBQ.Registration.newRegistration
      Registration vcode -> msum
        [ method  GET >> BBQ.Registration.registrationGET  vcode
        , method POST >> BBQ.Registration.registrationPOST vcode
        ]
      Authentication  -> msum
        [ method  GET >> BBQ.Authentication.loginPage
        , method POST >> BBQ.Authentication.auth
        ]
      Problem pid     -> BBQ.Problems.problemPage pid
      _ -> routeBasedOnUserStat url

routeBasedOnUserStat :: Sitemap -> RouteT Sitemap App Response
routeBasedOnUserStat url = do
  authURL      <- showURL Authentication
  dashboardURL <- showURL Dashboard

  loginStat <- lift $ askUserStat
  case loginStat of
    Nothing -> seeOther authURL $ toResponse "会话已过期，请登录"
    Just (aid, bbqStat) -> do
      case url of
        Dashboard ->
          case bbqStat of
            BBQNotStarted -> BBQ.Dashboard.notStartPage
            BBQInProgress -> BBQ.Dashboard.inProgressPage
            BBQFinished   -> BBQ.Dashboard.finishedPage
        IgniteFire -> forbidden $ toResponse "比赛已经结束"
        Upload pid ->
          case bbqStat of
            BBQInProgress -> BBQ.Upload.upload aid pid
            _ -> seeOther dashboardURL $ toResponse "不在比赛中"
        ViewSheets pid ->
          case bbqStat of
            BBQNotStarted -> seeOther dashboardURL $ toResponse "尚未参赛"
            _ -> BBQ.Upload.viewSheets aid pid
        ViewASheet pid sid ->
          case bbqStat of
            BBQNotStarted -> seeOther dashboardURL $ toResponse "尚未参赛"
            _ -> BBQ.Upload.viewASheet aid pid sid

site :: Site Sitemap (App Response)
site = do
  setDefault Home $ boomerangSite (runRouteT route) sitemap
