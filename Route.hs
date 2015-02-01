module Route where

import Control.Monad
import Data.Acid        (AcidState)
import Happstack.Server

import Data.BBQ
import Data.VCodePool
import Data.RequestState

import Middleware.Authenticate

import Page.Authenticate
import Page.List
import Page.StaticPages
import qualified Page.Registration
import qualified Page.ResetPassword
import qualified Page.Dashboard

dispatch :: (AcidState BBQ, AcidState VCodePools) -> ServerPartT IO Response
dispatch (bbq, vcodePools) = do
  authResult        <- checkAuthCookie vcodePools
  let state          = mkRequestState bbq vcodePools authResult
  route $ runHandler state

route :: (Handler Response -> ServerPartT IO Response) -> ServerPartT IO Response
route runHandler = msum [
    runHandler authenticate
  , runHandler Page.Registration.entry
  , runHandler Page.ResetPassword.entry
  , runHandler Page.Dashboard.entry
  , dir "list"   $ runHandler showDatabase
  , dir "public" $ serveDirectory DisableBrowsing [] "public"
  , runHandler staticPages
  ]
