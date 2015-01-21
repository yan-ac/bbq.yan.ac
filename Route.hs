module Route where

import Control.Monad
import Data.Acid        (AcidState)
import Happstack.Server

import Data.BBQ
import Data.VCodePool
import Data.RequestState

import CheckUserAuth

import Page.Authenticate
import Page.List
import Page.StaticPages

dispatch :: (AcidState BBQ, AcidState VCodePool) -> ServerPartT IO Response
dispatch (bbq, vcodePool) = do
  authResult        <- checkUserAuth vcodePool
  let state          = mkRequestState bbq vcodePool authResult
  route $ runHandler state

route :: (Handler Response -> ServerPartT IO Response) -> ServerPartT IO Response
route runHandler = msum [
    runHandler authenticate
  , dir "list"   $ runHandler showDatabase
  , dir "public" $ serveDirectory DisableBrowsing [] "public"
  , runHandler staticPages
  ]
