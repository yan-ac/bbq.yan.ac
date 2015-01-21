module Route where

import Control.Monad
import Data.Acid        (AcidState)
import Happstack.Server

import Data.BBQ
import Data.VCodePool
import Data.RequestState

import CheckUserAuth
import Layout.Basic

dispatch :: (AcidState BBQ, AcidState VCodePool) -> ServerPartT IO Response
dispatch (bbq, vcodePool) = do
  authResult        <- checkUserAuth vcodePool
  let basicTemplate' = basicTemplate authResult
  let state          = mkRequestState bbq vcodePool authResult basicTemplate'
  route $ runHandler state  


route :: (Handler Response -> ServerPartT IO Response) -> ServerPartT IO Response
route runHandler = msum [
    dir "public" $ serveDirectory DisableBrowsing ["index.html"] "public"
  ]

{-
route runApp' acid = do
  authResult <- checkUserAuth acid
  let runApp = runApp' acid
  msum [
      runApp (indexPage authResult)
    , aboutRegister authResult runApp
    , aboutLogin authResult runApp
--  , dir "list"   $ runApp (listPage authResult)
    , dir "public" $ serveDirectory DisableBrowsing ["index.html"] "public"
    , runApp (e404Page authResult)
    ]
-}
