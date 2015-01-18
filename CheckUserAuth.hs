module CheckUserAuth where

import Control.Applicative  (Applicative, Alternative, (<$>), (<*>))
import Control.Monad
import Control.Monad.Trans
import Happstack.Server
import Happstack.Server.RqData (getDataFn)
import Data.Acid.Advanced (query')

import Data.BBQ
import Acid.BBQ
import Data.VCodePool
import Acid.VCodePool
import Data.MaybeFail
import Layout.Basic
import AcidProvider
import KeyHolder


tryObtainAuthCookies :: RqData (String, String)
tryObtainAuthCookies =
    (,) <$> lookCookieValue "accountId" <*> lookCookieValue "accessKey"

checkUserAuth :: Acid -> ServerPartT IO (Maybe AccountId)
checkUserAuth acid = do
  c <- getDataFn tryObtainAuthCookies
  case c of
    Left _ -> return Nothing
    Right (accountId, accessKey) -> do
      now <- liftIO $ getCurrentTimeInSecond
      let vcodePool = vcodePoolState acid
      let accountId' = read accountId :: Int
      let accountId'' = AccountId accountId'
      authResult <- query' vcodePool (VerifyAccountVCode accountId'' (VCode accessKey) (ExpireTime now))
      if authResult
        then do
          return (Just accountId'')
        else do
          return Nothing