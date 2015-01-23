module CheckUserAuth where

import Control.Applicative     ((<$>), (<*>))
import Control.Monad
import Control.Monad.Trans
import Happstack.Server
import Happstack.Server.RqData (getDataFn)
import Data.Acid
import Data.Acid.Advanced      (query')

import Data.BBQ
import Acid.BBQ
import Data.VCodePool
import Acid.VCodePool
import Data.MaybeFail
import KeyHolder

tryObtainAuthCookies :: RqData (String, String)
tryObtainAuthCookies =
    (,) <$> lookCookieValue "accountId" <*> lookCookieValue "accessKey"

checkUserAuth :: AcidState VCodePools -> ServerPartT IO (Maybe AccountId)
checkUserAuth vcodePools = do
  c <- getDataFn tryObtainAuthCookies
  case c of
    Left _ -> return Nothing
    Right (accountId, accessKey) -> do
      now <- liftIO $ getCurrentTimeInSecond
      let accountId' =  AccountId (read accountId :: Int)
      authResult <- query' vcodePools (VerifyCookie accountId' (VCode accessKey) (ExpireTime now))
      case authResult of
        Left  _ -> return Nothing
        Right _ -> return (Just accountId')
