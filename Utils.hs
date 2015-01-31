{-# LANGUAGE CPP #-}
module Utils where

import Control.Monad.Trans.Either

import Data.BBQ (Email(..))
import Data.VCodePool (VCode(..), ExpireTime(..))

import qualified Data.Char
import qualified Data.ByteString.Char8
import qualified Text.Email.Validate

import Happstack.Server
import Control.Monad.Trans
import Data.RequestState
import Middleware.KeyHolder

thenThrowError cond err = if cond then left err else right ()
elseThrowError cond err = if cond then right () else left err

extractEither e = case e of
  Left a  -> left a
  Right b -> right b

#ifdef DEBUG
hostname = "localhost:8000"
#else
hostname = "https://bbq.yan.ac"
#endif

mkVerificationLink :: Bool -> String -> Email -> VCode -> String
mkVerificationLink flag base (Email email) (VCode vcode) =
  prefix ++ base ++ "?email=" ++ email ++ "&vcode=" ++ vcode
  where prefix = if flag then hostname else ""

dashboardURI :: String
dashboardURI = "/dashboard"

loginedMsg :: String
loginedMsg = "您已登录"

inRange lo hi val = val >= lo && val <= hi
isHanChar = inRange 0x4e00 0x9fff . Data.Char.ord

vrEmail = Text.Email.Validate.isValid . Data.ByteString.Char8.pack

fetchEmailVCode queryEvent = do
  email'     <- queryString $ look "email"
  vcode'     <- queryString $ look "vcode"
  let email  = Email email'
  let vcode  = VCode vcode'
  now        <- liftIO $ getCurrentTimeInSecond
  authResult <- query $ queryEvent email vcode (ExpireTime now)
  case authResult of
    Left errMsg -> return (Left errMsg)
    Right _     -> return (Right (email, vcode))
