{-# LANGUAGE DeriveDataTypeable, TemplateHaskell,
  TypeFamilies, RecordWildCards, NamedFieldPuns #-}

module Acid.VCodePool where

import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Control.Monad.Trans
import Data.Maybe
import Data.Acid            ( Query, Update, makeAcidic )
import Data.IxSet           ( (@=), Proxy(..), getOne, empty )
import qualified Data.IxSet as IxSet
import Data.BBQ
import Data.VCodePool
import Data.MaybeFail

initialVCodePool :: VCodePool
initialVCodePool = VCodePool
  { emailVCodes   = empty
  , accountVCodes = empty
  }

newEmailVCode :: Email -> VCode -> ExpireTime -> Update VCodePool ()
newEmailVCode email vcode expireTime = do
  pool@VCodePool{..} <- get
  let emailVCode = EmailVCode {
      evEmail      = email
    , evCode       = vcode
    , evExpireTime = expireTime
    }
  put $ pool
    { emailVCodes   = IxSet.updateIx email emailVCode emailVCodes
    , accountVCodes = accountVCodes
    }

newAccountVCode :: AccountId -> VCode -> ExpireTime -> Update VCodePool ()
newAccountVCode accountId vcode expireTime = do
  pool@VCodePool{..} <- get
  let accountVCode = AccountVCode {
      avAccountId  = accountId
    , avAccessKey  = vcode
    , avExpireTime = expireTime
    }
  put $ pool
    { emailVCodes   = emailVCodes
    , accountVCodes = IxSet.updateIx accountId accountVCode accountVCodes
    }

verifyEmailVCode :: Email -> VCode -> ExpireTime -> Query VCodePool (MaybeFail ())
verifyEmailVCode email givenCode now = do
  pool@VCodePool{..} <- ask
  case getOne $ emailVCodes @= email of
    Nothing -> return $ (Fail "RECORD NOT EXISTED")
    Just EmailVCode { evCode, evExpireTime } ->
      if evCode /= givenCode
        then return $ (Fail "INVALID VCODE")
        else if evExpireTime < now
          then return (Fail "EXPIRED RECORD")
          else return (Success ())

verifyAccountVCode :: AccountId -> VCode -> ExpireTime -> Query VCodePool Bool
verifyAccountVCode accountId givenAccessKey now = do
  pool@VCodePool{..} <- ask
  case getOne $ accountVCodes @= accountId of
    Nothing -> return False
    Just AccountVCode { avAccessKey, avExpireTime } ->
      if avAccessKey /= givenAccessKey
        then return False
        else if avExpireTime < now
          then return False
          else return True

deleteEmailVCode :: Email -> Update VCodePool ()
deleteEmailVCode email = do
  pool@VCodePool{..} <- get
  put $ pool
    { emailVCodes   = IxSet.deleteIx email emailVCodes
    , accountVCodes = accountVCodes
    }

getPool :: Query VCodePool VCodePool
getPool = ask

$(makeAcidic ''VCodePool
  [ 'newEmailVCode
  , 'newAccountVCode
  , 'verifyEmailVCode
  , 'verifyAccountVCode
  , 'deleteEmailVCode
  , 'getPool
  ])
