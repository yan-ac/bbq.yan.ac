{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, FlexibleInstances,
  TypeFamilies, FlexibleContexts, NamedFieldPuns #-}

module Data.VCodePool where

import Data.Data       ( Data, Typeable )
import Data.SafeCopy   ( base, deriveSafeCopy )
import Data.IxSet      ( Indexable(..), IxSet(..), ixFun, ixSet )

import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Control.Monad.Trans
import Data.Data            ( Data, Typeable )
import Data.Maybe
import Data.Acid            ( Query, Update, makeAcidic )
import Data.IxSet           ( (@=), Proxy(..), getOne, empty )
import qualified Data.IxSet as IxSet
import Data.Account

newtype ExpireTime = ExpireTime { unExpireTime :: Int }
  deriving (Eq, Ord, Data, Typeable, Show)
newtype VCode = VCode { unVCode :: String }
  deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''ExpireTime)  
$(deriveSafeCopy 0 'base ''VCode)

data VCodeRecord k = VCodeRecord {
    primaryKey :: k
  , vcode      :: VCode
  , expireTime :: ExpireTime
  } deriving (Eq, Ord, Data, Typeable, Show)

$(deriveSafeCopy 0 'base ''VCodeRecord)

instance Indexable (VCodeRecord Email) where
  empty = ixSet [ ixFun $ \bp -> [ primaryKey bp ] ]
instance Indexable (VCodeRecord AccountId) where
  empty = ixSet [ ixFun $ \bp -> [ primaryKey bp ] ]

type VCodePool k = IxSet (VCodeRecord k)

data VCodePools = VCodePools {
    newAccountPool  :: VCodePool Email
  , resetPasswdPool :: VCodePool Email
  , cookiePool      :: VCodePool AccountId
  } deriving (Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''VCodePools)

initialVCodePools :: VCodePools
initialVCodePools = VCodePools empty empty empty

updateNewAccountPool  pools pool = pools { newAccountPool  = pool }
updateResetPasswdPool pools pool = pools { resetPasswdPool = pool }
updateCookiePool      pools pool = pools { cookiePool      = pool }

newVCodeRecord
  :: (IxSet.Indexable (VCodeRecord k), Typeable k, Ord k)
  => (VCodePools -> VCodePool k)
  -> (VCodePools -> VCodePool k -> VCodePools)
  -> k -> VCode -> ExpireTime -> Update VCodePools ()
newVCodeRecord extract update key vcode expireTime = do
  let record = VCodeRecord key vcode expireTime
  pools <- get
  let pool = extract pools
  let pool' = IxSet.updateIx key record pool
  put $ update pools pool'

insertNewAccountRecord  = newVCodeRecord newAccountPool updateNewAccountPool
insertResetPasswdRecord = newVCodeRecord resetPasswdPool updateResetPasswdPool
insertCookieRecord      = newVCodeRecord cookiePool updateCookiePool

verifyVCodeRecord
  :: (IxSet.Indexable (VCodeRecord k), Typeable k, Ord k)
  => (VCodePools -> VCodePool k)
  -> k -> VCode -> ExpireTime -> Query VCodePools (Either String ())
verifyVCodeRecord extract key givenCode now = do
  pools <- ask
  let pool = extract pools
  case getOne $ pool @= key of
    Nothing -> return $ (Left "验证记录不存在")
    Just VCodeRecord { vcode, expireTime } ->
      if vcode /= givenCode
        then return $ (Left "无效的验证信息")
        else if expireTime < now
          then return $ (Left "验证信息已过期")
          else return $ (Right ())

verifyNewAccountRecord  = verifyVCodeRecord newAccountPool
verifyResetPasswdRecord = verifyVCodeRecord resetPasswdPool
verifyCookieRecord      = verifyVCodeRecord cookiePool

deleteVCodeRecord
  :: (IxSet.Indexable (VCodeRecord k), Typeable k, Ord k)
  => (VCodePools -> VCodePool k)
  -> (VCodePools -> VCodePool k -> VCodePools)
  -> k -> Update VCodePools ()
deleteVCodeRecord extract update key = do
  pools <- get
  let pool = extract pools
  let pool' = IxSet.deleteIx key pool
  put $ update pools pool'

deleteNewAccountRecord  = deleteVCodeRecord newAccountPool updateNewAccountPool
deleteResetPasswdRecord = deleteVCodeRecord resetPasswdPool updateResetPasswdPool

getPools :: Query VCodePools VCodePools
getPools = ask

$(makeAcidic ''VCodePools
  [ 'insertNewAccountRecord
  , 'insertResetPasswdRecord
  , 'insertCookieRecord
  , 'verifyNewAccountRecord
  , 'verifyResetPasswdRecord
  , 'verifyCookieRecord
  , 'deleteNewAccountRecord
  , 'deleteResetPasswdRecord
  , 'getPools
  ])
