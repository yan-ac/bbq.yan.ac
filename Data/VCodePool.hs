{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, FlexibleInstances #-}

module Data.VCodePool where

import Data.Data       ( Data, Typeable )
import Data.SafeCopy   ( base, deriveSafeCopy )
import Data.IxSet      ( Indexable(..), IxSet(..), ixFun, ixSet )
import Control.Applicative

import Data.BBQ

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
