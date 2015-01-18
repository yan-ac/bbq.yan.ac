{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Data.VCodePool where

import Data.Data       ( Data, Typeable )
import Data.SafeCopy   ( base, deriveSafeCopy )
import Data.IxSet      ( Indexable(..), IxSet(..), ixFun, ixSet )
import Control.Applicative
import Data.Time       ( formatTime )
import Data.Time.Clock ( getCurrentTime )
import System.Locale   ( defaultTimeLocale )
import System.Random

import Data.BBQ

newtype ExpireTime = ExpireTime { unExpireTime :: Int }
  deriving (Eq, Ord, Data, Typeable, Show)
newtype VCode = VCode { unVCode :: String }
  deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''ExpireTime)  
$(deriveSafeCopy 0 'base ''VCode)

data EmailVCode = EmailVCode
  { evEmail      :: Email
  , evCode       :: VCode
  , evExpireTime :: ExpireTime
  } deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''EmailVCode)

data AccountVCode = AccountVCode
  { avAccountId  :: AccountId
  , avAccessKey  :: VCode
  , avExpireTime :: ExpireTime
  } deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''AccountVCode)

instance Indexable EmailVCode where
  empty = ixSet [ ixFun $ \bp -> [ evEmail bp ] ]

instance Indexable AccountVCode where
  empty = ixSet [ ixFun $ \bp -> [ avAccountId bp ] ]

data VCodePool = VCodePool
  { emailVCodes   :: IxSet EmailVCode
  , accountVCodes :: IxSet AccountVCode
  } deriving (Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''VCodePool)
