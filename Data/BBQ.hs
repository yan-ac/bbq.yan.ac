{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Data.BBQ where

import Data.Data     ( Data, Typeable )
import Data.SafeCopy ( base, deriveSafeCopy )
import Data.IxSet    ( Indexable(..), IxSet(..), ixFun, ixSet )

newtype AccountId = AccountId { unAccountId :: Int }
  deriving (Eq, Ord, Data, Typeable)
newtype Email     = Email String
  deriving (Eq, Ord, Data, Typeable, Show)
newtype Password  = Password { unPassword :: String }
  deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''AccountId)
$(deriveSafeCopy 0 'base ''Email)
$(deriveSafeCopy 0 'base ''Password)

type PersonalInfo = (String, String, String)

instance Show AccountId where
  show (AccountId accountId) = show accountId

data Account = Account
  { accountId    :: AccountId
  , email        :: Email
  , password     :: Password
  , personalInfo :: PersonalInfo
  }
  deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''Account)

instance Indexable Account where
  empty = ixSet
    [ ixFun $ \bp -> [ accountId bp ]
    , ixFun $ \bp -> [ email bp ]
    ]

data BBQ = BBQ
  { nextAccountId :: AccountId
  , accounts      :: IxSet Account
  }
  deriving (Data, Typeable)
$(deriveSafeCopy 0 'base ''BBQ)
