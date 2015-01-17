{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Data.BBQ where

import Data.Data     ( Data, Typeable )
import Data.SafeCopy ( base, deriveSafeCopy )
import Data.IxSet    ( Indexable(..), IxSet(..), ixFun, ixSet )

newtype AccountId = AccountId { unAccountId :: Int }
  deriving (Eq, Ord, Data, Typeable)
newtype Email     = Email String
  deriving (Eq, Ord, Data, Typeable, Show)
newtype Password  = Password String
  deriving (Eq, Ord, Data, Typeable, Show)
newtype UserInfo  = UserInfo String
  deriving (Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''AccountId)
$(deriveSafeCopy 0 'base ''Email)
$(deriveSafeCopy 0 'base ''Password)
$(deriveSafeCopy 0 'base ''UserInfo)

instance Show AccountId where
  show (AccountId accountId) = show accountId
instance Show UserInfo where
  show (UserInfo info) = info

data Account = Account
  { accountId :: AccountId
  , email     :: Email
  , password  :: Password
  , userInfo  :: UserInfo
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
