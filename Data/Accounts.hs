{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Data.Accounts
  ( AccountId(..), Email(..), Password(..), PersonalInfo, Account, Accounts
  , openAccountsState
  , isValidEmailAddress
  , mkPassword
  , InsertNewAccount(..)
  , UpdatePassword(..)
  , UpdatePersonalInformation(..)
  , CheckEmailAddress(..)
  , GetAccountId(..)
  , GetAccount(..)
  , Authenticate(..)
  , ListByEmail(..)
  )
  where

import Control.Applicative     ((<$>))
import Data.Data               (Data, Typeable)
import Data.SafeCopy           (base, deriveSafeCopy)
import Data.IxSet 
    ( Indexable(..), IxSet(..), Proxy(..)
    , ixFun, ixSet, (@=), empty)
import qualified Data.IxSet as Ix

import qualified Text.Email.Validate   as Text.Email
import qualified Data.ByteString.Char8 as BS

import Control.Monad.Reader    (ask)
import Control.Monad.State     (get, put)
import Data.Acid               (Query, Update, makeAcidic)

import Data.ByteString         (ByteString(..))
import Crypto.BCrypt           (validatePassword)

import System.FilePath         ((</>))
import Data.Acid.SafeOpen

newtype AccountId = AccountId Int
  deriving (Eq, Ord, Data, Typeable, Show)
newtype Email     = Email String
  deriving (Eq, Ord, Data, Typeable, Show)
newtype Password  = Password ByteString
  deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''AccountId)
$(deriveSafeCopy 0 'base ''Email)
$(deriveSafeCopy 0 'base ''Password)

mkPassword = Password . BS.pack

type PersonalInfo = (String, String, String)

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

data Accounts = Accounts
  { nextAccountId :: AccountId
  , accounts      :: IxSet Account
  }
  deriving (Data, Typeable)
$(deriveSafeCopy 0 'base ''Accounts)

isValidEmailAddress = Text.Email.isValid . BS.pack

insertNewAccount' :: Accounts -> Email -> Password -> PersonalInfo -> (Accounts, AccountId)
insertNewAccount' pool email pswd info = (Accounts nextId accounts', thisId)
  where thisId  = nextAccountId pool
        (AccountId thisId') = thisId
        nextId    = AccountId (thisId' + 13)
        account   = Account thisId email pswd info
        accounts' = Ix.insert account $ accounts pool

getAccountId' :: Accounts -> Email -> Maybe AccountId
getAccountId' pool email = accountId <$> (Ix.getOne $ accounts pool @= email)

getAccount' :: Accounts -> AccountId -> Maybe Account
getAccount' pool id = Ix.getOne $ accounts pool @= id

updatePassword' :: Accounts -> AccountId -> Password -> Accounts
updatePassword' pool id pswd = pool { accounts = Ix.insert account' $ accounts pool }
  where (Just account) = getAccount' pool id
        account'       = account { password = pswd }

updatePersonalInformation' :: Accounts -> AccountId -> PersonalInfo -> Accounts
updatePersonalInformation' pool id info = pool { accounts = Ix.insert account' $ accounts pool }
  where (Just account) = getAccount' pool id
        account'       = account { personalInfo = info }

listByEmail' :: Accounts -> [Account]
listByEmail' pool = Ix.toDescList (Proxy :: Proxy Email) $ accounts pool

-- Acid Part --
initialAccountsState :: Accounts
initialAccountsState = Accounts
  { nextAccountId = AccountId 1327948
  , accounts      = empty
  }

insertNewAccount :: Email -> Password -> PersonalInfo -> Update Accounts (Either String AccountId)
insertNewAccount email pswd info = do
  pool <- get
  case getAccountId' pool email of
    Nothing -> do
      let (pool', id) = insertNewAccount' pool email pswd info
      put pool'
      return $ Right id
    Just _  -> return $ Left "该邮箱已被注册"

updatePassword :: Email -> Password -> Update Accounts (Either String ())
updatePassword email pswd = do
  pool <- get
  case getAccountId' pool email of
    Nothing -> return $ Left "用户不存在"
    Just id -> do
      put $ updatePassword' pool id pswd
      return $ Right ()

updatePersonalInformation :: AccountId -> PersonalInfo -> Update Accounts (Either String ())
updatePersonalInformation id info = do
  pool <- get
  case getAccount' pool id of
    Nothing -> return $ Left "用户不存在"
    Just _  -> do
      put $ updatePersonalInformation' pool id info
      return $ Right ()

checkEmailAddress :: Email -> Query Accounts (Either String ())
checkEmailAddress email = do
  pool <- ask
  case getAccountId' pool email of
    Nothing -> do
      let (Email email') = email
      if isValidEmailAddress email'
      then return $ Right ()
      else return $ Left "无效的邮箱"
    Just _  -> return $ Left "该邮箱已被注册"

getAccountId :: Email -> Query Accounts (Either String AccountId)
getAccountId email = do
  pool <- ask
  case getAccountId' pool email of
    Nothing -> return $ Left "用户不存在"
    Just id -> return $ Right id

getAccount :: AccountId -> Query Accounts (Either String Account)
getAccount id = do
  pool <- ask
  case getAccount' pool id of
    Nothing -> return $ Left "用户不存在"
    Just a  -> return $ Right a

authenticate :: Email -> Password -> Query Accounts Bool
authenticate email (Password pswd) = do
  pool <- ask
  case fmap password $ getAccountId' pool email >>= getAccount' pool of
    Nothing               -> return False
    Just (Password pswd') -> return $ validatePassword pswd' pswd

listByEmail :: Query Accounts [Account]
listByEmail = listByEmail' <$> ask

$(makeAcidic ''Accounts
  [ 'insertNewAccount
  , 'updatePassword
  , 'updatePersonalInformation
  , 'checkEmailAddress
  , 'getAccountId
  , 'getAccount
  , 'authenticate
  , 'listByEmail
  ])

openAccountsState basePath action =
  let path = basePath </> "Accounts"
  in  withLocalState path initialAccountsState $ \st ->
        action st
