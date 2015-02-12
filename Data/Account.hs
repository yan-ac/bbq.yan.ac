{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies #-}

module Data.Account
  ( AccountId(..), Email(..), Password(..), PersonalInfo, Account, Accounts
  , initialAccountsState
  , InsertNewAccount
  , UpdatePassword
  , UpdatePersonalInformation
  , CheckEmailAddress
  , GetAccountId
  , GetAccount
  , Authenticate
  , ListByEmail
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

newtype AccountId = AccountId Int
  deriving (Eq, Ord, Data, Typeable)
newtype Email     = Email String
  deriving (Eq, Ord, Data, Typeable, Show)
newtype Password  = Password ByteString
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

data Accounts = Accounts
  { nextAccountId :: AccountId
  , accounts      :: IxSet Account
  }
  deriving (Data, Typeable)
$(deriveSafeCopy 0 'base ''Accounts)

isValidEmailAddress = Text.Email.isValid . BS.pack

insertNewAccount' :: Email -> Password -> PersonalInfo -> Accounts -> Accounts
insertNewAccount' email pswd info pool = Accounts nextId accounts'
  where thisId  = nextAccountId pool
        (AccountId thisId') = thisId
        nextId    = AccountId (thisId' + 13)
        account   = Account thisId email pswd info
        accounts' = Ix.insert account $ accounts pool

getAccountId' :: Email -> Accounts -> Maybe AccountId
getAccountId' email pool = accountId <$> (Ix.getOne $ accounts pool @= email)

getAccount' :: AccountId -> Accounts -> Maybe Account
getAccount' id pool = Ix.getOne $ accounts pool @= id

updatePassword' :: AccountId -> Password -> Accounts -> Accounts
updatePassword' id pswd pool = pool { accounts = Ix.insert account' $ accounts pool }
  where (Just account) = getAccount' id pool
        account'       = account { password = pswd }

updatePersonalInformation' :: AccountId -> PersonalInfo -> Accounts -> Accounts
updatePersonalInformation' id info pool = pool { accounts = Ix.insert account' $ accounts pool }
  where (Just account) = getAccount' id pool
        account'       = account { personalInfo = info }

listByEmail' :: Accounts -> [Account]
listByEmail' pool = Ix.toDescList (Proxy :: Proxy Email) $ accounts pool

-- Acid Part --
initialAccountsState :: Accounts
initialAccountsState = Accounts
  { nextAccountId = AccountId 1327948
  , accounts      = empty
  }

insertNewAccount :: Email -> Password -> PersonalInfo -> Update Accounts (Either String ())
insertNewAccount email pswd info =
  case getAccountId' email <$> get of
    Nothing -> do
      put $ insertNewAccount' email pswd info <$> get
      return $ Right ()
    Just _  -> return $ Left "该邮箱已被注册"

updatePassword :: Email -> Password -> Update Accounts (Either String ())
updatePassword email pswd =
  case getAccountId' email <$> get of
    Nothing -> return $ Left "用户不存在"
    Just id -> do
      put $ updatePassword' id pswd <$> get
      return $ Right ()

updatePersonalInformation :: AccountId -> PersonalInfo -> Update Accounts (Either String ())
updatePersonalInformation id info =
  case getAccount' id <$> get of
    Nothing -> return $ Left "用户不存在"
    Just _  -> do
      put $ updatePersonalInformation' id info <$> get
      return $ Right ()

checkEmailAddress :: Email -> Query Accounts (Either String ())
checkEmailAddress email =
  case getAccountId' email <$> ask of
    Nothing -> do
      let (Email email') = email
      if isValidEmailAddress email'
      then return $ Right ()
      else return $ Left "无效的邮箱地址"
    Just _  -> return $ Left "该邮箱已被注册"

getAccountId :: Email -> Query Accounts (Either String AccountId)
getAccountId email =
  case getAccountId' email <$> ask of
    Nothing -> return $ Left "用户不存在"
    Just id -> return $ Right id

getAccount :: AccountId -> Query Accounts (Either String Account)
getAccount id =
  case getAccount' id <$> ask of
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
