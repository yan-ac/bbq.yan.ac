{-# LANGUAGE DeriveDataTypeable, TemplateHaskell,
  TypeFamilies, RecordWildCards #-}

module Acid.BBQ where

import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get, put )
import Data.Maybe
import Data.Acid            ( Query, Update, makeAcidic )
import Data.IxSet           ( (@=), Proxy(..), getOne, empty )
import qualified Data.IxSet as IxSet
import Data.BBQ
import Data.MaybeFail

initialBBQState :: BBQ
initialBBQState = BBQ
  { nextAccountId = AccountId 1327948
  , accounts      = empty
  }

newAccount :: (Email, Password) -> Update BBQ (MaybeFail AccountId)
newAccount (email, password) = do
  bbq@BBQ{..} <- get
  case getOne $ accounts @= email of
    Just _  -> return (Fail "USER EXISTED.")
    Nothing -> do
      let thisAccountId = nextAccountId
      let thisAccount = Account {
          accountId = thisAccountId
        , email     = email
        , password  = password
        , userInfo  = UserInfo "{}"
        }
      put $ bbq
        { nextAccountId = AccountId ((unAccountId thisAccountId) + 13)
        , accounts      = IxSet.insert thisAccount accounts
        }
      return (Success thisAccountId)

resetPassword :: (Email, Password) -> Update BBQ (MaybeFail ())
resetPassword (email, password) = do
  bbq@BBQ{..} <- get
  case getOne $ accounts @= email of
    Nothing         -> return (Fail "USER NOT EXISTED.")
    Just oldAccount -> do
      let accountId' = accountId oldAccount
      let newAccount = Account {
          accountId = accountId'
        , email     = email
        , password  = password
        , userInfo  = userInfo oldAccount
        }
      put $ bbq
        { accounts = IxSet.updateIx accountId' newAccount accounts }      
      return $ Success ()

updateUserInfo :: (AccountId, UserInfo) -> Update BBQ (MaybeFail ())
updateUserInfo (accountId', userInfo) = do
  bbq@BBQ{..} <- get
  case getOne $ accounts @= accountId' of
    Nothing         -> return (Fail "USER NOT EXISTED.")
    Just oldAccount -> do
      let newAccount = Account {
          accountId = accountId'
        , email     = email oldAccount
        , password  = password oldAccount
        , userInfo  = userInfo
        }
      put $ bbq
        { accounts = IxSet.updateIx accountId' newAccount accounts }      
      return $ Success ()

isEmailRegisterd :: Email -> Query BBQ Bool
isEmailRegisterd email = do
  bbq@BBQ{..} <- ask
  case getOne $ accounts @= email of
    Nothing      -> return False
    Just account -> return True

getAccountId :: Email -> Query BBQ (MaybeFail AccountId)
getAccountId email = do
  bbq@BBQ{..} <- ask
  case getOne $ accounts @= email of
    Nothing      -> return (Fail "USER NOT EXISTED.")
    Just account -> return (Success (accountId account))

getUserInfo :: AccountId -> Query BBQ (MaybeFail UserInfo)
getUserInfo accountId = do
  bbq@BBQ{..} <- ask
  case getOne $ accounts @= accountId of
    Nothing      -> return (Fail "USER NOT EXISTED.")
    Just account -> return (Success (userInfo account))

authenticate :: (Email, Password) -> Query BBQ (MaybeFail AccountId)
authenticate (email, providedPassword) = do
  bbq@BBQ{..} <- ask
  case getOne $ accounts @= email of
    Nothing      -> return (Fail "USER NOT EXISTED.")
    Just account -> 
      if password account == providedPassword
        then return (Success (accountId account))
        else return (Fail "AUTH FAILED")

listByEmail :: Query BBQ (MaybeFail [Account])
listByEmail = do
  BBQ{..} <- ask
  let accounts' = IxSet.toDescList (Proxy :: Proxy Email) accounts
  return (Success accounts')

$(makeAcidic ''BBQ
  [ 'newAccount
  , 'resetPassword
  , 'updateUserInfo
  , 'getAccountId
  , 'getUserInfo
  , 'authenticate
  , 'listByEmail
  ])
