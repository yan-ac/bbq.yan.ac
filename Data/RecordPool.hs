{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}

module Data.RecordPool
  ( ExpireTime(..), VCode(..)
  , RecordKey(..), PoolType(..)
  , insertNewRecord, getKeyFromRecord, removeRecord
  , expireIn
  , RecordPools(..)
  , renewCookie
  , openRecordPools
  )
  where

import Data.Data               (Data, Typeable)
import Data.SafeCopy           (base, deriveSafeCopy, SafeCopy(..))

import Control.Applicative     ((<$>))
import Control.Monad.Reader    (ask)
import Control.Monad.State     (get, put)
import Data.Acid
import Data.Acid.Local
import Data.Acid.Advanced

import Data.IxSet
    ( Indexable, IxSet(..), ixFun, ixSet
    , empty, (@=), getOne
    )
import qualified Data.IxSet as Ix

import System.Random
import Data.Time               (formatTime)
import Data.Time.Clock         (getCurrentTime)
import System.Locale           (defaultTimeLocale)

import Data.Accounts

import System.FilePath         ((</>))
import Data.Acid.SafeOpen

newtype ExpireTime = ExpireTime Int
  deriving (Eq, Ord, Data, Typeable, Show)
newtype VCode = VCode { unVCode :: String}
  deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''ExpireTime)
$(deriveSafeCopy 0 'base ''VCode)

data Record k = Record {
    getKey    :: k
  , getVCode  :: VCode
  , getETime  :: ExpireTime
  } deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''Record)

instance (Typeable k, Ord k) => Indexable (Record k) where
  empty = ixSet
    [ ixFun $ \bp -> [getKey bp]
    , ixFun $ \bp -> [getVCode bp]
    ]

type RecordPool k = IxSet (Record k)

newRecord :: (Typeable k, Ord k)
  => k -> VCode -> ExpireTime -> Update (RecordPool k) ()
newRecord key vcode etime = do
  let record = Record key vcode etime
  pool' <- Ix.updateIx key record <$> get
  put pool'

validateRecord :: (Typeable k, Ord k)
  => VCode -> ExpireTime -> Query (RecordPool k) (Maybe k)
validateRecord vcode now = do
  pool <- ask
  case getOne $ pool @= vcode of
    Nothing  -> return Nothing
    Just Record { getKey = key, getETime = etime } ->
      if etime < now
      then return Nothing
      else return $ Just key

removeRecord' :: (Typeable k, Ord k)
  => VCode -> Update (RecordPool k) ()
removeRecord' vcode = do
  pool' <- Ix.deleteIx vcode <$> get
  put pool'

-- Template Haskell of Acid State breaks here.   --
-- Writing those types manually here.            --
-- Should use makeAcidic when this bug is fixed. --
data NewRecord k = NewRecord k VCode ExpireTime
  deriving (Typeable)
$(deriveSafeCopy 0 'base ''NewRecord)
instance (Typeable k, SafeCopy k) => Method (NewRecord k) where
  type MethodResult (NewRecord k) = ()
  type MethodState  (NewRecord k) = RecordPool k
instance (Typeable k, SafeCopy k) => UpdateEvent (NewRecord k)

data ValidateRecord k = ValidateRecord VCode ExpireTime
  deriving (Typeable)
$(deriveSafeCopy 0 'base ''ValidateRecord)
instance (Typeable k, SafeCopy k) => Method (ValidateRecord k) where
  type MethodResult (ValidateRecord k) = Maybe k
  type MethodState  (ValidateRecord k) = RecordPool k
instance (Typeable k, SafeCopy k) => QueryEvent (ValidateRecord k)

data RemoveRecord k = RemoveRecord VCode
  deriving (Typeable)
$(deriveSafeCopy 0 'base ''RemoveRecord)
instance (Typeable k, SafeCopy k) => Method (RemoveRecord k) where
  type MethodResult (RemoveRecord k) = ()
  type MethodState  (RemoveRecord k) = RecordPool k
instance (Typeable k, SafeCopy k) => UpdateEvent (RemoveRecord k)

instance (Typeable k, SafeCopy k, Ord k) => IsAcidic (RecordPool k) where
  acidEvents = [ UpdateEvent (\(NewRecord key vcode etime) -> newRecord key  vcode etime)
               , QueryEvent  (\(ValidateRecord vcode now)  -> validateRecord vcode now)
               , UpdateEvent (\(RemoveRecord vcode)        -> removeRecord'  vcode)
               ]

-- Helper Functions --
getNextVCode :: IO VCode
getNextVCode = VCode . show <$> getStdRandom (randomR (100000000000000 :: Integer, 999999999999999 :: Integer))

expireIn :: ExpireTime -> IO ExpireTime
expireIn (ExpireTime ttl) = do
  now <- (read <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime) :: IO Int
  return $ ExpireTime $ now + ttl

data RecordPools = RecordPools {
    getNewAccountPool :: AcidState (RecordPool Email)
  , getResetPswdPool  :: AcidState (RecordPool Email)
  , getCookiePool     :: AcidState (RecordPool AccountId)
  }

data RecordKey = NewAccountEmail Email
               | ResetPswdEmail  Email
               | CookieAccountId AccountId

data PoolType = NewAccountPool
              | ResetPswdPool
              | CookiePool

insertNewRecord :: RecordPools -> RecordKey -> IO VCode
insertNewRecord pools key = do
  vcode <- getNextVCode
  case key of
    NewAccountEmail key' -> do
      record <- fmap (NewRecord key' vcode) (expireIn $ ExpireTime 3600)
      update' (getNewAccountPool pools) record
    ResetPswdEmail  key' -> do
      record <- fmap (NewRecord key' vcode) (expireIn $ ExpireTime 3600)
      update' (getResetPswdPool  pools) record
    CookieAccountId key' -> do
      record <- fmap (NewRecord key' vcode) (expireIn $ ExpireTime 900)
      update' (getCookiePool     pools) record
  return vcode

getKeyFromRecord :: RecordPools -> PoolType -> VCode -> IO (Maybe RecordKey)
getKeyFromRecord pools t vcode = do
  now <- expireIn $ ExpireTime 0
  case t of
    NewAccountPool -> do
      result <- query' (getNewAccountPool pools) $ ValidateRecord vcode now
      case result of
        Nothing -> return Nothing
        Just x  -> return $ Just $ NewAccountEmail x
    ResetPswdPool  -> do
      result <- query' (getResetPswdPool  pools) $ ValidateRecord vcode now
      case result of
        Nothing -> return Nothing
        Just x  -> return $ Just $ ResetPswdEmail  x
    CookiePool     -> do
      result <- query' (getCookiePool     pools) $ ValidateRecord vcode now
      case result of
        Nothing -> return Nothing
        Just x  -> return $ Just $ CookieAccountId x

removeRecord :: RecordPools -> PoolType -> VCode -> IO ()
removeRecord pools t vcode = do
  case t of
    NewAccountPool -> update' (getNewAccountPool pools) $ RemoveRecord vcode
    ResetPswdPool  -> update' (getResetPswdPool  pools) $ RemoveRecord vcode
    CookiePool     -> update' (getCookiePool     pools) $ RemoveRecord vcode


renewCookie :: RecordPools -> AccountId -> VCode -> ExpireTime -> IO ()
renewCookie pools id vcode etime = do
  let pool = getCookiePool pools
  update' pool $ NewRecord id vcode etime

-- I/O --
openRecordPools basePath action =
  let path1 = basePath </> "Pools" </> "NewAccount"
      path2 = basePath </> "Pools" </> "ResetPswd"
      path3 = basePath </> "Pools" </> "Cookie"
  in withLocalState path1 initialEmailPool     $ \st1 ->
     withLocalState path2 initialEmailPool     $ \st2 ->
     withLocalState path3 initialAccountIdPool $ \st3 ->
       action $ RecordPools st1 st2 st3

initialEmailPool :: RecordPool Email
initialEmailPool = empty
initialAccountIdPool :: RecordPool AccountId
initialAccountIdPool = empty
