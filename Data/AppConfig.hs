{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}

module Data.AppConfig where

import Control.Applicative     (Applicative, Alternative, (<$>), (<*>))
import Control.Monad           (MonadPlus)
import Control.Monad.Reader    (MonadReader, ReaderT(..), ask)
import Control.Monad.Trans     (MonadIO(..))
import Data.Acid        hiding (query, update)
import Data.Acid.Advanced      (query', update')
import Data.Data               (Data, Typeable)
import Happstack.Server
import Happstack.Server.RqData (getDataFn)

import Data.Accounts
import Data.RecordPool
import Data.Sheets

data AppConfig = AppConfig
  { accountsState :: AcidState Accounts
  , recordPools   :: RecordPools
  , sheetsState   :: AcidState Sheets
  }

-- I may introduce new features to this factory. --
mkRequestState
  :: AcidState Accounts
  -> RecordPools
  -> AcidState Sheets
  -> AppConfig
mkRequestState = AppConfig

newtype App a = App { unApp :: ServerPartT (ReaderT AppConfig IO) a }
  deriving ( Functor, Alternative, Applicative, Monad
           , MonadPlus, MonadIO, HasRqData, ServerMonad
           , WebMonad Response, FilterMonad Response
           , Happstack, MonadReader AppConfig
           )

runApp :: AppConfig -> App a -> ServerPartT IO a
runApp request (App sp) =
  mapServerPartT (flip runReaderT request) sp

-- Acid.Accounts and Acid.Participation --
class HasAcidState m st where
   getAcidState :: m (AcidState st)

instance HasAcidState App Accounts where
  getAcidState = accountsState <$> ask

instance HasAcidState App Sheets   where
  getAcidState = sheetsState   <$> ask

query :: forall event m.
         ( Functor m
         , MonadIO m
         , QueryEvent event
         , HasAcidState m (EventState event)
         ) =>
         event
      -> m (EventResult event)
query event =
    do as <- getAcidState
       query' (as :: AcidState (EventState event)) event

update :: forall event m.
          ( Functor m
          , MonadIO m
          , UpdateEvent event
          , HasAcidState m (EventState event)
          ) =>
          event
       -> m (EventResult event)
update event =
    do as <- getAcidState
       update' (as :: AcidState (EventState event)) event

-- RecordPools --
askNewRecord :: RecordKey -> App VCode
askNewRecord key = do
  pools <- recordPools <$> ask
  liftIO $ insertNewRecord pools key
  

queryRecord :: PoolType -> VCode -> App (Maybe RecordKey)
queryRecord t vcode = do
  pools  <- recordPools <$> ask
  liftIO $ getKeyFromRecord pools t vcode

deleteRecord :: PoolType -> VCode -> App ()
deleteRecord t vcode = do
  pools  <- recordPools <$> ask
  liftIO $ removeRecord pools t vcode

setAuthCookie :: AccountId -> App ()
setAuthCookie id = do
  (VCode cookie) <- askNewRecord $ CookieAccountId id
  let (AccountId id') = id
  addCookie Session (mkCookie "accountId" (show id'))
  addCookie Session (mkCookie "accessKey" cookie)

renewCookie' :: AccountId -> VCode -> App ()
renewCookie' id vcode = do
  etime <- expireIn' 900
  pools <- recordPools <$> ask
  liftIO $ renewCookie pools id vcode etime

-- AuthResult --
tryObtainAuthCookies :: RqData (String, String)
tryObtainAuthCookies =
    (,) <$> lookCookieValue "accountId" <*> lookCookieValue "accessKey"

expireIn' :: Int -> App ExpireTime
expireIn' ttl = do 
  liftIO $ expireIn $ ExpireTime ttl

askUserStat :: App (Maybe (AccountId, BBQStatus))
askUserStat = do
  c <- getDataFn tryObtainAuthCookies
  case c of
    Left _ -> return Nothing
    Right (givenId, key) -> do
      let givenId' = AccountId ((read givenId) :: Int)
      actualId <- queryRecord CookiePool $ VCode key
      case actualId of
        Nothing   -> return Nothing
        Just (CookieAccountId actualId') -> do
          if givenId' /= actualId'
          then return Nothing
          else do
            now <- expireIn' 0
            status <- query $ GetParticipantStatus actualId' now
            renewCookie' actualId' (VCode key)
            return $ Just (actualId', status)
