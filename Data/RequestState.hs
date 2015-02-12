{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}

module Data.RequestState where

import Control.Applicative     (Applicative, Alternative, (<$>))
import Control.Monad           (MonadPlus)
import Control.Monad.Reader    (MonadReader, ReaderT(..), ask)
import Control.Monad.Trans     (MonadIO(..))
import Data.Acid        hiding (query, update)
import Data.Acid.Advanced      (query', update')
import Data.Data               (Data, Typeable)
import Happstack.Server

import Data.Account
import Data.VCodePool

data RequestState = RequestState
  { accountState :: AcidState Accounts
  , recordPools  :: RecordPools
  }

-- I may introduce new features to this factory. --
mkRequestState
  :: AcidState Accounts
  -> RecordPools
  -> RequestState
mkRequestState = RequestState

newtype Handler a = Handler { unHandler :: ServerPartT (ReaderT RequestState IO) a }
  deriving ( Functor, Alternative, Applicative, Monad
           , MonadPlus, MonadIO, HasRqData, ServerMonad
           , WebMonad Response, FilterMonad Response
           , Happstack, MonadReader RequestState
           )

runHandler :: RequestState -> Handler a -> ServerPartT IO a
runHandler request (Handler sp) =
  mapServerPartT (flip runReaderT request) sp

-- Acid.Accounts and Acid.Participation --
class HasAcidState m st where
   getAcidState :: m (AcidState st)

instance HasAcidState Handler Accounts where
  getAcidState = accountState <$> ask

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
askNewRecord :: RecordKey -> Handler VCode
askNewRecord key = do
  pools <- recordPools <$> ask
  liftIO $ insertNewRecord pools key
  

queryRecord :: PoolType -> VCode -> Handler (Maybe RecordKey)
queryRecord t vcode = do
  pools  <- recordPools <$> ask
  liftIO $ getKeyFromRecord pools t vcode

deleteRecord :: PoolType -> VCode -> Handler ()
deleteRecord t vcode = do
  pools  <- recordPools <$> ask
  liftIO $ removeRecord pools t vcode
