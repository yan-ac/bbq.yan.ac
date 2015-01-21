{-# LANGUAGE DeriveDataTypeable, FlexibleContexts
  , GeneralizedNewtypeDeriving, MultiParamTypeClasses
  , ScopedTypeVariables, FlexibleInstances #-}

module Data.RequestState
  ( RequestState, Handler
  , mkRequestState
  , runHandler
  , query, update
  , askAuthResult
  , askBasicTemplate, askPlainTemplate, askLoginTemplate
  )
 where

import Control.Applicative  (Applicative, Alternative, (<$>))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad        (MonadPlus, mplus, msum)
import Control.Monad.Reader (MonadReader, ReaderT(..), ask)
import Control.Monad.State  (get, put)
import Control.Monad.Trans  (MonadIO(..))
import Data.Acid     hiding (query, update)
import Data.Acid.Advanced   (query', update')
import Data.Data            (Data, Typeable)
import Data.Text.Lazy       (Text)
import qualified Text.Blaze.Html5 as H
import Happstack.Server

import Layout.Basic
import Data.BBQ
import Data.VCodePool

data RequestState = RequestState
  { bbqState       :: AcidState BBQ
  , vcodePoolState :: AcidState VCodePool
  , authResult     :: Maybe AccountId
  , basicTemplate  :: (String -> H.Html -> H.Html)
  , plainTemplate  :: (String -> H.Html -> H.Html)
  , loginTemplate  :: (String -> H.Html -> H.Html)
  }

mkRequestState
  :: AcidState BBQ
  -> AcidState VCodePool
  -> Maybe AccountId
  -> RequestState
mkRequestState bbq pool auth = 
  RequestState {
    bbqState       = bbq
  , vcodePoolState = pool
  , authResult     = auth
  , basicTemplate  = mkBasicTemplate auth
  , plainTemplate  = mkBasicTemplate Nothing
  , loginTemplate  = mkBasicTemplate (Just 1)
  }

newtype Handler a = Handler { unHandler :: ServerPartT (ReaderT RequestState IO) a }
  deriving ( Functor, Alternative, Applicative, Monad
           , MonadPlus, MonadIO, HasRqData, ServerMonad
           , WebMonad Response, FilterMonad Response
           , Happstack, MonadReader RequestState
           )

runHandler :: RequestState -> Handler a -> ServerPartT IO a
runHandler request (Handler sp) =
  mapServerPartT (flip runReaderT request) sp

-- Acid --
class HasAcidState m st where
   getAcidState :: m (AcidState st)

instance HasAcidState Handler BBQ where
  getAcidState = bbqState       <$> ask

instance HasAcidState Handler VCodePool where
  getAcidState = vcodePoolState <$> ask

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

-- AuthResult --

askAuthResult :: Handler (Maybe AccountId)
askAuthResult = authResult <$> ask

-- Template   --

askBasicTemplate :: Handler (String -> H.Html -> H.Html)
askBasicTemplate = basicTemplate <$> ask

askPlainTemplate :: Handler (String -> H.Html -> H.Html)
askPlainTemplate = plainTemplate <$> ask

askLoginTemplate :: Handler (String -> H.Html -> H.Html)
askLoginTemplate = loginTemplate <$> ask
