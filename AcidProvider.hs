{-# LANGUAGE DeriveDataTypeable, FlexibleContexts
  , GeneralizedNewtypeDeriving, MultiParamTypeClasses
  , ScopedTypeVariables, FlexibleInstances #-}

module AcidProvider where
import Control.Applicative  (Applicative, Alternative, (<$>))
import Control.Exception.Lifted    (bracket)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad        (MonadPlus, mplus, msum)
import Control.Monad.Reader ( MonadReader, ReaderT(..), ask)
import Control.Monad.State  (get, put)
import Control.Monad.Trans  (MonadIO(..))
import Data.Acid
import Data.Acid.Local
import Data.Acid.Advanced   (query', update')
import Data.Maybe           (fromMaybe)
import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)
import Data.Data            (Data, Typeable)
import Data.Text.Lazy       (Text)
import Happstack.Server
import System.FilePath      ((</>))

import Data.BBQ
import Acid.BBQ
import Data.VCodePool
import Acid.VCodePool

class HasAcidState m st where
   getAcidState :: m (AcidState st)

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

withLocalState
  :: ( MonadBaseControl IO m
     , MonadIO m
     , IsAcidic st
     , Typeable st
     ) =>
     Maybe FilePath
  -> st
  -> (AcidState st -> m a)
  -> m a
withLocalState mPath initialState =
  bracket (liftIO $ open initialState)
          (liftIO . createCheckpointAndClose)
  where
    open = maybe openLocalState openLocalStateFrom mPath

-- add a state here
data Acid = Acid
  { acidBBQState   :: AcidState BBQ
  , vcodePoolState :: AcidState VCodePool
  }

withAcid :: Maybe FilePath -> (Acid -> IO a) -> IO a
withAcid mBasePath action =
  let basePath = fromMaybe "_state" mBasePath
      -- specify state path
      bbqPath  = Just $ basePath </> "BBQ"
      poolPath = Just $ basePath </> "Pool"
  in withLocalState bbqPath initialBBQState   $ \c ->
     withLocalState poolPath initialVCodePool $ \g ->
       action (Acid c g)

newtype App a = App { unApp :: ServerPartT (ReaderT Acid IO) a }
    deriving ( Functor, Alternative, Applicative, Monad
             , MonadPlus, MonadIO, HasRqData, ServerMonad
             , WebMonad Response, FilterMonad Response
             , Happstack, MonadReader Acid
             )

runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) =
  mapServerPartT (flip runReaderT acid) sp

-- give state extractor
instance HasAcidState App BBQ where
  getAcidState = acidBBQState   <$> ask

instance HasAcidState App VCodePool where
  getAcidState = vcodePoolState <$> ask
