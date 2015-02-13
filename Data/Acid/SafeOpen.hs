{-# LANGUAGE FlexibleContexts #-}

module Data.Acid.SafeOpen where

import Control.Exception.Lifted    (bracket)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans         (MonadIO(..))
import Data.Acid
import Data.Acid.Local
import Data.Data                   (Typeable)

withLocalState
  :: ( MonadBaseControl IO m
     , MonadIO m
     , IsAcidic st
     , Typeable st
     ) =>
     FilePath
  -> st
  -> (AcidState st -> m a)
  -> m a
withLocalState path initialState =
  bracket (liftIO $ open initialState)
          (liftIO . createCheckpointAndClose)
  where
    open = openLocalStateFrom path
