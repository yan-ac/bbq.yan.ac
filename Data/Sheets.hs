{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Data.Sheets where

import Data.Data               (Data, Typeable)
import Data.SafeCopy           (base, deriveSafeCopy)

import Control.Applicative     ((<$>))
import Control.Monad.Reader    (ask)
import Control.Monad.State     (get, put)
import Data.Acid               (Query, Update, makeAcidic)

import Data.Map.Strict as Map  (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List

import Data.Accounts
import Data.RecordPool

newtype ProblemId = ProblemId Int
  deriving (Eq, Ord, Data, Typeable, Show)
newtype SheetId = SheetId Int
  deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''ProblemId)
$(deriveSafeCopy 0 'base ''SheetId)

data Artwork = Artwork [SheetId] (Map ProblemId [SheetId])
  deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''Artwork)

data BBQStatus = BBQNotStarted | BBQInProgress | BBQFinished
  deriving (Eq, Ord, Data, Typeable, Show)
data Participant = Participant ExpireTime Artwork
  deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''BBQStatus)
$(deriveSafeCopy 0 'base ''Participant)

type Participants = Map AccountId Participant

initialArtwork :: Artwork
initialArtwork = Artwork [] $ Map.fromList [(ProblemId x, []) | x <- [1001..1009]]

insertSheet' :: Artwork -> ProblemId -> SheetId -> Artwork
insertSheet' (Artwork ss ps) pid sid = Artwork (ss ++ [sid]) (Map.insert pid (l ++ [sid]) ps)
  where (Just l) = Map.lookup pid ps

deleteSheet' :: Artwork -> ProblemId -> SheetId -> Artwork
deleteSheet' (Artwork ss ps) pid sid = Artwork ss $ Map.insert pid (List.delete sid l) ps
  where (Just l) = Map.lookup pid ps

listSheets' :: Artwork -> ProblemId -> [SheetId]
listSheets' (Artwork ss ps) pid = l
  where (Just l) = Map.lookup pid ps

startBBQ' :: Participants -> AccountId -> ExpireTime -> Participants
startBBQ' pool id etime = Map.insert id participant pool
  where participant = Participant etime $ initialArtwork

getParticipantStatus' :: Participants -> AccountId -> ExpireTime -> BBQStatus
getParticipantStatus' pool id now =
  case Map.lookup id pool of
    Nothing -> BBQNotStarted
    Just (Participant etime _) ->
      if etime > now
      then BBQInProgress
      else BBQFinished

startBBQ :: AccountId -> ExpireTime -> Update Participants ()
startBBQ id etime = do
  pool <- get
  put $ startBBQ' pool id etime

getParticipantStatus :: AccountId -> ExpireTime -> Query Participants BBQStatus
getParticipantStatus id now = do
  pool <- ask
  return $ getParticipantStatus' pool id now

updateSheets :: AccountId -> (Artwork -> Artwork) -> Update Participants ()
updateSheets aid f = do
  pool <- get
  let (Just (Participant e a)) = Map.lookup aid pool
  let a' = f a
  let pool' = Map.insert aid (Participant e a') pool
  put $ pool'

uploadSheet :: AccountId -> ProblemId -> SheetId -> Update Participants ()
uploadSheet aid pid sid = updateSheets aid (\a -> insertSheet' a pid sid)

deleteSheet :: AccountId -> ProblemId -> SheetId -> Update Participants ()
deleteSheet aid pid sid = updateSheets aid (\a -> deleteSheet' a pid sid)

sortSheets :: AccountId -> ProblemId -> [SheetId] -> Update Participants Bool
sortSheets aid pid l1 = do
  pool <- get
  let (Just (Participant e (Artwork ss ps))) = Map.lookup aid pool
  let (Just l2) = Map.lookup pid ps
  if foldl (&&) True [elem x l2 | x <- l1]
  then do
    let pool' = Map.insert aid (Participant e (Artwork ss $ Map.insert pid l1 ps)) pool
    put $ pool'
    return True
  else return False

listSheets :: AccountId -> ProblemId -> Query Participants [SheetId]
listSheets aid pid = do
  pool <- ask
  let (Just (Participant e a)) = Map.lookup aid pool
  return $ listSheets' a pid

$(makeAcidic ''Participants
  [ 'startBBQ
  , 'getParticipantStatus
  , 'uploadSheet
  , 'deleteSheet
  , 'sortSheets
  , 'listSheets
  ])
