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

import System.FilePath         ((</>))
import Data.Acid.SafeOpen

newtype ProblemId = ProblemId { unProblemId :: Int }
  deriving (Eq, Ord, Data, Typeable, Read, Show)
newtype SheetId = SheetId { unSheetId :: Int}
  deriving (Eq, Ord, Data, Typeable, Read, Show)
$(deriveSafeCopy 0 'base ''ProblemId)
$(deriveSafeCopy 0 'base ''SheetId)

data Sheets = Sheets {
    nextSheetId :: SheetId
  , getSheetSet :: Map AccountId (Map ProblemId [SheetId])
  , getQuota    :: Map AccountId Int
  , getEndTime  :: Map AccountId ExpireTime
  } deriving (Eq, Ord, Data, Typeable, Show)
$(deriveSafeCopy 0 'base ''Sheets)

data BBQStatus = BBQNotStarted | BBQInProgress | BBQFinished
  deriving (Eq, Ord, Data, Typeable, Read, Show)
$(deriveSafeCopy 0 'base ''BBQStatus)

initialSheetSet = Map.fromList [(ProblemId p, []) | p <- [1..9]]

startBBQ :: AccountId -> ExpireTime -> Update Sheets ()
startBBQ id etime = do
  pool <- get
  put $ pool {
    getSheetSet = Map.insert id initialSheetSet $ getSheetSet pool
  , getQuota    = Map.insert id 45              $    getQuota pool
  , getEndTime  = Map.insert id etime           $  getEndTime pool
  }

getNextSheetId :: Update Sheets SheetId
getNextSheetId = do
  pool <- get
  let thisId = nextSheetId pool
  let nextId = SheetId $ 1 + unSheetId thisId
  put $ pool { nextSheetId = nextId }
  return thisId

appendSheet :: AccountId -> ProblemId -> SheetId -> Update Sheets ()
appendSheet aid pid sid = do
  pool <- get
  let Just quota = Map.lookup aid $ getQuota pool
  let Just set   = Map.lookup aid $ getSheetSet pool
  let Just list  = Map.lookup pid set
  let list' = list ++ [sid]
  let set'  = Map.insert pid list' set
  put $ pool {
      getSheetSet = Map.insert aid set'        $ getSheetSet pool
    , getQuota    = Map.insert aid (quota - 1) $ getQuota pool
    }

listSheets :: AccountId -> ProblemId -> Query Sheets [SheetId]
listSheets aid pid = do
  pool <- ask
  let Just set  = Map.lookup aid $ getSheetSet pool
  let Just list = Map.lookup pid set
  return list

getRemainQuota :: AccountId -> Query Sheets Int
getRemainQuota aid = do
  pool <- ask
  let Just quota = Map.lookup aid $ getQuota pool
  return quota

getParticipantStatus :: AccountId -> ExpireTime -> Query Sheets BBQStatus
getParticipantStatus id now = do
  pool   <- ask
  let etime' = Map.lookup id $ getEndTime pool
  case etime' of
    Nothing -> return BBQNotStarted
    Just et -> if et > now
               then return BBQInProgress
               else return BBQFinished

returnAll :: Query Sheets (Map AccountId (Map ProblemId [SheetId]))
returnAll = getSheetSet <$> ask

$(makeAcidic ''Sheets
  [ 'startBBQ
  , 'getParticipantStatus
  , 'getRemainQuota
  , 'getNextSheetId
  , 'appendSheet
  , 'listSheets
  , 'returnAll
  ])

initialSheetsState = Sheets {
    nextSheetId = SheetId 11797
  , getSheetSet = Map.fromList []
  , getQuota    = Map.fromList []
  , getEndTime  = Map.fromList []
}

openSheetsState basePath action =
  let path = basePath </> "Sheets"
      initial = initialSheetsState
  in  withLocalState path initial $ \st ->
        action st
