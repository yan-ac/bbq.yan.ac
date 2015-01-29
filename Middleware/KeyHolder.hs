module Middleware.KeyHolder where

import Control.Applicative
import Data.Time       ( formatTime )
import Data.Time.Clock ( getCurrentTime )
import System.Locale   ( defaultTimeLocale )
import System.Random

import Data.VCodePool

newtype Second = Second { unSecond :: Int }
  deriving (Eq, Ord, Show)

getCurrentTimeInSecond :: IO Int
getCurrentTimeInSecond = (read <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime) :: IO Int

expireIn :: Second -> IO ExpireTime
expireIn ttl = do
  let ttl' = unSecond ttl
  now <- getCurrentTimeInSecond
  return (ExpireTime (now + ttl'))

getNextVCode :: IO VCode
getNextVCode = do
  num <- getStdRandom (randomR (100000000 :: Int, 999999999 :: Int))
  let str = show num
  return (VCode str)
