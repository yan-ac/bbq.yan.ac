{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Page.List (showDatabase) where

import Happstack.Server
import Control.Monad
import Control.Monad.Trans
import Data.IxSet
import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Internal (customAttribute, customParent)

import Data.RequestState
import Layout.Basic
import Data.BBQ
import Acid.BBQ
import Data.VCodePool
import Acid.VCodePool
import qualified Config

import Data.Time
import System.Locale
import Data.Time.Clock.POSIX

showDatabase :: Handler Response
showDatabase = msum [
    dir "accounts" $ dir Config.superUserPassword $ do
      list <- query ListByEmail
      ok $ toResponse $ showAccounts list

  , dir "vcodes"   $ dir Config.superUserPassword $ do
      pools <- query GetPools
      ok $ toResponse $ showPools pools
  ]

stringToCell :: String -> H.Html
stringToCell str = do H.td $ do H.toHtml str

showAccount :: Account -> H.Html
showAccount account = do
  let (Email email')        = email        account
  let (AccountId id)        = accountId    account
  let (name, school, grade) = personalInfo account
  H.tr $ do mapM_ stringToCell [email', show id, name, school, grade]

showAccounts accounts = basicTemplate False "账户列表" $ do
  H.table ! A.class_ "table" $ do
    H.thead $ do H.tr $ do mapM_ stringToCell ["邮箱", "ID", "姓名", "学校", "年级"]
    H.tbody $ do mapM_ showAccount accounts

showVCodeRecord :: (Show k) => VCodeRecord k -> H.Html
showVCodeRecord vcodeRecord = do
  let key               = primaryKey vcodeRecord
  let (VCode vcode')    = vcode      vcodeRecord
  let (ExpireTime time) = expireTime vcodeRecord
  let key'   = show key
  let time'  = toInteger time
  let time'' = formatTime defaultTimeLocale  "%c" $ posixSecondsToUTCTime (fromInteger time')
  H.tr $ do mapM_ stringToCell [key', vcode', time'']

showVCodePool :: (Ord k, Show k) => H.Html -> VCodePool k -> H.Html
showVCodePool title ixSet = do
  let pool = toList ixSet
  H.h1 $ do title
  H.table ! A.class_ "table" $ do
    H.thead $ do H.tr $ do mapM_ stringToCell ["主键", "验证码", "过期时间"]
    H.tbody $ do mapM_ showVCodeRecord pool

showPools pools =  do
  basicTemplate False "验证码列表" $ do
    showVCodePool "新账户" $ newAccountPool pools
    showVCodePool "重置密码" $ resetPasswdPool pools
    showVCodePool "Cookie" $ cookiePool pools
