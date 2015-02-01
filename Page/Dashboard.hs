{-# LANGUAGE OverloadedStrings #-}
module Page.Dashboard (entry) where

import Happstack.Server
import Data.RequestState
import Layout.Basic
import Acid.BBQ
import Data.BBQ

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

entry = dir "dashboard" $  do
  nullDir
  method GET
  authResult <- askAuthResult
  case authResult of
    Nothing -> seeOther ("/login" :: String) (toResponse ("请先登录" :: String))
    Just id -> do
      template <- loadTmplWithAuth basicTemplate
      account' <- query $ GetAccount id
      case account' of
        Left errMsg   -> internalServerError $ simpleResponse template "内部服务器错误，请通过电子邮件联系我们。"
        Right account -> ok $ toResponse $ template "个人中心" (mkDashboardPage account)

mkDashboardPage :: Account -> H.Html
mkDashboardPage account = do
  let (Email email')        = email        account
  let (AccountId id)        = accountId    account
  let (name, school, grade) = personalInfo account
  H.h1 $ do "个人信息"
  H.table ! A.class_ "table" $ do
    H.thead $ do H.tr $ do mapM_ stringToCell ["邮箱", "ID", "姓名", "学校", "年级"]
    H.tbody $ do H.tr $ do mapM_ stringToCell [email', show id, name, school, grade]
  H.p $ do "比赛将于二月十四日开始，请耐心等待。"

stringToCell :: String -> H.Html
stringToCell str = do H.td $ do H.toHtml str
