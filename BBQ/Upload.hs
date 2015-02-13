{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

module BBQ.Upload where

import Happstack.Server
import Web.Routes              (RouteT, showURL)
import Control.Monad.Trans.Either
import Control.Monad.Trans     (MonadIO(..), lift)
import Text.Hamlet
import System.FilePath         ((</>))
import System.Directory

import BBQ.JSOrder
import BBQ.Sitemap
import BBQ.Common

import Data.ValidatableForm
import Data.Accounts
import Data.RecordPool
import Data.Sheets
import Data.AppConfig

upload :: AccountId -> ProblemId -> RouteT Sitemap App Response
upload aid pid = do
  quota <- lift $ query $ GetRemainQuota aid
  if quota == 0
  then forbidden $ toJSResponse $ JSOrderError "已达上传上限"
  else do
    decodeBody (defaultBodyPolicy "tmp" 1000000 1000 1000)
    sid <- lift $ update $ GetNextSheetId
    (path, _, _) <- lookFile "sheet"
    let filename = show $ unSheetId sid
    liftIO $ renameFile path $ "sheets" </> filename
    lift $ update $ AppendSheet aid pid sid
    ok $ toJSResponse $ JSOrderOK "上传成功"

viewSheets :: AccountId -> ProblemId -> RouteT Sitemap App Response
viewSheets aid pid = do
  sheets <- lift $ query $ ListSheets aid pid
  routeFn <- askRouteFn'

  ok $ toResponse $ siteLayout "已提交解答" ([hamlet|
    $forall sid <- sheets
      <img src=@{ViewASheet pid sid}>
    |]) routeFn

viewASheet :: AccountId -> ProblemId -> SheetId -> RouteT Sitemap App Response
viewASheet aid pid sid = do
  sheets <- lift $ query $ ListSheets aid pid
  if not $ sid `elem` sheets
  then forbidden $ toResponse ("偷看别人的解答是不好的哦" :: String)
  else do
    let path = "sheets" </> (show $ unSheetId sid)
    serveFile (asContentType "image/jpeg") path
