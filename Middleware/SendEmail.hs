{-# LANGUAGE OverloadedStrings #-}
module Middleware.SendEmail where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.Mail.SMTP

{-
sendNotification :: String -> String -> String -> IO ()
sendNotification recipent title content = do
  let recipent' = T.pack recipent
  let title' = T.pack title
  let content' = TL.pack content
  sendMailWithLogin' "localhost" 587 "testuser" "testpassword" $ simpleMail
    (Address (Just "Yan.ac 消息通知") "notifications@yan.ac")
    [Address Nothing recipent'] [] [] title' [plainTextPart content']
-}

sendNotification :: String -> String -> String -> IO ()
sendNotification recipent title content = do
  putStrLn $ "Recipent: " ++ recipent
  putStrLn $ "Title:    " ++ title
  putStrLn $ "Content:"
  putStrLn content
