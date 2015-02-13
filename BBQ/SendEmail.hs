{-# LANGUAGE OverloadedStrings, CPP #-}
module BBQ.SendEmail where

import Data.Text
import Data.Accounts (Email(..))
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL

--import Data.String.UTF8

--import Network.Mail.SMTP

#ifndef PRODUCTION
sendNotification :: Email -> Text -> Text -> IO ()
sendNotification (Email recipent) title content = do
  putStrLn recipent
  T.putStrLn title
  T.putStrLn content

#else
sendNotification :: Email -> Text -> Text -> IO ()
sendNotification (Email recipent) title content = do
  let recipent' = T.pack recipent
  let title' = T.pack title
  let content' = TL.pack content
  sendMailWithLogin' "localhost" 587 "testuser" "testpassword" $ simpleMail
    (Address (Just "Yan.ac 消息通知") "notifications@yan.ac")
    [Address Nothing recipent'] [] [] title' [plainTextPart content']
#endif
