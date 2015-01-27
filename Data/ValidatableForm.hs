{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Data.ValidatableForm where

import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.State

import Data.Text (Text)
import Data.BBQ (Email(..), Password(..))

import           Text.Blaze ((!))
import           Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html (toHtml)
import           Text.Blaze.Internal

import Happstack.Server
import Data.RequestState

import Text.Email.Validate (isValid)
import Data.ByteString.Char8 (pack)

toAttr = stringValue

mkFormItemWithInputProvided :: (String -> a) -> String -> Text -> Html -> State [Html] a
mkFormItemWithInputProvided cons field hint input = do
  let fragment = sequence_ [ H.label ! A.for (toAttr field) $ do toHtml hint, input ]
  form <- get
  let form' = form ++ [fragment]
  put form'
  return (cons "")

mkFormItem :: (String -> a) -> AttributeValue -> String -> Text -> State [Html] a
mkFormItem cons type_ field hint = mkFormItemWithInputProvided
  cons field hint ( H.input ! A.type_ type_ ! A.name (toAttr field) )

class TypesafeForm m where
  askEmail    :: String -> Text -> m Email
  askPassword :: String -> Text -> m Password
  askText     :: String -> Text -> m String
  askChoice   :: String -> Text -> [(String, String)] -> m String
  addButton   :: String -> Text -> m ()
  should      :: Bool -> Maybe String -> m ()

instance TypesafeForm (State [Html]) where
  askEmail    = mkFormItem Email "email"
  askPassword = mkFormItem Password "password"
  askText     = mkFormItem id "text"
  askChoice field hint choices = mkFormItemWithInputProvided
    id field hint
    ( do
        H.select ! A.name (toAttr field) $ do
          mapM_ (\(name, text) -> H.option ! A.value (toAttr name) $ do toHtml text)
                choices
    )
  addButton field text = do
    let fragment = H.button ! A.type_ "submit" ! A.name (toAttr field) $ do toHtml text
    form <- get
    let form' = form ++ [fragment]
    put form'
    return ()

  should _ _ = return ()

getHtmlFromValidatableForm :: AttributeValue -> State [Html] a -> Html
getHtmlFromValidatableForm action form = do
  H.form ! A.action action
         ! A.method "post" $ do
    sequence_ items
  where (_, items) = runState form ([]::[Html])

----------------

instance (Monad m, HasRqData m, Functor m) => TypesafeForm (EitherT String m) where
  askEmail field _ = do
    email <- lift $ body $ look field
    if isValid . pack $ email
      then right (Email email)
      else left "错误的邮箱地址"
  askPassword field _ = do
    password <- lift $ body $ look field
    if (length password >= 12) && (length password <= 24)
      then right (Password password)
      else left "密码应当为 12—24 位"
  askText field _ = do
    text <- lift $ body $ look field
    right text
  askChoice field _ choices = do
    choice <- lift $ body $ look field
    let choices' = [x | (x, _) <- choices]
    if choice `elem` choices'
      then right choice
      else left "请在给定的选择范围内选择"
  addButton _ _ = return ()
  should cond errMsg = do
    if cond then right ()
            else left msg
    where msg = case errMsg of Just x  -> x
                               Nothing -> "不正确的参数"

getValidatorFromValidatableForm form = do
  runEitherT form
