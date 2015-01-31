{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Data.ValidatableForm where

import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.State

import Data.Text (Text(..))
import Data.BBQ (Email(..), Password(..))

import Happstack.Server
import Data.RequestState

import Text.Email.Validate (isValid)
import Data.ByteString.Char8 (pack)

type Hint  = Text
type Field = String

data FormItem =
    EmailItem    Field Hint
  | PasswordItem Field Hint
  | TextItem     Field Hint
  | Button       Field Hint
  | SelectItem   Field Hint [(String, String)]

insertItem :: (String -> a) -> FormItem -> State [FormItem] a
insertItem cons item = do
  form <- get
  let form' = form ++ [item]
  put form'
  return $ cons ""

class TypesafeForm m where
  askEmail    :: String -> Text -> m Email
  askPassword :: String -> Text -> m Password
  askText     :: String -> Text -> m String
  addButton   :: String -> Text -> m ()
  askChoice   :: String -> Text -> [(String, String)] -> m String
  should      :: Bool -> Maybe String -> m ()

instance TypesafeForm (State [FormItem]) where
  askEmail    f h = insertItem Email       (EmailItem f h)
  askPassword f h = insertItem Password (PasswordItem f h)
  askText     f h = insertItem id           (TextItem f h)
  addButton   f h = insertItem (\x -> ())     (Button f h)
  askChoice f h c = insertItem id       (SelectItem f h c)
  should _ _ = return ()

getFormItems :: State [FormItem] a -> [FormItem]
getFormItems form = items
  where (_, items) = runState form ([]::[FormItem])

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
  addButton _ _ = return ()
  askChoice field _ choices = do
    choice <- lift $ body $ look field
    let choices' = [x | (x, _) <- choices]
    if choice `elem` choices'
      then right choice
      else left "请在给定的选择范围内选择"
  should cond errMsg = do
    if cond then right ()
            else left msg
    where msg = case errMsg of Just x  -> x
                               Nothing -> "不正确的参数"

runForm form = do
  runEitherT form
