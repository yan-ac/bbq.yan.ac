{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Data.MaybeFail where

import Data.Data     ( Data, Typeable )
import Data.SafeCopy ( base, deriveSafeCopy )

data MaybeFail a =
    Success a
  | Fail String
  deriving(Eq, Data, Typeable)

instance (Show a) => Show (MaybeFail a) where
  show (Success result) = "1 " ++ (show result)
  show (Fail error)    = "0 " ++ error

$(deriveSafeCopy 0 'base ''MaybeFail)
