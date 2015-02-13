module BBQ.Common
  ( (<>)
  , thenThrowError
  , elseThrowError
  , extractEither
  )
  where

import qualified Data.Text
import Control.Monad.Trans.Either

thenThrowError cond err = if cond then left err else right ()
elseThrowError cond err = if cond then right () else left err

extractEither e = case e of
  Left a  -> left a
  Right b -> right b

(<>) = Data.Text.append
