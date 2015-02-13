module BBQ.JSOrder where

import Happstack.Server
import Data.Text (Text(..), unpack)

data JSOrder = JSOrderOK       String
             | JSOrderError    String
             | JSOrderRedirect Text

instance Show JSOrder where
  show order = case order of
    JSOrderOK       str -> "OK"    ++ str
    JSOrderError    str -> "ERROR" ++ str
    JSOrderRedirect url -> "REDIRECT" ++ unpack url

toJSResponse :: JSOrder -> Response
toJSResponse = toResponse . show
