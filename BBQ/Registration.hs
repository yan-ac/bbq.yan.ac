module BBQ.Registration where

import Happstack.Server
import Web.Routes              (RouteT)

import BBQ.Sitemap
import Data.Accounts
import Data.RecordPool
import Data.AppConfig

newRegistration :: RouteT Sitemap App Response
newRegistration = ok $ toResponse "好啊好啊"

registraion :: VCode -> RouteT Sitemap App Response
registraion vcode = ok $ toResponse "好啊好啊"
