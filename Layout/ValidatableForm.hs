{-# LANGUAGE OverloadedStrings #-}
module Layout.ValidatableForm where

import Layout.Ming
import Data.ValidatableForm
import Control.Monad
import Layout.Basic
import Data.Text (Text)

import           Text.Blaze ((!))
import           Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html (toHtml)
import           Text.Blaze.Internal

toAttr = stringValue

formItemsToHtml :: [FormItem] -> [H.Html]
formItemsToHtml items = map formItemToHtml items

formItemToHtml :: FormItem -> H.Html
formItemToHtml item  = case item of
  EmailItem    f h -> toInputElement "email" f h
  PasswordItem f h -> toInputElement "password" f h
  TextItem     f h -> toInputElement "text" f h
  Button       f t -> H.button ! A.class_ "btn btn-default"
                               ! A.type_ "submit" 
                               ! A.name (toAttr f) $ do toHtml t
  SelectItem f h c -> do
    H.div ! A.class_ "form-group" $ do
      H.label ! A.for (toAttr f) $ do toHtml h
      H.select ! A.name (toAttr f) $ do
        mapM_ (\(name, text) -> H.option ! A.value (toAttr name) $ do toHtml text) c

toInputElement :: String -> String -> Text -> H.Html
toInputElement t f h = do
  H.div ! A.class_ "form-group" $ do
    H.label ! A.for (toAttr f) $ do toHtml h
    H.input ! A.type_ (toAttr t) ! A.class_ "form-control" ! A.name (toAttr f)

formTemplate' :: Bool -> String -> String -> H.Html -> H.Html
formTemplate' auth title url formContent =
  basicTemplate auth title $ do
    H.div ! A.class_ "jumbotron" $ do
      H.h1 $ do toHtml title
    H.div ! A.class_ "row" $ do
      H.div ! A.class_ "col-xs-1 col-sm-3 col-md-4" $ do ""
      H.div ! A.class_ "col-xs-10 col-sm-6 col-md-4" $ do
        H.form ! A.action (toAttr url)
               ! A.method "post" $ do
          formContent

formTemplate :: Bool -> String -> String -> [FormItem] -> H.Html
formTemplate auth title url items =
  formTemplate' auth title url (sequence_ items')
  where items' = formItemsToHtml items
