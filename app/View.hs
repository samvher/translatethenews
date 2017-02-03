{-# LANGUAGE OverloadedStrings #-}

module View where

import Types

import Data.Text                        ( Text, pack )
import Data.Text.Lazy                   ( toStrict )
import Lucid
import Text.Digestive.Form
import Text.Digestive.View
import Text.Digestive.Lucid.Html5
import Web.Spock                        hiding ( head, text )
import Web.Spock.Digestive

-- Some functions for layout

pageTemplate :: Html () -> Html ()
pageTemplate contents = html_ (do head_ (title_ "Translate the News")
                                  body_ contents)

errorPage :: Html () -> Html ()
errorPage = pageTemplate

lucid :: Html () -> TTNAction ctx a
lucid document = html (toStrict (renderText document))

-- Some functions for form views

viewConstr :: (Text -> View (Html ()) -> Html ())
           -> Text
           -> Text
           -> View (Html ())
           -> Html ()
viewConstr f ref lbl view = p_ (do label ref view $ toHtml lbl
                                   f ref view
                                   errorList ref view)

inputText_ :: Text -> Text -> View (Html ()) -> Html ()
inputText_ = viewConstr inputText

inputTextArea_ :: Maybe Int -> Maybe Int -> Text -> Text -> View (Html ()) -> Html ()
inputTextArea_ r c = viewConstr $ inputTextArea r c

inputPass_ :: Text -> Text -> View (Html ()) -> Html ()
inputPass_ = viewConstr inputPassword

submit :: Text -> Html ()
submit value = p_ $ input_ [type_ "submit", value_ value]

csrf :: Token -> Html ()
csrf tok = input_ [name_ "__csrf_token", type_ "hidden", value_ tok]

-- Pages

hello :: Html ()
hello = pageTemplate $ h1_ "Hello world!"

-- TODO: This should be somewhere else.
serveForm :: Text
          -> Form Text (TTNAction ctx) a
          -> FormRenderer
          -> (a -> TTNAction ctx b)
          -> TTNAction ctx b
serveForm label form renderer successAction = do
    tok       <- getCsrfToken
    (view, l) <- runForm label form
    case l of
      Nothing -> lucid . renderer tok $ fmap toHtml view
      Just x  -> successAction x
