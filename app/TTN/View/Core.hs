{-# LANGUAGE OverloadedStrings #-}

module TTN.View.Core where

import TTN.Model.Core

import Data.Text                        ( Text )
import Data.Text.Lazy                   ( toStrict )
import Lucid
import Text.Digestive.View

import qualified Text.Digestive.Lucid.Html5 as DL
import qualified Web.Spock as S

-- Some useful type aliases

type Token = Text

type FormRenderer = Token -> View (Html ()) -> Html ()

-- Some higher level layout functions

lucid :: Html () -> TTNAction ctx a
lucid = S.html . toStrict . renderText

pageTemplate :: Html () -> Html ()
pageTemplate contents = html_ (do head_ (title_ "Translate the News")
                                  body_ contents)

errorPage :: Html () -> Html ()
errorPage = pageTemplate

renderPage :: Html () -> TTNAction ctx a
renderPage = lucid . pageTemplate

renderSimpleStr :: String -> TTNAction ctx a
renderSimpleStr msg = renderPage $ toHtml msg

renderSimpleForm :: FormRenderer -> Token -> View Text -> TTNAction ctx a
renderSimpleForm renderer tok view = lucid . renderer tok $ fmap toHtml view

-- Some functions for form views

constructView :: (Text -> View (Html ()) -> Html ())
              -> Text
              -> Text
              -> View (Html ())
              -> Html ()
constructView f ref lbl view = p_ (do DL.label ref view $ toHtml lbl
                                      f ref view
                                      DL.errorList ref view)

inputText_ :: Text -> Text -> View (Html ()) -> Html ()
inputText_ = constructView DL.inputText

inputTextArea_ :: Maybe Int -> Maybe Int -> Text -> Text -> View (Html ()) -> Html ()
inputTextArea_ r c = constructView $ DL.inputTextArea r c

inputPass_ :: Text -> Text -> View (Html ()) -> Html ()
inputPass_ = constructView DL.inputPassword

submit :: Text -> Html ()
submit value = p_ $ input_ [type_ "submit", value_ value]

csrf :: Token -> Html ()
csrf tok = input_ [name_ "__csrf_token", type_ "hidden", value_ tok]

