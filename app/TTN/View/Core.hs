{-|
Module      : TTN.View.Core
Description : Site-wide code for templating.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module TTN.View.Core where

import TTN.Model.Core

import Data.Monoid                      ( (<>) )
import Data.Text                        ( Text )
import Data.Text.Lazy                   ( toStrict )
import Lucid
import Text.Digestive.View

import qualified Text.Digestive.Lucid.Html5 as DL
import qualified Web.Spock as S

-- * View type aliases

type Token = Text

type FormRenderer ctx = Token -> View (TTNView ctx ()) -> TTNView ctx ()

-- * Higher level layout functions

lucid :: TTNView ctx () -> TTNAction ctx a
lucid v = S.html . toStrict =<< renderTextT v

pageTemplate :: TTNView ctx () -> TTNView ctx ()
pageTemplate contents = html_ ( htmlHead >> htmlBody contents )

htmlHead :: TTNView ctx ()
htmlHead = head_ $ do
    title_ "Translate the News"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/reset.css"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/style.css"]
    let gFonts = "https://fonts.googleapis.com/css?" <>
          "family=Open+Sans:400,400i|Oswald:300,500&amp;" <>
          "subset=cyrillic,cyrillic-ext,greek,greek-ext," <>
          "latin-ext,vietnamese"
    link_ [href_ gFonts, rel_ "stylesheet"]

htmlBody :: TTNView ctx () -> TTNView ctx ()
htmlBody contents = body_ . div_ [id_ "container"] $ do
    div_ [id_ "header"] . h1_ . a_ [href_ "/"] $ "translatethenews.org"
    div_ [id_ "content-main"] contents

errorPage :: TTNView ctx () -> TTNView ctx ()
errorPage = pageTemplate . div_ [id_ "simple-message"]

renderPage :: TTNView ctx () -> TTNAction ctx a
renderPage = lucid . pageTemplate

renderSimpleStr :: String -> TTNAction ctx a
renderSimpleStr msg = renderPage . div_ [id_ "simple-message"] $ toHtml msg

renderSimpleForm :: FormRenderer ctx -> Token -> View Text -> TTNAction ctx a
renderSimpleForm renderer tok view = lucid . renderer tok $ fmap toHtml view

-- * Functions for generating form views

-- | Shorter code for generating simple form fields
constructView :: (Text -> View (TTNView ctx ()) -> TTNView ctx ())
                 -- ^ A function that renders a single form element by ref
              -> Text -- ^ Reference of the form element
              -> Text -- ^ Label to be used in the rendered form
              -> View (TTNView ctx ())
              -> TTNView ctx ()
constructView f ref lbl view = div_ [class_ "form-input"] $ do
    DL.label ref view $ h lbl
    f ref view
    DL.errorList ref view

inputText_ :: Text -> Text -> View (TTNView ctx ()) -> TTNView ctx ()
inputText_ = constructView DL.inputText

inputTextArea_ :: Maybe Int -> Maybe Int -- ^ Rows and columns
               -> Text -> Text -> View (TTNView ctx ()) -> TTNView ctx ()
inputTextArea_ r c = constructView $ DL.inputTextArea r c

inputPass_ :: Text -> Text -> View (TTNView ctx ()) -> TTNView ctx ()
inputPass_ = constructView DL.inputPassword

submit :: Text -> TTNView ctx ()
submit value = input_ [type_ "submit", value_ value, class_ "input-submit"]

csrf :: Token -> TTNView ctx ()
csrf tok = input_ [name_ "__csrf_token", type_ "hidden", value_ tok]

-- | Avoid annoying ambiguous types
h :: Text -> TTNView ctx ()
h = toHtml

