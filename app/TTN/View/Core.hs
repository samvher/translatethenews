{-|
Module      : TTN.View.Core
Description : Site-wide code for templating.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module TTN.View.Core where

import TTN.Routes

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

defaultBlocks :: TTNBlockDef ctx
defaultBlocks TTNPageTitle = h "Translate the News"
defaultBlocks TTNNavBar    = h "Nav bar"
defaultBlocks TTNContent   = return ()

mkPage :: TTNBlockDef ctx -> TTNView ctx ()
mkPage blockDef = runTemplate pageTemplate blockDef 

pageTemplate :: TTNTemplate ctx
pageTemplate = html_ ( htmlHead >> htmlBody )

htmlHead :: TTNTemplate ctx
htmlHead = head_ $ do
    title_ $ getBlock TTNPageTitle
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/reset.css"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/style.css"]
    let gFonts = "https://fonts.googleapis.com/css?" <>
          "family=EB+Garamond|PT+Sans:400,400i,700&amp;" <>
          "subset=cyrillic,cyrillic-ext,latin-ext"
    link_ [href_ gFonts, rel_ "stylesheet"]

htmlBody :: TTNTemplate ctx
htmlBody = body_ . div_ [id_ "container"] $ do
    div_ [id_ "header"] $ do
      h1_ . a_ [href_ "/"] $ "translatethenews.org"
      div_ . a_ [href_ loginPath] $ "log in"
    div_ [id_ "content-main"] $ getBlock TTNContent

-- errorPage :: TTNView ctx () -> TTNView ctx ()
-- errorPage = pageTemplate . div_ [id_ "simple-message"]

renderPage :: TTNBlockDef ctx -> TTNAction ctx a
renderPage = lucid . mkPage

renderSimpleStr :: String -> TTNAction ctx a
renderSimpleStr msg = renderPage blockDef
  where blockDef TTNContent = div_ [id_ "simple-message"] $ toHtml msg
        blockDef other      = defaultBlocks other

renderSimpleHtml :: TTNView ctx () -> TTNAction ctx a
renderSimpleHtml h = renderPage blockDef
  where blockDef TTNContent = h
        blockDef other      = defaultBlocks other

renderSimpleForm :: FormRenderer ctx -> Token -> View Text -> TTNAction ctx a
renderSimpleForm renderer tok view = renderPage blockDef
  where blockDef TTNContent = renderer tok $ fmap toHtml view
        blockDef other      = defaultBlocks other

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

