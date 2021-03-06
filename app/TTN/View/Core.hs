{-|
Module      : TTN.View.Core
Description : Central code for templating.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module TTN.View.Core where

import TTN.Routes

import TTN.Model.Article
import TTN.Model.Language
import TTN.Model.Core
import TTN.Model.User
import TTN.View.Shared

import Control.Monad.Trans.Class        ( lift )
import Data.Monoid                      ( (<>) )
import Data.Text                        ( Text )
import Database.Persist                 ( Entity )
import Lucid
import Text.Digestive.View

import qualified Web.Spock as S

-- * Higher level layout functions

renderPage :: TTNBlockDef ctx -> TTNAction ctx a
renderPage blockDef = lucid $ runTemplate pageTemplate blockDef 

renderSimpleStr :: String -> TTNAction ctx a
renderSimpleStr msg = renderPage blockDef
  where blockDef TTNContent = div_ [id_ "simple-message"] $ toHtml msg
        blockDef other      = defaultBlocks other

renderSimpleHtml :: TTNView ctx () -> TTNAction ctx a
renderSimpleHtml h = renderPage blockDef
  where blockDef TTNContent = h
        blockDef other      = defaultBlocks other

renderSimpleForm :: FormRenderer ctx -> View Text -> TTNAction ctx a
renderSimpleForm renderer view = renderPage blockDef
  where blockDef TTNContent = renderer $ fmap toHtml view
        blockDef other      = defaultBlocks other

-- errorPage :: TTNView ctx () -> TTNView ctx ()
-- errorPage = pageTemplate . div_ [id_ "simple-message"]

-- * Template functions

pageTemplate :: TTNTemplate ctx ()
pageTemplate = html_ ( htmlHead >> htmlBody )

htmlHead :: TTNTemplate ctx ()
htmlHead = head_ $ do
    title_ $ getBlock TTNPageTitle
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/reset.css"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/style.css"]
    let gFonts = "https://fonts.googleapis.com/css?" <>
          "family=EB+Garamond|PT+Sans:400,400i,700&amp;" <>
          "subset=cyrillic,cyrillic-ext,latin-ext"
    link_ [href_ gFonts, rel_ "stylesheet"]

htmlBody :: TTNTemplate ctx ()
htmlBody = body_ . div_ [id_ "container"] $ do
    div_ [id_ "header"] $ do
      h1_ . a_ [href_ "/"] $ do span_ [class_ "site-title"] "translatethenews.org"
                                span_ [class_ "site-logo"] ""
      div_ [class_ "nav"] $ do
          loginSegment
          navSegment
    div_ [id_ "content-main"] $ getBlock TTNContent

loginSegment :: TTNTemplate ctx ()
loginSegment = maybe notLoggedInBox loggedInBox =<< getCurrentUser
  where notLoggedInBox = ul_ [id_ "login-box"] . li_ $ do
                             a_ [href_ loginPath] "Log in"
                             h " or "
                             a_ [href_ registerPath] "register"
        loggedInBox :: Entity User -> TTNTemplate ctx ()
        loggedInBox u = ul_ [id_ "login-box"] $ do
                            li_ . h $ "Logged in as " <> userName (entUser u) <> "."
                            li_ . a_ [href_ logoutPath] $ h "Log out"
                            li_ . a_ [href_ profilePath] $ h "Edit profile"

navSegment :: TTNTemplate ctx ()
navSegment = maybe notLoggedInNav loggedInNav =<< getCurrentUser
  where loggedInNav :: Entity User -> TTNTemplate ctx ()
        loggedInNav _ = ul_ . div_ [id_ "nav-box"] $ do
            li_ . a_ [href_ listPrefTranslationsPath] $ h "View translations"
            li_ . a_ [href_ listPrefArticlesPath]     $ h "Translate articles"
            li_ . a_ [href_ listMyArticlesPath]       $ h "My articles"
        mkLangNavLink :: Language -> TTNTemplate ctx ()
        mkLangNavLink l =
            li_ . a_ [href_ $ listTranslationsInPath l] . h $ langAsText l
        notLoggedInNav :: TTNTemplate ctx ()
        notLoggedInNav = ul_ . div_ [id_ "lang-box"] $ do
                             li_ . h $ "View translations in:"
                             mapM_ mkLangNavLink allLanguages

