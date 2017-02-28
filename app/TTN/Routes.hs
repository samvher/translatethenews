{-|
Module      : TTN.Routes
Description : Defines the paths and their parameters.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module TTN.Routes where

import TTN.Model.Article

import Data.Maybe                ( fromMaybe )
import Data.Text                 ( Text )
import Web.Routing.Combinators   ( Path(..)
                                 , PathState(..)
                                 , var )
import Web.Spock                 ( renderRoute
                                 , (<//>) )

-- | Argument is article id
viewArticleR, editArticleR :: Path '[Int] Open
viewArticleR      = "articles" <//> var <//> "view"
editArticleR      = "articles" <//> var <//> "edit"

viewArticlePath, editArticlePath :: Article Stored -> Text
viewArticlePath = renderRoute viewArticleR . artID'
editArticlePath = renderRoute editArticleR . artID'

-- | Arguments are first article id, then language
newTranslationR, viewTranslationR :: Path '[Int, Language] Open
newTranslationR  = "articles" <//> var <//> "translations" <//> var <//> "new"
viewTranslationR = "articles" <//> var <//> "translations" <//> var <//> "view"

newTranslationPath, viewTranslationPath :: Article Stored -> Language -> Text
newTranslationPath  a = renderRoute newTranslationR  (artID' a)
viewTranslationPath a = renderRoute viewTranslationR (artID' a)

listArticlesR, newArticleR :: Path '[] Open
newArticleR   = "articles" <//> "new"
listArticlesR = "articles"

listPrefArticlesR, listPrefTranslationsR :: Path '[] Open
listPrefArticlesR     = "articles" <//> "pref"
listPrefTranslationsR = "articles" <//> "translations" <//> "pref"

listArticlesInR, listTranslationsInR :: Path '[Language] Open
listArticlesInR     = "articles" <//> "in" <//> var
listTranslationsInR = "articles" <//> "translatedto" <//> var

loginR, logoutR, registerR, profileR :: Path '[] Open
loginR     = "login"
logoutR    = "logout"
registerR  = "register"
profileR   = "profile"

newArticlePath, listArticlesPath :: Text
newArticlePath   = renderRoute newArticleR
listArticlesPath = renderRoute listArticlesR

listPrefArticlesPath, listPrefTranslationsPath :: Text
listPrefArticlesPath     = renderRoute listPrefArticlesR
listPrefTranslationsPath = renderRoute listPrefTranslationsR

listArticlesInPath, listTranslationsInPath :: Language -> Text
listArticlesInPath     = renderRoute listArticlesInR
listTranslationsInPath = renderRoute listTranslationsInR

loginPath, logoutPath, registerPath, profilePath :: Text
loginPath        = renderRoute loginR
logoutPath       = renderRoute logoutR
registerPath     = renderRoute registerR
profilePath      = renderRoute profileR

