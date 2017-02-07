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

loginR, logoutR, registerR :: Path '[] Open
loginR    = "login"
logoutR   = "logout"
registerR = "register"

newArticlePath, listArticlesPath, loginPath, logoutPath, registerPath :: Text
newArticlePath   = renderRoute newArticleR
listArticlesPath = renderRoute listArticlesR
loginPath        = renderRoute loginR
logoutPath       = renderRoute logoutR
registerPath     = renderRoute registerR

