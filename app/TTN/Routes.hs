{-|
Module      : TTN.Routes
Description : Defines the paths and their parameters.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module TTN.Routes where

import TTN.Model.Article
import TTN.Model.Language

import Data.Maybe                ( fromMaybe )
import Data.Text                 ( Text )
import Database.Persist          ( Entity )
import Web.Routing.Combinators   ( Path(..)
                                 , PathState(..)
                                 , var )
import Web.Spock                 ( renderRoute
                                 , root
                                 , (<//>) )

-- | Argument is article id
viewArticleR, editArticleR :: Path '[Key Article] Open
viewArticleR      = "articles" <//> var
editArticleR      = "articles" <//> var <//> "edit"

viewArticlePath, editArticlePath :: Entity Article -> Text
viewArticlePath = renderRoute viewArticleR . articleID
editArticlePath = renderRoute editArticleR . articleID

-- | Arguments are first article id, then language
autoTranslationR, newTranslationR, viewTranslationR
  :: Path '[Key Article, Language] Open
autoTranslationR = "articles" <//> var <//> "translations" <//> var <//> "auto"
newTranslationR  = "articles" <//> var <//> "translations" <//> var <//> "new"
viewTranslationR = "articles" <//> var <//> "translations" <//> var <//> "view"

autoTranslationPath, newTranslationPath, viewTranslationPath :: Entity Article
                                                             -> Language
                                                             -> Text
autoTranslationPath a = renderRoute autoTranslationR (articleID a)
newTranslationPath  a = renderRoute newTranslationR  (articleID a)
viewTranslationPath a = renderRoute viewTranslationR (articleID a)

listArticlesR, newArticleR :: Path '[] Open
newArticleR   = "articles" <//> "new"
listArticlesR = "articles"

listPrefArticlesR, listPrefTranslationsR, listMyArticlesR :: Path '[] Open
listPrefArticlesR     = "articles"     <//> "in" <//> "profilelangs"
listPrefTranslationsR = "translations" <//> "in" <//> "profilelangs"
listMyArticlesR       = "my-articles"

listArticlesInR, listTranslationsInR :: Path '[Language] Open
listArticlesInR     = "articles"     <//> "in" <//> var
listTranslationsInR = "translations" <//> "in" <//> var

loginR, logoutR, registerR, profileR :: Path '[] Open
loginR     = "login"
logoutR    = "logout"
registerR  = "register"
profileR   = "profile" <//> "edit"

newArticlePath, listArticlesPath :: Text
newArticlePath   = renderRoute newArticleR
listArticlesPath = renderRoute listArticlesR

listPrefArticlesPath, listPrefTranslationsPath, listMyArticlesPath :: Text
listPrefArticlesPath     = renderRoute listPrefArticlesR
listPrefTranslationsPath = renderRoute listPrefTranslationsR
listMyArticlesPath       = renderRoute listMyArticlesR

listArticlesInPath, listTranslationsInPath :: Language -> Text
listArticlesInPath     = renderRoute listArticlesInR
listTranslationsInPath = renderRoute listTranslationsInR

loginPath, logoutPath, registerPath, profilePath, rootPath :: Text
loginPath        = renderRoute loginR
logoutPath       = renderRoute logoutR
registerPath     = renderRoute registerR
profilePath      = renderRoute profileR
rootPath         = renderRoute root

migration1R :: Path '[] Open
migration1R = "migrate1"

