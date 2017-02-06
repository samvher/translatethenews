{-|
Module      : TTN.Routes
Description : Defines the paths and their parameters.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module TTN.Routes where

import TTN.Model.Article ( Language )

import Web.Routing.Combinators
import Web.Spock

-- | Argument is article id
viewArticleR, editArticleR :: Path '[Int] Open
viewArticleR      = "articles" <//> var <//> "view"
editArticleR      = "articles" <//> var <//> "edit"

-- | Arguments are first article id, then language
newTranslationR, viewTranslationR :: Path '[Int, Language] Open
newTranslationR  = "articles" <//> var <//> "translations" <//> var <//> "new"
viewTranslationR = "articles" <//> var <//> "translations" <//> var <//> "view"

listArticlesR, newArticleR :: Path '[] Open
newArticleR   = "articles" <//> "new"
listArticlesR = "articles"

loginR, logoutR, registerR :: Path '[] Open
loginR    = "login"
logoutR   = "logout"
registerR = "register"

