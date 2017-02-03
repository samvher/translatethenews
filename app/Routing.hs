{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Routing where

import Types

import Web.Routing.Combinators
import Web.Spock

viewArticleR, editArticleR :: Path '[Int] Open
viewArticleR      = "articles" <//> var <//> "view"
editArticleR      = "articles" <//> var <//> "edit"

newTranslationR, viewTranslationR :: Path '[Int, Language] Open
newTranslationR = "articles" <//> var <//> "translations" <//> var <//> "new"
viewTranslationR = "articles" <//> var <//> "translations" <//> var <//> "view"

listArticlesR, newArticleR :: Path '[] Open
newArticleR   = "articles" <//> "new"
listArticlesR = "articles"

loginR, logoutR, registerR :: Path '[] Open
loginR    = "login"
logoutR   = "logout"
registerR = "register"

