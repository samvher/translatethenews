{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Routing where

import Types

import Web.Routing.Combinators
import Web.Spock

viewArticleR, editArticleR :: Path '[Int] Open
viewArticleR      = "articles" <//> var <//> "view"
editArticleR      = "articles" <//> var <//> "edit"

translateArticleR :: Path '[Int, Language] Open
translateArticleR = "articles" <//> var <//> "translateto" <//> var

listArticlesR, newArticleR :: Path '[] Open
newArticleR   = "articles" <//> "new"
listArticlesR = "articles"

loginR, logoutR, registerR :: Path '[] Open
loginR    = "login"
logoutR   = "logout"
registerR = "register"

