{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Routing where

import Web.Routing.Combinators
import Web.Spock

viewArticleR, editArticleR, translateArticleR :: Path '[Int] Open
viewArticleR      = "articles" <//> var <//> "view"
editArticleR      = "articles" <//> var <//> "edit"
translateArticleR = "articles" <//> var <//> "translate"

listArticlesR, newArticleR :: Path '[] Open
newArticleR   = "articles" <//> "new"
listArticlesR = "articles"

loginR, registerR :: Path '[] Open
loginR    = "login"
registerR = "register"

