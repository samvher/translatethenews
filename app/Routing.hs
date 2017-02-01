{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Routing where

import Web.Routing.Combinators
import Web.Spock

viewArticleR :: Path '[Int] Open
viewArticleR = "articles" <//> var <//> "view"

newArticleR :: Path '[] Open
newArticleR = "articles" <//> "new"

editArticleR :: Path '[Int] Open
editArticleR = "articles" <//> var <//> "edit"

listArticlesR :: Path '[] Open
listArticlesR = "articles"

registerR :: Path '[] Open
registerR = "register"

loginR :: Path '[] Open
loginR = "login"
