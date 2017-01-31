{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Routing where

import Web.Routing.Combinators
import Web.Spock

viewArticleR :: Path '[Int] Open
viewArticleR = "article" <//> var <//> "view"

newArticleR :: Path '[] Open
newArticleR = "article" <//> "new"

editArticleR :: Path '[Int] Open
editArticleR = "article" <//> var <//> "edit"

registerR :: Path '[] Open
registerR = "register"

loginR :: Path '[] Open
loginR = "login"
