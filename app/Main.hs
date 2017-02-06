{-|
Module      : Main
Description : Source for translatethenews executable.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

module Main where

import TTN.ConnInfo           ( ttnConnInfo )
import TTN.Routes

import TTN.Controller.Article
import TTN.Controller.Core
import TTN.Controller.User
import TTN.Model.Core

import Web.Spock

-- The web app

-- | Connects paths, requests, handlers, authentication requirements
app :: TTNMonad ()
app = prehook initHook $ do
        get listArticlesR listArticles
        get root . redirect $ renderRoute loginR
        prehook guestOnlyHook $ do
            getpost registerR processRegistration
            getpost loginR    processLogin
        prehook authHook $ do
            get     logoutR           processLogout
            getpost newArticleR       newArticle
            get     viewArticleR      viewArticle
            getpost editArticleR      editArticle -- TODO: Maybe this should be disabled?
            getpost newTranslationR   newTranslation
            get     viewTranslationR  viewTranslation

-- | Configures and runs the Spock server
main :: IO ()
main = do cfg <- getCfg ttnConnInfo
          runSpock 3000 (spock cfg app)

