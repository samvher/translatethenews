{-|
Module      : Main
Description : Source for translatethenews executable.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

module Main where

import TTN.Hidden                     ( ttnConnInfo )
import TTN.Routes

import TTN.Controller.Article
import TTN.Controller.Core
import TTN.Controller.User
import TTN.Model.Core

import Network.Wai.Middleware.Static  ( staticPolicy, addBase )
import Web.Spock

-- The web app

-- | Connects paths, requests, handlers, authentication requirements
app :: TTNMonad ()
app = do middleware . staticPolicy $ addBase "static"
         prehook initHook $ do
             get root . redirect $ renderRoute listArticlesR
             get listArticlesR   listArticles
             get listArticlesInR articlesInLang
             prehook guestOnlyHook $ do
                 getpost registerR processRegistration
                 getpost loginR    processLogin
             prehook authHook $ do
                 get     listPrefArticlesR listTranslationsInLangs
                 getpost profileR          editProfile
                 get     logoutR           processLogout
                 getpost newArticleR       newArticle
                 get     viewArticleR      viewArticle
                 -- TODO: Maybe this should be disabled?
                 getpost editArticleR      editArticle
                 getpost newTranslationR   newTranslation
                 get     viewTranslationR  viewTranslation

-- | Configures and runs the Spock server
main :: IO ()
main = do cfg <- getCfg ttnConnInfo
          runSpock 3000 $ spock cfg app

