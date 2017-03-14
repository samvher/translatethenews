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
import TTN.Model.Article              ( Language(..) )
import TTN.Model.Core

import Network.Wai.Middleware.Static  ( staticPolicy, addBase )
import Web.Spock

-- The web app

-- | Connects paths, requests, handlers, authentication requirements
app :: TTNMonad ()
app = do middleware . staticPolicy $ addBase "static"
         prehook initHook $ do
             get root                welcomePage
             get listTranslationsInR listTranslationsIn
             get viewTranslationR    viewTranslation
             prehook guestOnlyHook $ do
                 getpost registerR processRegistration
                 getpost loginR    processLogin
             prehook authHook $ do
                 get     listArticlesR         listArticles
                 get     listArticlesInR       articlesInLang
                 get     listPrefTranslationsR listPrefTranslations
                 get     listPrefArticlesR     listArticlesInLangs
                 get     listMyArticlesR       listUserArticles
                 getpost profileR              editProfile
                 get     logoutR               processLogout
                 getpost newArticleR           newArticle
                 get     viewArticleR          viewArticle
                 -- Disabling this for now, we don't really want articles
                 -- to be edited, especially if there are translations.
                 -- getpost editArticleR          editArticle
                 getpost newTranslationR       newTranslation

-- | Configures and runs the Spock server
main :: IO ()
main = do cfg <- getCfg ttnConnInfo
          runSpock 3000 $ spock cfg app

