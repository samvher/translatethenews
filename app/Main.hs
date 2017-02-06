module Main where

import TTN.ConnInfo           ( ttnConnInfo )
import TTN.Routes

import TTN.Controller.Article
import TTN.Controller.Core
import TTN.Controller.User
import TTN.Model.Core

import Web.Spock

-- The web app

app :: TTNMonad ()
app = prehook initHook $ do
        get root hello
        get listArticlesR listArticles
        prehook guestOnlyHook $ do
            getpost registerR processRegistration
            getpost loginR    processLogin
        prehook authHook $ do
            get     logoutR           processLogout
            get     viewArticleR      viewArticle
            getpost newArticleR       processArticle
            getpost editArticleR      editArticle -- TODO: Maybe this should be disabled?
            getpost newTranslationR   translateArticle
            get     viewTranslationR  viewTranslation

main :: IO ()
main = do cfg <- getCfg ttnConnInfo
          runSpock 3000 (spock cfg app)

