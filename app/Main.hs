{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

-- TODO: Clean out unused dependencies and extensions

module Main where

import Article
import ConnInfo                         ( ttnConnInfo )
import Parsers
import Routing
import Types
import Util
import View

import Control.Monad
import Crypto.Hash.SHA256               ( hash )
import Data.ByteString                  ( ByteString )
import Data.HVect                       hiding ( null, pack )
import Data.Maybe                       ( listToMaybe )
import Data.Text                        ( Text
                                        , pack )
import Data.Text.Encoding               ( encodeUtf8 )
import Data.Text.Lazy                   ( toStrict )
import Database.PostgreSQL.Simple.SqlQQ ( sql )
import Lucid
import Network.HTTP.Types.Status
import Text.Digestive.Form
import Text.Digestive.Types
import Text.Digestive.View
import Text.Digestive.Lucid.Html5
import Web.Spock                        hiding ( head, text )
import Web.Spock.Config
import Web.Spock.Digestive

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToRow as Pg

-- PostgreSQL info
-- Login info in ConnInfo.hs in this directory (gitignored)

dbConn :: PoolOrConn Pg.Connection
dbConn = PCConn (ConnBuilder (Pg.connect ttnConnInfo)
                             Pg.close
                             (PoolCfg 5 5 60))

-- The web app

getCfg :: IO TTNCfg
getCfg = do cfg' <- defaultSpockCfg defSession dbConn defState
            return cfg' { spc_csrfProtection = True }

app :: TTNMonad ()
app = prehook initHook $ do
        get root $ lucid hello
        get listArticlesR listArticles
        get viewArticleR viewArticle
        prehook guestOnlyHook $ do
            getpost registerR processRegistration
            getpost loginR    processLogin
        prehook authHook $ do
            getpost newArticleR processArticle
            getpost editArticleR editArticle
            getpost translateArticleR translateArticle

main :: IO ()
main = do cfg <- getCfg
          runSpock 3000 (spock cfg app)

-- Authentication stuff

initHook :: TTNAction () (HVect '[])
initHook = return HNil

visitorLoggedIn :: TTNAction ctx Bool
visitorLoggedIn = do u <- sessUser <$> readSession
                     return $ case u of
                       Nothing -> False
                       Just _  -> True

authHook :: TTNAction (HVect xs) (HVect (User ': xs))
authHook = do oldCtx <- getContext
              u      <- sessUser <$> readSession
              case u of
                Nothing   -> noAccessPage "Sorry, no access! Log in first."
                Just user -> return (user :&: oldCtx)

data IsGuest = IsGuest

guestOnlyHook :: TTNAction (HVect xs) (HVect (IsGuest ': xs))
guestOnlyHook = do oldCtx     <- getContext
                   loggedIn   <- visitorLoggedIn
                   if loggedIn
                     then noAccessPage "You're already logged in!"
                     else return (IsGuest :&: oldCtx)

noAccessPage :: Text -> TTNAction ctx a
noAccessPage msg = do setStatus status403
                      lucid . pageTemplate $ toHtml msg

-- Registration and login stuff

encodePass :: Text -> Text
encodePass = pack . show . hash . encodeUtf8

-- Registration

sqlAddUser :: Pg.Query
sqlAddUser =
    [sql| INSERT INTO users (name, email, password) VALUES (?, ?, ?) |]

insertUser :: User -> Pg.Connection -> IO ()
insertUser user dbConn = void $ Pg.execute dbConn sqlAddUser user

sqlTestUniqueness :: Pg.Query
sqlTestUniqueness = [sql| SELECT * FROM users WHERE name = ? OR email = ? |]

testUniqueness :: (Text, Text) -> Pg.Connection -> IO [User]
testUniqueness vals dbConn = Pg.query dbConn sqlTestUniqueness vals

processRegistration :: TTNAction ctx a
processRegistration = serveForm "register" registerForm renderRegister $ \u ->
                        do runQuery $ insertUser u
                           lucid . pageTemplate . toHtml $ show u

registerForm :: Form Text (TTNAction ctx) User
registerForm = "register" .: checkM nonUniqueMsg uniqueness (mkUser
    <$> "username" .: check "No username supplied" checkNE (text Nothing)
    <*> "email"    .: check "Email not valid" (testPattern emailP) (text Nothing)
    <*> "password" .: check "No password supplied" checkNE (text Nothing))
  where nonUniqueMsg = "Username or email already registered"
        mkUser u e p = User u e $ encodePass p
        uniqueness (User u e _) = null <$> runQuery (testUniqueness (u, e))

renderRegister :: Token -> View (Html ()) -> Html ()
renderRegister tok view = pageTemplate $
    form_ [method_ "post", action_ "/register"]
          (do errorList "register" view
              inputText_ "register.username" "Username" view
              inputText_ "register.email"    "Email"    view
              inputPass_ "register.password" "Password" view
              csrf tok
              submit "Register")

-- Login

sqlGetUser :: Pg.Query
sqlGetUser = [sql| SELECT * FROM users WHERE name = ? AND password = ? |]

-- TODO: Confirmation email
-- TODO: Check username/email separately
getUsers :: (Text, Text) -> Pg.Connection -> IO [User]
getUsers creds dbConn = Pg.query dbConn sqlGetUser creds

processLogin :: TTNAction ctx a
processLogin = serveForm "login" loginForm renderLogin $ \u ->
                 do modifySession $ \s -> s { sessUser = Just u }
                    lucid . pageTemplate . toHtml $ show u

loginForm :: Form Text (TTNAction ctx) User
loginForm = "login" .: validateM findUser (readCreds
    <$> "username" .: check "No username supplied" checkNE (text Nothing)
    <*> "password" .: check "No password supplied" checkNE (text Nothing))
  where readCreds u p  = (u, encodePass p)
        findUser creds = let f  [] = Error "Invalid credentials"
                             f [u] = Success u
                             f  _  = Error "Multiple users, contact an admin"
                          in f <$> runQuery (getUsers creds)

renderLogin :: Token -> View (Html ()) -> Html ()
renderLogin tok view = pageTemplate $
    form_ [method_ "post", action_ "/login"]
          (do errorList "login" view
              inputText_ "login.username" "Username" view
              inputPass_ "login.password" "Password" view
              csrf tok
              submit "Log in")

