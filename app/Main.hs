{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import ConnInfo                         ( ttnConnInfo )

import Control.Monad.Trans.Maybe
import Crypto.Hash.SHA256               ( hash )
import Data.ByteString                  ( ByteString )
import Data.Maybe                       ( listToMaybe )
import Data.Text                        ( Text
                                        , pack )
import Data.Text.Encoding               ( encodeUtf8 )
import Data.Text.Lazy                   ( toStrict )
import Database.PostgreSQL.Simple.SqlQQ ( sql )
import Lucid
import Network.HTTP.Types.Status
import Web.Spock hiding ( head )
import Web.Spock.Config

import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToRow as Pg

-- Session info

data TTNSes = TTNSes (Maybe User)

defSession :: TTNSes
defSession = TTNSes Nothing

-- State info

data TTNSt = TTNSt ()

defState :: TTNSt
defState = TTNSt ()

-- PostgreSQL info
-- Login info in ConnInfo.hs in this directory (gitignored)

dbConn :: PoolOrConn Pg.Connection
dbConn = PCConn (ConnBuilder (Pg.connect ttnConnInfo)
                             Pg.close
                             (PoolCfg 5 5 60))

-- The web app

getCfg :: IO (SpockCfg Pg.Connection TTNSes TTNSt)
getCfg = do cfg' <- defaultSpockCfg defSession dbConn defState
            return cfg' { spc_csrfProtection = True }

app :: SpockM Pg.Connection TTNSes state ()
app = do get root $ lucid hello
         get "register" $ do tok <- getCsrfToken
                             lucid (register tok)
         post "register" processRegistration
         get "login" $ do s <- readSession
                          case s of
                            TTNSes Nothing  -> do tok <- getCsrfToken
                                                  lucid (login tok)
                            TTNSes (Just u) -> lucid $ pageTemplate $
                                                 toHtml ("Already logged in" :: Text)
         post "login" processLogin

main :: IO ()
main = do cfg <- getCfg
          runSpock 3000 (spock cfg app)

-- Registration processing

encodePass :: Text -> Text
encodePass = pack . show . hash . encodeUtf8

data User = User Text Text Text
            deriving ( Read, Show )

userFromPOST :: SpockAction db sess st (Maybe User)
userFromPOST = runMaybeT $ do
  name        <- MaybeT (param "name")
  email       <- MaybeT (param "email")
  password    <- MaybeT (param "password")
  return $ User name email (encodePass password)

instance Pg.FromRow User where
    fromRow = do (userId :: Int) <- Pg.field
                 name <- Pg.field
                 email <- Pg.field
                 passHash <- Pg.field
                 return (User name email passHash)

instance Pg.ToRow User where
    toRow (User name email passHash) = Pg.toRow (name, email, passHash)

sqlAddUser :: Pg.Query
sqlAddUser =
    [sql| INSERT INTO users (name, email, password)
          VALUES (?, ?, ?) |]

insertUser :: User -> Pg.Connection -> IO ()
insertUser user dbConn = do
    Pg.execute dbConn sqlAddUser user
    return ()

-- TODO: Check existing users
-- TODO: Validation
processRegistration :: SpockAction Pg.Connection sess st ()
processRegistration = do
    maybeNewUser <- userFromPOST
    case maybeNewUser of
      Nothing -> do lucid $ errorPage "Invalid submission"
                    setStatus badRequest400
      Just user -> do runQuery (insertUser user)
                      lucid $ pageTemplate (toHtml $ show user)

data LoginData = LoginData Text Text

loginFromPOST :: SpockAction db sess st (Maybe LoginData)
loginFromPOST = runMaybeT $ do
  name     <- MaybeT (param "name")
  password <- MaybeT (param "password")
  return $ LoginData name (encodePass password)

sqlGetUser :: Pg.Query
sqlGetUser = [sql| SELECT * FROM users WHERE name = ? AND password = ? |]

getUser :: LoginData -> Pg.Connection -> IO [User]
getUser (LoginData user pass) dbConn = Pg.query dbConn sqlGetUser (user, pass)

processLogin :: SpockAction Pg.Connection TTNSes st ()
processLogin = do user <- tryLogin
                  case user of
                    Nothing -> lucid $ errorPage "Login failed"
                    Just dude -> do writeSession $ TTNSes (Just dude)
                                    lucid $ pageTemplate (toHtml $ show dude)
  where tryLogin = runMaybeT $ do loginData <- MaybeT loginFromPOST
                                  let results = runQuery (getUser loginData)
                                  MaybeT (listToMaybe <$> results)

-- Some functions for view

pageTemplate :: Html () -> Html ()
pageTemplate contents = html_ (do head_ (title_ "Translate the News")
                                  body_ contents)

errorPage :: Html () -> Html ()
errorPage = pageTemplate

lucid :: Html () -> SpockAction db sess st ()
lucid document = html (toStrict (renderText document))

-- Pages

hello :: Html ()
hello = pageTemplate $ h1_ "Hello world!"

register :: Text -> Html ()
register tok = pageTemplate $
  form_ [method_ "post", action_ "/register"]
        (do formfield "Username" "name"
            formfield "Email" "email"
            passfield
            csrf tok
            submit "Register")

login :: Text -> Html ()
login tok = pageTemplate $
  form_ [method_ "post", action_ "/login"]
        (do formfield "Username" "name"
            passfield
            csrf tok
            submit "Log in")

formfield :: Text -> Text -> Html ()
formfield label name = p_ (do label_ $ toHtml label
                              input_ [name_ name])

passfield :: Html ()
passfield = p_ (do label_ "Password"
                   input_ [name_ "password", type_ "password"])

submit :: Text -> Html ()
submit value = input_ [type_ "submit", value_ value]

csrf :: Text -> Html ()
csrf tok = input_ [name_ "__csrf_token", type_ "hidden", value_ tok]

