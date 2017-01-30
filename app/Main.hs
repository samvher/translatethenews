{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import ConnInfo                         ( ttnConnInfo )

import Control.Monad
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
import Text.Digestive.Form
import Text.Digestive.View
import Text.Digestive.Lucid.Html5
import Web.Spock                        hiding ( head, text )
import Web.Spock.Config
import Web.Spock.Digestive

import qualified Data.Text as T
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

-- We're not going to be generic

type TTNAction = SpockAction Pg.Connection TTNSes TTNSt
type TTNCfg    = SpockCfg    Pg.Connection TTNSes TTNSt
type TTNMonad  = SpockM      Pg.Connection TTNSes TTNSt

-- The web app

getCfg :: IO TTNCfg
getCfg = do cfg' <- defaultSpockCfg defSession dbConn defState
            return cfg' { spc_csrfProtection = True }

app :: TTNMonad ()
app = do get root $ lucid hello
         get "register" $ do tok <- getCsrfToken
                             lucid (register tok)
         post "register" processRegistration
--          get "login" $ do s <- readSession
--                           case s of
--                             TTNSes Nothing  -> do tok <- getCsrfToken
--                                                   lucid (login tok)
--                             TTNSes (Just u) -> lucid $ pageTemplate $
--                                                  toHtml ("Already logged in" :: Text)
         getpost "login" processLogin

main :: IO ()
main = do cfg <- getCfg
          runSpock 3000 (spock cfg app)

-- Registration processing

encodePass :: Text -> Text
encodePass = pack . show . hash . encodeUtf8

data User = User Text Text Text
            deriving ( Read, Show )

userFromPOST :: TTNAction (Maybe User)
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
processRegistration :: TTNAction ()
processRegistration = do
    maybeNewUser <- userFromPOST
    case maybeNewUser of
      Nothing -> do lucid $ errorPage "Invalid submission"
                    setStatus badRequest400
      Just user -> do runQuery (insertUser user)
                      lucid $ pageTemplate (toHtml $ show user)

data LoginData = LoginData Text Text
                 deriving ( Read, Show )

sqlGetUser :: Pg.Query
sqlGetUser = [sql| SELECT * FROM users WHERE name = ? AND password = ? |]

getUser :: LoginData -> Pg.Connection -> IO [User]
getUser (LoginData user pass) dbConn = Pg.query dbConn sqlGetUser (user, pass)

processLogin :: TTNAction ()
processLogin = do tok <- getCsrfToken
                  (v, l) <- runForm "login" loginForm
                  case l of
                    Nothing -> lucid . pageTemplate $ loginView tok (fmap toHtml v)
                    Just dude -> lucid . pageTemplate . toHtml $ show dude

loginForm :: Form Text TTNAction LoginData
loginForm = "login" .: checkM "Invalid credentials" checkLogin (mkLoginData
    <$> "username" .: check "No username supplied" checkNE (text Nothing)
    <*> "password" .: check "No password supplied" checkNE (text Nothing))
  where mkLoginData u p = LoginData u $ encodePass p
        checkNE = (> 0) . T.length -- non-empty
        checkLogin loginData = not . null <$> runQuery (getUser loginData)

loginView :: Text -> View (Html ()) -> Html ()
loginView tok view = pageTemplate $
    form_ [method_ "post", action_ "/login"]
          (do errorList "login" view
              inputText_ "login.username" "Username" view
              inputPass_ "login.password" "Password" view
              csrf tok
              submit "Log in")

-- Some functions for layout

pageTemplate :: Html () -> Html ()
pageTemplate contents = html_ (do head_ (title_ "Translate the News")
                                  body_ contents)

errorPage :: Html () -> Html ()
errorPage = pageTemplate

lucid :: Html () -> TTNAction ()
lucid document = html (toStrict (renderText document))

-- Some functions for form views

viewConstr :: (Text -> View (Html ()) -> Html ())
           -> Text
           -> Text
           -> View (Html ())
           -> Html ()
viewConstr f ref lbl view = p_ (do label ref view $ toHtml lbl
                                   f ref view
                                   errorList ref view)

inputText_ :: Text -> Text -> View (Html ()) -> Html ()
inputText_ = viewConstr inputText

inputPass_ :: Text -> Text -> View (Html ()) -> Html ()
inputPass_ = viewConstr inputPassword

submit :: Text -> Html ()
submit value = p_ $ input_ [type_ "submit", value_ value]

csrf :: Text -> Html ()
csrf tok = input_ [name_ "__csrf_token", type_ "hidden", value_ tok]

-- Pages

hello :: Html ()
hello = pageTemplate $ h1_ "Hello world!"

register :: Text -> Html ()
register tok = pageTemplate $
  form_ [method_ "post", action_ "/register"]
        (do -- formfield "Username" "name"
            -- formfield "Email" "email"
            -- passfield
            csrf tok
            submit "Register")

