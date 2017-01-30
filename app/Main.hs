{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import ConnInfo                         ( ttnConnInfo )

import Control.Monad
import Control.Monad.Trans.Maybe
import Crypto.Hash.SHA256               ( hash )
import Data.ByteString                  ( ByteString )
import Data.HVect                       hiding ( null
                                               , pack )
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

type TTNAction ctx = SpockActionCtx ctx Pg.Connection TTNSes TTNSt
-- SpockAction Pg.Connection TTNSes TTNSt
type TTNCfg = SpockCfg Pg.Connection TTNSes TTNSt
type TTNMonad ctx = SpockCtxM ctx Pg.Connection TTNSes TTNSt ()

-- The web app

getCfg :: IO TTNCfg
getCfg = do cfg' <- defaultSpockCfg defSession dbConn defState
            return cfg' { spc_csrfProtection = True }

app :: TTNMonad ()
app = prehook initHook $ do
        get root $ lucid hello
        prehook guestOnlyHook $ do
            getpost "register" processRegistration
            getpost "login"    processLogin
        prehook authHook $
            get     "test"     $ lucid hello

main :: IO ()
main = do cfg <- getCfg
          runSpock 3000 (spock cfg app)

-- Authentication stuff

initHook :: TTNAction () (HVect '[])
initHook = return HNil

getUserFromSession :: TTNSes -> Maybe User
getUserFromSession (TTNSes u) = u

authHook :: TTNAction (HVect xs) (HVect (User ': xs))
authHook = do oldCtx <- getContext
              sess   <- readSession
              let mUser = getUserFromSession sess
              case mUser of
                Nothing   -> noAccessPage "Sorry, no access! Log in first."
                Just user -> return (user :&: oldCtx)

data IsGuest = IsGuest

guestOnlyHook :: TTNAction (HVect xs) (HVect (IsGuest ': xs))
guestOnlyHook = do oldCtx     <- getContext
                   (TTNSes u) <- readSession
                   case u of
                     Nothing   -> return (IsGuest :&: oldCtx)
                     Just user -> noAccessPage "You're already logged in!"

noAccessPage :: Text -> TTNAction ctx a
noAccessPage msg = do setStatus status403
                      lucid . pageTemplate $ toHtml msg

-- Some reusable functions for forms and form views

renderForm :: FormRenderer -> Text -> View Text -> Html ()
renderForm fr tok view = fr tok $ fmap toHtml view

checkNE :: Text -> Bool
checkNE = (> 0) . T.length -- non-empty

-- User data type

encodePass :: Text -> Text
encodePass = pack . show . hash . encodeUtf8

data User = User Text Text Text
            deriving ( Read, Show )

instance Pg.FromRow User where
    fromRow = do (userId :: Int) <- Pg.field
                 name <- Pg.field
                 email <- Pg.field
                 passHash <- Pg.field
                 return (User name email passHash)

instance Pg.ToRow User where
    toRow (User name email passHash) = Pg.toRow (name, email, passHash)

-- Registration

sqlAddUser :: Pg.Query
sqlAddUser =
    [sql| INSERT INTO users (name, email, password)
          VALUES (?, ?, ?) |]

insertUser :: User -> Pg.Connection -> IO ()
insertUser user dbConn = do
    Pg.execute dbConn sqlAddUser user
    return ()

sqlTestUniqueness :: Pg.Query
sqlTestUniqueness = [sql| SELECT * FROM users WHERE name = ? OR email = ? |]

testUniqueness :: (Text, Text) -> Pg.Connection -> IO [User]
testUniqueness vals dbConn = Pg.query dbConn sqlTestUniqueness vals

processRegistration :: TTNAction ctx ()
processRegistration = do tok <- getCsrfToken
                         (v, l) <- runForm "register" registerForm
                         case l of
                           Nothing -> lucid . pageTemplate $ renderForm renderRegister tok v
                           Just u -> do runQuery (insertUser u)
                                        lucid . pageTemplate . toHtml $ show u

registerForm :: Form Text (TTNAction ctx) User
registerForm = "register" .: checkM nonUniqueMsg uniqueness (mkUser
    <$> "username" .: check "No username supplied" checkNE (text Nothing)
    <*> "email"    .: check "No email supplied"    checkNE (text Nothing)
    <*> "password" .: check "No password supplied" checkNE (text Nothing))
  where nonUniqueMsg = "Username or email already registered"
        mkUser u e p = User u e $ encodePass p
        uniqueness (User u e _) = null <$> runQuery (testUniqueness (u, e))

renderRegister :: Text -> View (Html ()) -> Html ()
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

-- TODO: Validate email
-- TODO: Check username/email separately
getUsers :: (Text, Text) -> Pg.Connection -> IO [User]
getUsers creds dbConn = Pg.query dbConn sqlGetUser creds

processLogin :: TTNAction ctx ()
processLogin = do tok <- getCsrfToken
                  (v, l) <- runForm "login" loginForm
                  case l of
                    Nothing -> lucid . pageTemplate $ renderForm renderLogin tok v
                    Just u  -> do writeSession $ TTNSes (Just u)
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

renderLogin :: Text -> View (Html ()) -> Html ()
renderLogin tok view = pageTemplate $
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

lucid :: Html () -> TTNAction ctx a
lucid document = html (toStrict (renderText document))

-- Some functions for form views

type FormRenderer = Text -> View (Html ()) -> Html ()

viewConstr :: FormRenderer -> Text -> Text -> View (Html ()) -> Html ()
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

