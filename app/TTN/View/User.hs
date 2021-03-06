{-|
Module      : TTN.View.User
Description : Templating code related to the user subsystem.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module TTN.View.User where

import TTN.Routes

import TTN.Controller.Shared
import TTN.Model.Core
import TTN.Model.User
import TTN.View.Shared

import Control.Monad.Trans.Class            ( lift )
import Data.Maybe                           ( fromJust )
import Data.Text                            ( Text )
import Database.Persist                     ( get )

import Lucid
import Text.Digestive.View

import qualified Text.Digestive.Lucid.Html5 as DL
import qualified Web.Spock                  as S

-- * Registration

renderRegisterForm :: View (TTNView ctx ()) -> TTNView ctx ()
renderRegisterForm view = div_ [id_ "register-form"] .
    form_ [method_ "post", action_ "/register"] $ do
        DL.errorList "register" view
        inputText_ "register.username" "Username" view
        inputText_ "register.email"    "Email"    view
        inputPass_ "register.password" "Password" view
        csrf
        submit "Register"

-- * User profile

renderProfileForm :: Text -> View (TTNView ctx ()) -> TTNView ctx ()
renderProfileForm target view = div_ [id_ "edit-profile-form"] $ do
    -- TODO: next line is ugly and should be abstracted
    currentUName <- lift (userName . entUser . fromJust . sessUser <$> S.readSession)
    h2_ $ h currentUName
    form_ [method_ "post", action_ target] $ do
        DL.errorList "profile" view
        inputText_ "profile.email" "Email" view
        inputMultiSelect_ "profile.read-langs" "Languages you would like to read in" view
        inputMultiSelect_ "profile.trans-langs" "Languages you would like to translate from" view
        csrf
        submit "Edit profile"

-- * Login

renderLoginForm :: View (TTNView ctx ()) -> TTNView ctx ()
renderLoginForm view = div_ [id_ "login-form"] .
    form_ [method_ "post", action_ "/login"] $ do
        DL.errorList "login" view
        inputText_ "login.username" "Username" view
        inputPass_ "login.password" "Password" view
        csrf
        submit "Log in"

-- * User profile

-- TODO: Flesh out
renderProfileBadge :: Key User -> TTNView ctx ()
renderProfileBadge uID = do
    user <- lift . runSQL $ get uID
    em_ . toHtml $ maybe "(unknown)" userName user

-- * Authentication pages

mustLogin :: TTNView ctx ()
mustLogin = div_ [id_ "simple-message"] $ do
    h "Sorry, no access! "
    a_ [href_ loginPath] $ h "Log in"
    h " first."

loggedIn :: TTNView ctx ()
loggedIn = div_ [id_ "simple-message"] $ do 
    h "You're already logged in! "
    a_ [href_ logoutPath] $ h "Log out"
    h " if you want to go here."

noPermission :: TTNView ctx ()
noPermission = div_ [id_ "simple-message"] $ do
    h "You don't have permission to view this page."

