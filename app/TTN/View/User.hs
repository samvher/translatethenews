{-|
Module      : TTN.View.User
Description : Templating code related to the user subsystem.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module TTN.View.User where

import TTN.Routes

import TTN.Model.Core
import TTN.Model.User
import TTN.View.Core

import Lucid
import Text.Digestive.View

import qualified Text.Digestive.Lucid.Html5 as DL

-- * Registration

renderRegisterForm :: Token -> View (TTNView ctx ()) -> TTNView ctx ()
renderRegisterForm tok view = form_ [method_ "post", action_ "/register"]
          (do DL.errorList "register" view
              inputText_ "register.username" "Username" view
              inputText_ "register.email"    "Email"    view
              inputPass_ "register.password" "Password" view
              csrf tok
              submit "Register")

-- * Login

renderLoginForm :: Token -> View (TTNView ctx ()) -> TTNView ctx ()
renderLoginForm tok view = div_ [id_ "login-form"] $
    form_ [method_ "post", action_ "/login"]
          (do DL.errorList "login" view
              inputText_ "login.username" "Username" view
              inputPass_ "login.password" "Password" view
              csrf tok
              submit "Log in")

-- * User profile

-- TODO: Flesh out
renderProfileBadge :: User -> TTNView ctx ()
renderProfileBadge u = em_ . toHtml $ uName u

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

