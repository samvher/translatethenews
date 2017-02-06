{-# LANGUAGE OverloadedStrings #-}

module TTN.View.User where

import TTN.View.Core

import Lucid
import Text.Digestive.View

import qualified Text.Digestive.Lucid.Html5 as DL

-- Registration

renderRegister :: Token -> View (Html ()) -> Html ()
renderRegister tok view = pageTemplate $
    form_ [method_ "post", action_ "/register"]
          (do DL.errorList "register" view
              inputText_ "register.username" "Username" view
              inputText_ "register.email"    "Email"    view
              inputPass_ "register.password" "Password" view
              csrf tok
              submit "Register")

-- Login

renderLogin :: Token -> View (Html ()) -> Html ()
renderLogin tok view = pageTemplate $
    form_ [method_ "post", action_ "/login"]
          (do DL.errorList "login" view
              inputText_ "login.username" "Username" view
              inputPass_ "login.password" "Password" view
              csrf tok
              submit "Log in")
