{-|
Module      : TTN.Controller.User
Description : Controller code for authentication-related issues.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TTN.Controller.User where

import TTN.Routes
import TTN.Util                         ( checkNE
                                        , emailP
                                        , testPattern )

import TTN.Controller.Core
import TTN.Model.Core
import TTN.Model.User
import TTN.View.Core
import TTN.View.User

import Crypto.Hash.SHA256               ( hash )
import Data.HVect                       ( HVect(..) )
import Data.Text                        ( Text, pack )
import Data.Text.Encoding               ( encodeUtf8 )
import Text.Digestive.Form
import Text.Digestive.Types

import qualified Web.Spock as S

-- * Type-safe authentication functions

-- | Test if the current visitor is logged in
visitorLoggedIn :: TTNAction ctx Bool
visitorLoggedIn = do u <- sessUser <$> S.readSession
                     return $ case u of
                       Nothing -> False
                       Just _  -> True

-- | Pages behind this hook require authentication
authHook :: TTNAction (HVect xs) (HVect (User ': xs))
authHook = do oldCtx <- S.getContext
              u      <- sessUser <$> S.readSession
              case u of
                Nothing   -> noAccess "Sorry, no access! Log in first."
                Just user -> return (user :&: oldCtx)

-- | Pages behind this hook require that the visitor is not logged in
guestOnlyHook :: TTNAction (HVect xs) (HVect (IsGuest ': xs))
guestOnlyHook = do oldCtx     <- S.getContext
                   loggedIn   <- visitorLoggedIn
                   if loggedIn
                     then noAccess "You're already logged in!"
                     else return (IsGuest :&: oldCtx)

-- * Password hashing

-- | Use SHA256 and some trivial transformations
encodePass :: Text -> Text
encodePass = pack . show . hash . encodeUtf8

-- * Registration

-- TODO: Confirmation email
-- | Process and serve registration form, add a successful registration to
--   the database.
processRegistration :: TTNAction ctx a
processRegistration =
    serveForm "register" registerForm renderRegisterForm $ \u ->
        do S.runQuery $ insertUser u
           renderSimpleStr $ "Success: " ++ show u

-- | Registration form
registerForm :: Form Text (TTNAction ctx) (Text, Text, Text)
registerForm = "register" .: checkM nonUniqueMsg uniqueness ( prepUser
    <$> "username" .: check "No username supplied" checkNE (text Nothing)
    <*> "email"    .: check "Email not valid" (testPattern emailP) (text Nothing)
    <*> "password" .: check "No password supplied" checkNE (text Nothing))
  where nonUniqueMsg = "Username or email already registered"
        uniqueness (n, e, _) = S.runQuery $ isUnique (n, e)
        prepUser n e p = (n, e, encodePass p)

-- * Login/logout

-- | Process and serve login form, register successful login in the
--   session.
processLogin :: TTNAction ctx a
processLogin = serveForm "login" loginForm renderLoginForm $ \u ->
                 do S.modifySession ( \s -> s { sessUser = Just u } )
                    S.redirect $ S.renderRoute listArticlesR

-- | Login form
loginForm :: Form Text (TTNAction ctx) User
loginForm = "login" .: validateM findUser (readCreds
    <$> "username" .: check "No username supplied" checkNE (text Nothing)
    <*> "password" .: check "No password supplied" checkNE (text Nothing))
  where readCreds u p  = (u, encodePass p)
        findUser creds = let f  [] = Error "Invalid credentials"
                             f [u] = Success u
                             f  _  = Error "Multiple users, contact an admin"
                          in f <$> S.runQuery (getUsers creds)

-- | Remove authenticated status from the session
processLogout :: TTNAction ctx a
processLogout = do S.modifySession $ \s -> s { sessUser = Nothing }
                   renderSimpleStr "You have been logged out."

-- * Get current user

getLoggedInUID :: () -> TTNAction ctx (Result Text Int)
getLoggedInUID _ = do u <- sessUser <$> S.readSession
                      return $ case u of -- TODO: this is not quite right
                                 Nothing -> Error "Not logged in"
                                 Just u' -> Success $ uID u'

