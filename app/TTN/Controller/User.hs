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

import TTN.Hidden                       ( salt )
import TTN.Routes
import TTN.Util                         ( checkNE
                                        , emailP
                                        , testPattern )

import TTN.Controller.Shared
import TTN.Model.Article
import TTN.Model.Core
import TTN.Model.User
import TTN.View.Core
import TTN.View.User

import Crypto.Hash.SHA256               ( hash )
import Data.HVect                       ( HVect(..) )
import Data.Text                        ( Text, append, pack )
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
                Nothing   -> noAccess mustLogin
                Just user -> return (user :&: oldCtx)

-- | Pages behind this hook require that the visitor is not logged in
guestOnlyHook :: TTNAction (HVect xs) (HVect (IsGuest ': xs))
guestOnlyHook = do oldCtx      <- S.getContext
                   logInStatus <- visitorLoggedIn
                   if logInStatus
                     then noAccess loggedIn
                     else return (IsGuest :&: oldCtx)

-- * Password hashing

-- | Use SHA256 and some trivial transformations
encodePass :: Text -> Text
encodePass = pack . show . hash . encodeUtf8 . append salt

-- * Registration

-- TODO: Confirmation email
-- | Process and serve registration form, add a successful registration to
--   the database.
processRegistration :: TTNAction ctx a
processRegistration =
    serveForm "register" registerForm renderRegisterForm $ \u ->
        do runQuerySafe $ insertUser u
           S.redirect loginPath -- TODO: Add some sort of notification

-- | Registration form
registerForm :: Form Text (TTNAction ctx) (Text, Text, Text)
registerForm = "register" .: checkM nonUniqueMsg uniqueness ( prepUser
    <$> "username" .: check "No username supplied" checkNE (text Nothing)
    <*> "email"    .: check "Email not valid" (testPattern emailP) (text Nothing)
    <*> "password" .: check "No password supplied" checkNE (text Nothing))
  where nonUniqueMsg = "Username or email already registered"
        uniqueness (n, e, _) = runQuerySafe $ isUnique (n, e)
        prepUser n e p = (n, e, encodePass p)

-- * User profile

-- TODO: Type-safe authentication, use a notification for successful edit
-- | Deal with user profile edits, for profile of logged in user
editProfile :: TTNAction ctx a
editProfile = do user <- getLoggedInUser
                 let profileForm   = mkProfileForm user
                     renderer      = renderProfileForm profilePath
                     gotoProfile u = do S.modifySession $ \s ->
                                            s { sessUser = Just u }
                                        S.redirect profilePath
                 serveForm "profile" profileForm renderer gotoProfile

mkProfileForm :: User -> Form Text (TTNAction ctx) User
mkProfileForm user = "profile" .: validateM writeToDb ( mkUser
    <$> "email"       .: check "Email not valid"
                               (testPattern emailP)
                               (text . Just $ uEmail user)
    <*> "read-langs"  .: choiceMultiple allLangs (Just defReadLangs)
    <*> "trans-langs" .: choiceMultiple allLangs (Just $ uTransLangs user))
  where mkUser e r t = user { uEmail      = e
                            , uReadLangs  = r
                            , uTransLangs = t }
        writeToDb u = Success <$> runQuerySafe (updateUser u)
        allLangs = zip allLanguages $ map (pack . show) allLanguages
        defReadLangs = uReadLangs user

-- * Login/logout

-- | Process and serve login form, register successful login in the
--   session.
processLogin :: TTNAction ctx a
processLogin = serveForm "login" loginForm renderLoginForm $ \u ->
                 do S.modifySession ( \s -> s { sessUser = Just u } )
                    S.redirect rootPath

-- | Login form
loginForm :: Form Text (TTNAction ctx) User
loginForm = "login" .: validateM findUser (readCreds
    <$> "username" .: check "No username supplied" checkNE (text Nothing)
    <*> "password" .: check "No password supplied" checkNE (text Nothing))
  where readCreds u p  = (u, encodePass p)
        findUser creds = let f  [] = Error "Invalid credentials"
                             f [u] = Success u
                             f  _  = Error "Multiple users, contact an admin"
                          in f <$> runQuerySafe (getUsers creds)

-- | Remove authenticated status from the session
processLogout :: TTNAction ctx a
processLogout = do S.modifySession $ \s -> s { sessUser = Nothing }
                   renderSimpleStr "You have been logged out."

-- * Get current user

-- TODO: Use type safe authentication check
getLoggedInUser :: TTNAction ctx User
getLoggedInUser = do
    currentUser <- sessUser <$> S.readSession
    maybe (renderSimpleStr "Not logged in!") return currentUser

-- | Weird formulation because it is used as a Form validator
getLoggedInUID :: () -> TTNAction ctx (Result Text Int)
getLoggedInUID _ = do u <- sessUser <$> S.readSession
                      maybe (renderSimpleStr "Not logged in")
                            (return . Success . uID)
                            u

