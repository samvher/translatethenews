{-|
Module      : TTN.Controller.User
Description : Controller code for authentication-related issues.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
import TTN.Model.Language
import TTN.Model.User
import TTN.View.Core
import TTN.View.User

import Control.Monad                    ( mplus )
import Crypto.Hash.SHA256               ( hash )
import Data.HVect                       ( HVect(..) )
import Data.Maybe                       ( isJust, isNothing )
import Data.Text                        ( Text, append, pack )
import Data.Text.Encoding               ( encodeUtf8 )

import Text.Digestive.Form
import Text.Digestive.Types

import Database.Persist
import Database.Persist.Sql

import qualified Web.Spock as S

-- * Type-safe authentication functions

-- | Test if the current visitor is logged in
visitorLoggedIn :: TTNAction ctx Bool
visitorLoggedIn = isJust . sessUser <$> S.readSession

-- | Pages behind this hook require authentication
authHook :: TTNAction (HVect xs) (HVect (Entity User ': xs))
authHook = do oldCtx <- S.getContext
              u      <- sessUser <$> S.readSession
              case u of
                Nothing   -> noAccess mustLogin
                Just user -> return (user :&: oldCtx)

-- | Pages behind this hook require that the visitor is not logged in
guestOnlyHook :: TTNAction (HVect xs) (HVect (IsGuest ': xs))
guestOnlyHook = do oldCtx      <- S.getContext
                   logInStatus <- visitorLoggedIn
                   if logInStatus then noAccess loggedIn
                                  else return (IsGuest :&: oldCtx)

adminOnlyHook :: TTNAction (HVect xs) (HVect (IsAdmin ': xs))
adminOnlyHook = do oldCtx <- S.getContext
                   u      <- sessUser <$> S.readSession
                   case u of
                     Nothing              -> noAccess mustLogin
                     Just (Entity _ user) -> if userName user == "test"
                                                then return (IsAdmin :&: oldCtx)
                                                else noAccess noPermission

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
        do runSQL $ insert u
           S.redirect loginPath -- TODO: Add some sort of notification

-- | Registration form
registerForm :: Form Text (TTNAction ctx) User
registerForm = "register" .: checkM nonUniqueMsg uniqueness ( prepUser
    <$> "username" .: check "No username supplied" checkNE (text Nothing)
    <*> "email"    .: check "Email not valid" (testPattern emailP) (text Nothing)
    <*> "password" .: check "No password supplied" checkNE (text Nothing))
  where nonUniqueMsg = "Username or email already registered"
        uniqueness = runSQL . hasUniqueCreds
        prepUser n e p = User n e (encodePass p) [] []

hasUniqueCreds :: User -> SqlPersistM Bool
hasUniqueCreds (User n e _ _ _) = do
    mUserU <- getBy $ UniqueUsername n
    mUserE <- getBy $ UniqueEmail    e
    return . isNothing $ mUserU `mplus` mUserE

-- * User profile

-- TODO: Type-safe authentication, use a notification for successful edit
-- | Deal with user profile edits, for profile of logged in user
editProfile :: TTNAction ctx a
editProfile = do userEnt <- getLoggedInUser
                 let profileForm      = mkProfileForm $ entUser userEnt
                     renderer         = renderProfileForm profilePath
                     processProfile u = do
                         let uid = userID userEnt
                         new <- runSQL $ do replace uid u
                                            selectFirst [UserId ==. uid] []
                         S.modifySession $ \s -> s { sessUser = new }
                         S.redirect profilePath
                 serveForm "profile" profileForm renderer processProfile

mkProfileForm :: User -> Form Text (TTNAction ctx) User
mkProfileForm user = "profile" .: (mkUser
    <$> "email"       .: check "Email not valid"
                               (testPattern emailP)
                               (text . Just $ userEmail user)
    <*> "read-langs"  .: choiceMultiple allLangs (Just defReadLangs)
    <*> "trans-langs" .: choiceMultiple allLangs (Just $ userTransLangs user))
  where mkUser e r t = user { userEmail      = e
                            , userReadLangs  = r
                            , userTransLangs = t }
        allLangs = zip allLanguages $ map (pack . show) allLanguages
        defReadLangs = userReadLangs user

-- * Login/logout

-- | Process and serve login form, register successful login in the
--   session.
processLogin :: TTNAction ctx a
processLogin = serveForm "login" loginForm renderLoginForm $ \u ->
                 do S.modifySession ( \s -> s { sessUser = Just u } )
                    S.redirect rootPath

-- | Login form
loginForm :: Form Text (TTNAction ctx) (Entity User)
loginForm = "login" .: validateM findUser (readCreds
    <$> "username" .: check "No username supplied" checkNE (text Nothing)
    <*> "password" .: check "No password supplied" checkNE (text Nothing))
  where readCreds u p  = (u, encodePass p)
        findUser creds = do u <- runSQL $ checkLogin creds
                            return $ maybe (Error "Invalid credentials") Success u

checkLogin :: (Text, Text) -> SqlPersistM (Maybe (Entity User))
checkLogin (n, p) = do maybeUser <- getBy (UniqueUsername n)
                       return $ checkPass =<< maybeUser
  where checkPass u = if userPass (entUser u) == p then Just u else Nothing

-- | Remove authenticated status from the session
processLogout :: TTNAction ctx a
processLogout = do S.modifySession $ \s -> s { sessUser = Nothing }
                   renderSimpleStr "You have been logged out."

-- * Get current user

-- TODO: Use type safe authentication check
getLoggedInUser :: TTNAction ctx (Entity User)
getLoggedInUser = do
    currentUser <- sessUser <$> S.readSession
    maybe (renderSimpleStr "Not logged in!") return currentUser

-- | Weird formulation because it is used as a Form validator
getLoggedInUID :: () -> TTNAction ctx (Result Text (Key User))
getLoggedInUID _ = do
    u <- sessUser <$> S.readSession
    maybe (renderSimpleStr "Not logged in") (return . Success . userID) u

