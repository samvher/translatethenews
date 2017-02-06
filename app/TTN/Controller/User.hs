{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TTN.Controller.User where

import TTN.Util ( checkNE, emailP, testPattern )

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

-- Authentication stuff

visitorLoggedIn :: TTNAction ctx Bool
visitorLoggedIn = do u <- sessUser <$> S.readSession
                     return $ case u of
                       Nothing -> False
                       Just _  -> True

authHook :: TTNAction (HVect xs) (HVect (User ': xs))
authHook = do oldCtx <- S.getContext
              u      <- sessUser <$> S.readSession
              case u of
                Nothing   -> noAccess "Sorry, no access! Log in first."
                Just user -> return (user :&: oldCtx)

guestOnlyHook :: TTNAction (HVect xs) (HVect (IsGuest ': xs))
guestOnlyHook = do oldCtx     <- S.getContext
                   loggedIn   <- visitorLoggedIn
                   if loggedIn
                     then noAccess "You're already logged in!"
                     else return (IsGuest :&: oldCtx)

-- Registration and login stuff

encodePass :: Text -> Text
encodePass = pack . show . hash . encodeUtf8

-- Registration

-- TODO: Confirmation email
processRegistration :: TTNAction ctx a
processRegistration = serveForm "register" registerForm renderRegister $ \u ->
                        do S.runQuery $ insertUser u
                           renderSimpleStr $ show u

registerForm :: Form Text (TTNAction ctx) User
registerForm = "register" .: checkM nonUniqueMsg uniqueness (mkUser
    <$> "uid"      .: pure 0
    <*> "username" .: check "No username supplied" checkNE (text Nothing)
    <*> "email"    .: check "Email not valid" (testPattern emailP) (text Nothing)
    <*> "password" .: check "No password supplied" checkNE (text Nothing))
  where nonUniqueMsg = "Username or email already registered"
        uniqueness u = null <$> S.runQuery (testUniqueness (uName u, uEmail u))
        mkUser i u e p = User { uID = i
                              , uName = u
                              , uEmail = e
                              , uPassHash = encodePass p }

-- Login

processLogin :: TTNAction ctx a
processLogin = serveForm "login" loginForm renderLogin $ \u ->
                 do S.modifySession $ \s -> s { sessUser = Just u }
                    renderSimpleStr $ show u

loginForm :: Form Text (TTNAction ctx) User
loginForm = "login" .: validateM findUser (readCreds
    <$> "username" .: check "No username supplied" checkNE (text Nothing)
    <*> "password" .: check "No password supplied" checkNE (text Nothing))
  where readCreds u p  = (u, encodePass p)
        findUser creds = let f  [] = Error "Invalid credentials"
                             f [u] = Success u
                             f  _  = Error "Multiple users, contact an admin"
                          in f <$> S.runQuery (getUsers creds)

processLogout :: TTNAction ctx a
processLogout = do S.modifySession $ \s -> s { sessUser = Nothing }
                   renderSimpleStr "You have been logged out."
