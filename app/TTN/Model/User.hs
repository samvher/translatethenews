{-|
Module      : TTN.Model.User
Description : Interface with the PSQL database for User-related types.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE QuasiQuotes #-}

module TTN.Model.User where

import Control.Monad                    ( void )
import Data.Text                        ( Text )
import Database.PostgreSQL.Simple.SqlQQ ( sql )

import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToRow as Pg

-- * User

data User =
    User {
      uID       :: Int,
      uName     :: Text,
      uEmail    :: Text,
      uPassHash :: Text -- ^ Generated with encodePass (in TTN.Controller.User)
    } deriving ( Read, Show )

instance Pg.FromRow User where
    fromRow = do userId          <- Pg.field
                 name            <- Pg.field
                 email           <- Pg.field
                 passHash        <- Pg.field
                 return (User userId name email passHash)

-- | User ID is determined by the DB, we don't supply it
instance Pg.ToRow User where
    toRow (User _ name email passHash) = Pg.toRow (name, email, passHash)

-- | For type-safe authentication
data IsGuest = IsGuest

sqlAddUser :: Pg.Query
sqlAddUser =
    [sql| INSERT INTO users (name, email, password) VALUES (?, ?, ?) |]

insertUser :: User -> Pg.Connection -> IO ()
insertUser user dbConn = void $ Pg.execute dbConn sqlAddUser user

-- TODO: Check username/email separately
sqlTestUniqueness :: Pg.Query
sqlTestUniqueness = [sql| SELECT * FROM users WHERE name = ? OR email = ? |]

-- | Check that given name or emailaddress has not been used before
testUniqueness :: (Text, Text) -> Pg.Connection -> IO [User]
testUniqueness vals dbConn = Pg.query dbConn sqlTestUniqueness vals

sqlGetUser :: Pg.Query
sqlGetUser = [sql| SELECT * FROM users WHERE name = ? AND password = ? |]

getUsers :: (Text, Text) -> Pg.Connection -> IO [User]
getUsers creds dbConn = Pg.query dbConn sqlGetUser creds

