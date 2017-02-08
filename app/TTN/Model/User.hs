{-|
Module      : TTN.Model.User
Description : Interface with the PSQL database for User-related types.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TTN.Model.User where

import Control.Monad                    ( void )
import Data.Maybe                       ( listToMaybe )
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
      uEmail    :: Text
    } deriving ( Read, Show )

instance Pg.FromRow User where
    fromRow = do userId          <- Pg.field
                 name            <- Pg.field
                 email           <- Pg.field
                 return (User userId name email)

-- | For type-safe authentication
data IsGuest = IsGuest

sqlAddUser :: Pg.Query
sqlAddUser =
    [sql| INSERT INTO users (name, email, password) VALUES (?, ?, ?) |]

insertUser :: (Text, Text, Text) -> Pg.Connection -> IO ()
insertUser user dbConn = void $ Pg.execute dbConn sqlAddUser user

sqlTestUniqueness :: Pg.Query
sqlTestUniqueness = [sql| SELECT COUNT(*)
                          FROM users WHERE name = ? OR email = ? |]

-- | Check that given name or emailaddress has not been used before
isUnique :: (Text, Text) -> Pg.Connection -> IO Bool
isUnique vals dbConn = do
    (counts :: [Pg.Only Int]) <- Pg.query dbConn sqlTestUniqueness vals
    return . (== 0) . Pg.fromOnly $ head counts

sqlGetUser :: Pg.Query
sqlGetUser = [sql| SELECT id, name, email
                   FROM users WHERE name = ? AND password = ? |]

getUsers :: (Text, Text) -> Pg.Connection -> IO [User]
getUsers creds dbConn = Pg.query dbConn sqlGetUser creds

sqlGetUserById :: Pg.Query
sqlGetUserById = [sql| SELECT id, name, email
                       FROM users WHERE id = ? |]

getUserById :: Int -> Pg.Connection -> IO (Maybe User)
getUserById uID dbConn =
    listToMaybe <$> Pg.query dbConn sqlGetUserById (Pg.Only uID)

