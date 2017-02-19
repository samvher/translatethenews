{-|
Module      : TTN.Model.User
Description : Interface with the PSQL database for User-related types.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TTN.Model.User where

import TTN.Model.Article                ( Language )
import TTN.Util                         ( maybeRead )

import Control.Monad                    ( void )
import Data.Maybe                       ( fromMaybe
                                        , listToMaybe )
import Data.Text                        ( Text )
import Database.PostgreSQL.Simple.SqlQQ ( sql )

import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToRow as Pg

-- * User

data User =
    User {
      uID         :: Int,
      uName       :: Text,
      uEmail      :: Text,
      uReadLangs  :: [Language],
      uTransLangs :: [Language]
    } deriving ( Read, Show )

instance Pg.FromRow User where
    fromRow = do userId          <- Pg.field
                 name            <- Pg.field
                 email           <- Pg.field
                 readLangs       <- Pg.field
                 transLangs      <- Pg.field
                 return User {
                          uID = userId
                        , uName = name
                        , uEmail = email
                        , uReadLangs = fromMaybe [] $ maybeRead readLangs
                        , uTransLangs = fromMaybe [] $ maybeRead transLangs }

-- | This is only used for UPDATE statements
instance Pg.ToRow User where
    toRow u = Pg.toRow ( uEmail u
                       , show $ uReadLangs u
                       , show $ uTransLangs u
                       , uID u )

-- | For type-safe authentication
data IsGuest = IsGuest

-- | When registering only these data are taken
sqlAddUser :: Pg.Query
sqlAddUser =
    [sql| INSERT INTO users (name, email, password, read_langs, trans_langs)
          VALUES (?, ?, ?, '[]', '[]') |]

insertUser :: (Text, Text, Text) -> Pg.Connection -> IO ()
insertUser user dbConn = void $ Pg.execute dbConn sqlAddUser user

sqlTestUniqueness :: Pg.Query
sqlTestUniqueness = [sql| SELECT COUNT(*)
                          FROM users WHERE name = ? OR email = ? |]

-- | Other user profile fields can only be changed later, but at this point
--   the username is uneditable.
sqlEditUser :: Pg.Query
sqlEditUser =
    [sql| UPDATE users SET email = ?
                         , read_langs = ?
                         , trans_langs = ?
          WHERE id = ?
          RETURNING id, name, email, read_langs, trans_langs |]

updateUser :: User -> Pg.Connection -> IO User
updateUser usr dbConn = do [u] <- Pg.query dbConn sqlEditUser usr
                           return u

-- | Check that given name or emailaddress has not been used before
isUnique :: (Text, Text) -> Pg.Connection -> IO Bool
isUnique vals dbConn = do
    (counts :: [Pg.Only Int]) <- Pg.query dbConn sqlTestUniqueness vals
    return . (== 0) . Pg.fromOnly $ head counts

sqlGetUser :: Pg.Query
sqlGetUser = [sql| SELECT id, name, email, read_langs, trans_langs
                   FROM users WHERE name = ? AND password = ? |]

getUsers :: (Text, Text) -> Pg.Connection -> IO [User]
getUsers creds dbConn = Pg.query dbConn sqlGetUser creds

sqlGetUserById :: Pg.Query
sqlGetUserById = [sql| SELECT id, name, email, read_langs, trans_langs
                       FROM users WHERE id = ? |]

getUserById :: Int -> Pg.Connection -> IO (Maybe User)
getUserById uID dbConn =
    listToMaybe <$> Pg.query dbConn sqlGetUserById (Pg.Only uID)

