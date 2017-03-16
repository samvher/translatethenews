{-|
Module      : TTN.Model.User
Description : Model code for User.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module TTN.Model.User where

import TTN.Model.Language

import Data.Text                        ( Text )

import Database.Persist
import Database.Persist.TH

-- * User

share [mkPersist sqlSettings, mkMigrate "migrateUser"] [persistLowerCase|
User sql=users
  name       Text sql=name
  email      Text sql=email
  pass       Text sql=password
  readLangs  [Language] sql=read_langs default='[]'
  transLangs [Language] sql=trans_langs default='[]'
  Id sql=id
  UniqueUsername name
  UniqueEmail email
  deriving Read Show
|]

userID :: Entity User -> Key User
userID (Entity id _) = id

entUser :: Entity User -> User
entUser (Entity _ u) = u

-- | For type-safe authentication
data IsGuest = IsGuest

