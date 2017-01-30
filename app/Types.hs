{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import Data.Text                        ( Text , pack )
import Lucid
import Text.Digestive.View
import Web.Spock                        hiding ( head, text )
import Web.Spock.Config

import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToRow as Pg

-- Session info

data TTNSes =
    TTNSes {
      sessUser :: Maybe User
    } deriving ( Read, Show )

defSession :: TTNSes
defSession =
    TTNSes {
      sessUser = Nothing
    }

-- State info

data TTNSt = TTNSt ()

defState :: TTNSt
defState = TTNSt ()

-- Spock types for our use

type TTNAction ctx = SpockActionCtx ctx Pg.Connection TTNSes TTNSt
type TTNCfg        = SpockCfg           Pg.Connection TTNSes TTNSt
type TTNMonad  ctx = SpockCtxM      ctx Pg.Connection TTNSes TTNSt ()

-- User constructor

data User = User Text Text Text
            deriving ( Read, Show )

instance Pg.FromRow User where
    fromRow = do (userId :: Int) <- Pg.field
                 name            <- Pg.field
                 email           <- Pg.field
                 passHash        <- Pg.field
                 return (User name email passHash)

instance Pg.ToRow User where
    toRow (User name email passHash) = Pg.toRow (name, email, passHash)

-- Useful aliases

type Token = Text

type FormRenderer = Token -> View (Html ()) -> Html ()
