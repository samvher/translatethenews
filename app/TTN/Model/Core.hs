{-|
Module      : TTN.Model.Core
Description : Defines the TTN monads and session and state types.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

module TTN.Model.Core where

import TTN.Model.User

import qualified Web.Spock as S
import qualified Web.Spock.Config as S
import qualified Database.PostgreSQL.Simple as Pg

-- * Session info

data TTNSes =
    TTNSes {
      sessUser :: Maybe User
    } deriving ( Read, Show )

-- | Default session
defSession :: TTNSes
defSession =
    TTNSes {
      sessUser = Nothing
    }

-- * State info

data TTNSt = TTNSt ()

-- | Default state
defState :: TTNSt
defState = TTNSt ()

-- * Spock type aliases

type TTNAction ctx = S.SpockActionCtx ctx Pg.Connection TTNSes TTNSt
type TTNCfg        = S.SpockCfg           Pg.Connection TTNSes TTNSt
type TTNMonad  ctx = S.SpockCtxM      ctx Pg.Connection TTNSes TTNSt ()

