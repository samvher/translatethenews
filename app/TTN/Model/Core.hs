module TTN.Model.Core where

import TTN.Model.User

import qualified Web.Spock as S
import qualified Web.Spock.Config as S
import qualified Database.PostgreSQL.Simple as Pg

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

type TTNAction ctx = S.SpockActionCtx ctx Pg.Connection TTNSes TTNSt
type TTNCfg        = S.SpockCfg           Pg.Connection TTNSes TTNSt
type TTNMonad  ctx = S.SpockCtxM      ctx Pg.Connection TTNSes TTNSt ()

