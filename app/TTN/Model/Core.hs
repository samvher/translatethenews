{-|
Module      : TTN.Model.Core
Description : Defines the TTN monads and session and state types.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

module TTN.Model.Core where

import TTN.Model.User

import Control.Monad.Morph                          ( hoist )
import Control.Monad.Trans.Class                    ( lift )
import Control.Monad.Trans.Reader                   ( ReaderT(..)
                                                    , ask
                                                    , runReaderT )
import Lucid                                        ( HtmlT )

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

type TTNView   ctx   = HtmlT (TTNAction ctx)
type TTNBlockDef ctx = TTNBlock -> TTNView ctx ()
type TTNTemplate ctx = HtmlT (ReaderT (TTNBlockDef ctx) (TTNAction ctx)) ()

data TTNBlock = TTNPageTitle
              | TTNNavBar
              | TTNContent

runTemplate :: TTNBlockDef ctx -> TTNTemplate ctx -> TTNView ctx ()
runTemplate f = hoist (`runReaderT` f)

getBlock :: TTNBlock -> TTNTemplate ctx
getBlock blockname = do
    f <- lift ask
    hoist lift $ f blockname

