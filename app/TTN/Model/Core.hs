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

-- * More aliases but for the templating system
--
-- | Just an HtmlT monad with access to the session
type TTNView   ctx   = HtmlT (TTNAction ctx)

-- | A blockdef function defines the structure of different blocks in the
--   template. Each constructor in TTNBlock corresponds to one type of
--   block in the template, and a blockdef function must take any of these
--   constructors and return a valid piece of TTNView machinery.
type TTNBlockDef ctx = TTNBlock -> TTNView ctx ()

data TTNBlock = TTNPageTitle -- ^ <title>
              | TTNNavBar    -- ^ Which buttons to show
              | TTNContent   -- ^ Main content area

-- | It has a ReaderT layer but we will only interact with it with
--   runTemplate and getBlock.
type TTNTemplate ctx = HtmlT (ReaderT (TTNBlockDef ctx) (TTNAction ctx)) ()

-- | Take the given template and blockdef and fill in the blocks.
runTemplate :: TTNTemplate ctx -> TTNBlockDef ctx -> TTNView ctx ()
runTemplate template blockDef = hoist (`runReaderT` blockDef) template

-- | Retrieve the TTNView machinery for the given block.
getBlock :: TTNBlock -> TTNTemplate ctx
getBlock blockname = do
    f <- lift ask
    hoist lift $ f blockname

