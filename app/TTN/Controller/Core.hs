{-|
Module      : TTN.Controller.Core
Description : Diverse code for working in Spock which is not particularly
              related to Articles or authentication.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE DataKinds #-}

module TTN.Controller.Core where

import TTN.Util

import TTN.Model.Article
import TTN.Model.Core
import TTN.Model.Translation
import TTN.Model.User
import TTN.View.Core
import TTN.View.Shared

import Control.Monad.Logger             ( runNoLoggingT )
import Control.Monad.IO.Class           ( liftIO )
import Data.HVect                       ( HVect(..) )
import Data.Text                        ( pack )

import qualified Database.Persist.Postgresql as Pg
import qualified Web.Spock as S
import qualified Web.Spock.Config as S

-- | Spock configuration
getCfg :: Pg.ConnectionString -> IO TTNCfg
getCfg connString = do
    -- Set up connection for Persistent
    pool <- runNoLoggingT $ Pg.createPostgresqlPool connString 5
    runNoLoggingT . flip Pg.runSqlPool pool $ do
        Pg.runMigration migrateUser
        Pg.runMigration migrateArticle
        Pg.runMigration migrateTranslation
    -- Set up Spock config
    cfg' <- S.defaultSpockCfg defSession (S.PCPool pool) defState
    -- Enable CSRF protection
    return cfg' { S.spc_csrfProtection = True }

initHook :: TTNAction () (HVect '[])
initHook = return HNil

welcomePage :: TTNAction ctx a
welcomePage = do
 -- TODO: some systematic location and wrapper
    welcome <- liftIO $ readFile "WELCOME.md"
    renderSimpleHtml . hr . pack $ mdToHtml welcome

