{-|
Module      : TTN.Controller.Core
Description : Diverse code for working in Spock which is not particularly
              related to Articles or authentication.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE DataKinds #-}

module TTN.Controller.Core where

import TTN.Model.Core

import Data.HVect                       ( HVect(..) )

import qualified Database.PostgreSQL.Simple as Pg
import qualified Web.Spock as S
import qualified Web.Spock.Config as S

-- | Spock DB connection pool
dbConn :: Pg.ConnectInfo -> S.PoolOrConn Pg.Connection
dbConn connInfo = S.PCConn (S.ConnBuilder (Pg.connect connInfo)
                                          Pg.close
                                          (S.PoolCfg 5 5 60))

-- | Spock configuration
getCfg :: Pg.ConnectInfo -> IO TTNCfg
getCfg connInfo = do
    cfg' <- S.defaultSpockCfg defSession (dbConn connInfo) defState
    return cfg' { S.spc_csrfProtection = True }

initHook :: TTNAction () (HVect '[])
initHook = return HNil

