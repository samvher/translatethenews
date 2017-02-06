{-# LANGUAGE DataKinds #-}

module TTN.Controller.Core where

import TTN.Model.Core
import TTN.View.Core

import Data.HVect                       ( HVect(..) )
import Data.Text                        ( Text )
import Network.HTTP.Types.Status        ( status403 )
import Text.Digestive.Form              ( Form )
import Web.Spock.Digestive              ( runForm )

import qualified Database.PostgreSQL.Simple as Pg
import qualified Web.Spock as S
import qualified Web.Spock.Config as S

dbConn :: Pg.ConnectInfo -> S.PoolOrConn Pg.Connection
dbConn connInfo = S.PCConn (S.ConnBuilder (Pg.connect connInfo)
                                          Pg.close
                                          (S.PoolCfg 5 5 60))

getCfg :: Pg.ConnectInfo -> IO TTNCfg
getCfg connInfo = do
    cfg' <- S.defaultSpockCfg defSession (dbConn connInfo) defState
    return cfg' { S.spc_csrfProtection = True }

noAccess :: String -> TTNAction ctx a
noAccess msg = do S.setStatus status403
                  renderSimpleStr msg

initHook :: TTNAction () (HVect '[])
initHook = return HNil

serveForm :: Text
          -> Form Text (TTNAction ctx) a
          -> FormRenderer
          -> (a -> TTNAction ctx b)
          -> TTNAction ctx b
serveForm label form renderer successAction = do
    tok       <- S.getCsrfToken
    (view, l) <- runForm label form
    case l of
      Nothing -> renderSimpleForm renderer tok view
      Just x  -> successAction x

hello :: TTNAction ctx a
hello = renderSimpleStr "Привет, мир!"
