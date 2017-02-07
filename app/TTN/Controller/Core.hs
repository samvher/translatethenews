{-|
Module      : TTN.Controller.Core
Description : Diverse code for working in Spock which is not particularly
              related to Articles or authentication.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TTN.Controller.Core where

import TTN.Model.Core
import TTN.View.Core

import Control.Exception                ( SomeException, try )
import Data.HVect                       ( HVect(..) )
import Data.Text                        ( Text )
import Network.HTTP.Types.Status        ( status403 )
import Text.Digestive.Form              ( Form )
import Web.Spock.Digestive              ( runForm )

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

-- | Serve access denied page
noAccess :: String -> TTNAction ctx a
noAccess msg = do S.setStatus status403
                  renderSimpleStr msg

-- | For working with type-safe authentication
initHook :: TTNAction () (HVect '[])
initHook = return HNil

-- | Takes an internal label for a form, digestive-functors form
--   description, a renderer for turning the view into HTML (see the
--   FormRenderer type alias in TTN.View.Core), and an action to perform on
--   the output of the form after successfully processing it. Takes out
--   some of the pain of working with CSRF tokens.
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

-- | Safely run a query (redirecting to an error page in case of exceptions).
--   We have to use forall a ctx to play nice with ScopedTypeVariables,
--   and the type declaration of q'.
runQuerySafe :: forall a ctx. (Pg.Connection -> IO a) -> TTNAction ctx a
runQuerySafe q = do result <- S.runQuery q'
                    case result of
                      Left  _   -> renderSimpleStr errorStr
                      Right res -> return res
  where q' conn = try (q conn) :: IO (Either SomeException a)
        errorStr = "Sorry, a database error occurred."

-- | Hello world page
hello :: TTNAction ctx a
hello = renderSimpleStr "Привет, мир!"
