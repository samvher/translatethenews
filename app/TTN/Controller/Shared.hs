{-|
Module      : TTN.Controller.Core
Description : Diverse code for working in Spock which is not particularly
              related to Articles or authentication.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE ScopedTypeVariables #-}

module TTN.Controller.Shared where

import TTN.Model.Core
import TTN.View.Core
import TTN.View.Shared

import Control.Exception                ( SomeException, try )
import Control.Monad.IO.Class           ( liftIO )
import Data.Text                        ( Text )
import Data.Time.Clock                  ( UTCTime, getCurrentTime )
import Network.HTTP.Types.Status        ( status403 )
import Text.Digestive.Form              ( Form )
import Web.Spock.Digestive              ( runForm )

import qualified Database.PostgreSQL.Simple as Pg
import qualified Web.Spock as S

-- | Serve access denied page
noAccess :: TTNView ctx () -> TTNAction ctx a
noAccess msg = do S.setStatus status403
                  renderSimpleHtml msg

-- | Takes an internal label for a form, digestive-functors form
--   description, a renderer for turning the view into HTML (see the
--   FormRenderer type alias in TTN.View.Core), and an action to perform on
--   the output of the form after successfully processing it.
serveForm :: Text
          -> Form Text (TTNAction ctx) a
          -> FormRenderer ctx
          -> (a -> TTNAction ctx b)
          -> TTNAction ctx b
serveForm label form renderer successAction = do
    (view, l) <- runForm label form
    case l of
      Nothing -> renderSimpleForm renderer view
      Just x  -> successAction x

-- | Safely run a query (showing an error page in case of exceptions).
--   We have to use forall a ctx to play nice with ScopedTypeVariables,
--   and the type declaration of q'.
runQuerySafe :: forall a ctx. (Pg.Connection -> IO a) -> TTNAction ctx a
runQuerySafe q = do result <- S.runQuery q'
                    case result of
                      Left  err -> renderSimpleStr $ errorStr ++ show err
                      Right res -> return res
  where q' conn = try (q conn) :: IO (Either SomeException a)
        errorStr = "Sorry, a database error occurred: "

-- | Get current time (use in validators in forms)
now :: TTNAction ctx UTCTime
now = liftIO getCurrentTime

