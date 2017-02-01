module Util where

import Types

import Data.Maybe ( listToMaybe )
import Web.Spock

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

checkNE :: T.Text -> Bool
checkNE = (> 0) . T.length -- non-empty

-- This is not so generic... but not sure where it should go

runQuery' :: Pg.FromRow a => Pg.Query -> TTNAction ctx [a]
runQuery' q = runQuery $ \c -> Pg.query c q ()
