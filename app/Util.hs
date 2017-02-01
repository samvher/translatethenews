module Util where

import Data.Maybe ( listToMaybe )

import qualified Data.Text as T

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

checkNE :: T.Text -> Bool
checkNE = (> 0) . T.length -- non-empty

-- This is not so generic... but not sure where it should go

