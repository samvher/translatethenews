{-# LANGUAGE OverloadedStrings #-}

module Main where

import ConnInfo ( ttnConnInfo )

import Data.Text.Lazy ( toStrict )
import Lucid
import Web.Spock
import Web.Spock.Config

import qualified Database.PostgreSQL.Simple as Pg

-- Session info

data TTNSes = TTNSes ()

defSession :: TTNSes
defSession = TTNSes ()

-- State info

data TTNSt = TTNSt ()

defState :: TTNSt
defState = TTNSt ()

-- PostgreSQL info
-- Login info in ConnInfo.hs in this directory (gitignored)

dbConn :: PoolOrConn Pg.Connection
dbConn = PCConn (ConnBuilder (Pg.connect ttnConnInfo)
                             Pg.close
                             (PoolCfg 5 5 60))

-- The web app

getCfg :: IO (SpockCfg Pg.Connection TTNSes TTNSt)
getCfg = do cfg' <- defaultSpockCfg defSession dbConn defState
            return cfg' { spc_csrfProtection = True }

app :: SpockM Pg.Connection session state ()
app = get root $ lucid hello

main :: IO ()
main = do cfg <- getCfg
          runSpock 3000 (spock cfg app)

-- Some functions for view

pageTemplate :: Html () -> Html ()
pageTemplate contents = html_ (do head_ (title_ "Translate the News")
                                  body_ contents)

lucid :: Html () -> SpockAction database session state ()
lucid document = html (toStrict (renderText document))

-- Pages

hello :: Html ()
hello = pageTemplate $ h1_ "Hello world!"
