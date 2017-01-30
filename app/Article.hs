{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Article where

import Control.Monad
import Data.Text
import Data.Time.Clock
import Database.PostgreSQL.Simple.SqlQQ ( sql )

import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToRow as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg

data Language = English
              | Russian
              | Turkish
              | Indonesian
                deriving ( Read, Show )

instance Pg.FromField Language where
    fromField f dat = read . unpack <$> Pg.fromField f dat

instance Pg.ToField Language where
    toField = Pg.toField . pack . show

data Article =
    Article {
      artPubDate  :: UTCTime,
      artTitle    :: Text,
      artAuthor   :: Text,
      artURL      :: Text,
      artSummary  :: Maybe Text,
      artOrigLang :: Language
    } deriving ( Read, Show )

instance Pg.FromRow Article where
    fromRow = do (id :: Int) <- Pg.field
                 pub_date    <- Pg.field
                 title       <- Pg.field
                 author      <- Pg.field
                 url         <- Pg.field
                 summary     <- Pg.field
                 orig_lang   <- Pg.field
                 return Article { artPubDate  = pub_date,
                                  artTitle    = title,
                                  artAuthor   = author,
                                  artURL      = url,
                                  artSummary  = summary,
                                  artOrigLang = orig_lang }

instance Pg.ToRow Article where
    toRow a = Pg.toRow ( artPubDate a
                       , artTitle a
                       , artAuthor a
                       , artURL a
                       , artSummary a
                       , artOrigLang a )

sqlAddArticle :: Pg.Query
sqlAddArticle =
    [sql| INSERT INTO articles
                        (pub_date, title, author, url, summary, orig_lang)
                 VALUES (?, ?, ?, ?, ?, ?) |]

insertArticle :: Article -> Pg.Connection -> IO ()
insertArticle art dbConn = void $ Pg.execute dbConn sqlAddArticle art

