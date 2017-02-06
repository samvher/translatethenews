{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TTN.Model.Article where

import TTN.Util                         ( innerZip, maybeRead )

import Data.Maybe                       ( fromMaybe, listToMaybe )
import Data.Text                        ( Text, pack, unpack )
import Database.PostgreSQL.Simple.SqlQQ ( sql )
import Web.PathPieces

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToRow as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg

-- Language

data Language = English
              | Russian
              | Turkish
              | Indonesian
                deriving ( Read, Show )

instance Pg.FromField Language where
    fromField f dat = read . unpack <$> Pg.fromField f dat

instance Pg.ToField Language where
    toField = Pg.toField . pack . show

instance PathPiece Language where
    fromPathPiece = maybeRead . unpack
    toPathPiece   = pack . show

langCode :: Language -> Text
langCode English = "en"
langCode Russian = "ru"

-- Helper types (as phantoms)

data Stored = Stored
data UnStored = UnStored

-- Article

data Article a =
    Article {
      artID       :: Maybe Int,
      artPubDate  :: Text,
      artTitle    :: Text,
      artAuthor   :: Text,
      artURL      :: Text,
      artSummary  :: Maybe Text,
      artOrigLang :: Language,
      artBody     :: [[(Int, Text)]]
    } deriving ( Read, Show )

artLangAsText :: Article a -> Text
artLangAsText = pack . show . artOrigLang

markStored :: Article a -> Article Stored
markStored Article {..} = Article {..} -- Using RecordWildCards

instance Pg.FromRow (Article Stored) where
    fromRow = do id          <- Pg.field
                 pub_date    <- Pg.field
                 title       <- Pg.field
                 author      <- Pg.field
                 url         <- Pg.field
                 summary     <- Pg.field
                 orig_lang   <- Pg.field
                 body        <- Pg.field
                 return Article {
                          artID       = id,
                          artPubDate  = pub_date,
                          artTitle    = title,
                          artAuthor   = author,
                          artURL      = url,
                          artSummary  = summary,
                          artOrigLang = orig_lang,
                          artBody     = fromMaybe [] $ maybeRead body }

instance Pg.ToRow (Article UnStored) where
    toRow a = Pg.toRow ( artPubDate a
                       , artTitle a
                       , artAuthor a
                       , artURL a
                       , artSummary a
                       , artOrigLang a
                       , show $ artBody a)

instance Pg.ToRow (Article Stored) where
    toRow a = Pg.toRow ( artPubDate a
                       , artTitle a
                       , artAuthor a
                       , artURL a
                       , artSummary a
                       , artOrigLang a
                       , show $ artBody a 
                       , artID a ) -- ^ At the end for UPDATE :/

sqlAddArticle :: Pg.Query
sqlAddArticle =
    [sql| INSERT INTO articles
                        (pub_date, title, author, url, summary, orig_lang, body)
                 VALUES (?, ?, ?, ?, ?, ?, ?)
                 RETURNING * |]

insertArticle :: Article UnStored
              -> Pg.Connection
              -> IO (Article Stored)
insertArticle art dbConn = do [a] <- Pg.query dbConn sqlAddArticle art
                              return a

sqlEditArticle :: Pg.Query
sqlEditArticle =
    [sql| UPDATE articles SET pub_date = ?
                            , title = ?
                            , author = ?
                            , url = ?
                            , summary = ?
                            , orig_lang = ?
                            , body = ?
          WHERE id = ?
          RETURNING * |]

updateArticle :: Article Stored -> Pg.Connection -> IO (Article Stored)
updateArticle art dbConn = do [a] <- Pg.query dbConn sqlEditArticle art
                              return a

sqlGetArticle :: Pg.Query
sqlGetArticle = [sql| SELECT * FROM articles WHERE id = ? |]

getArticleById :: Int -> Pg.Connection -> IO (Maybe (Article Stored))
getArticleById aID dbConn = do
    (as :: [Article Stored]) <- Pg.query dbConn sqlGetArticle (Pg.Only aID)
    return $ listToMaybe as

sqlListArticles :: Pg.Query
sqlListArticles = [sql| SELECT * FROM articles |]

getArticleList :: Pg.Connection -> IO [Article Stored]
getArticleList dbConn = Pg.query dbConn sqlListArticles ()

-- Translation

data Translation =
    Translation {
      trID      :: Maybe Int,
      trAID     :: Int,
      trUID     :: Int,
      trLang    :: Language,
      trTitle   :: Text,
      trSummary :: Maybe Text,
      trBody    :: [[(Int, Text)]]
    } deriving ( Read, Show )

instance Pg.FromRow Translation where
    fromRow = do id      <- Pg.field
                 aID     <- Pg.field
                 uID     <- Pg.field
                 lang    <- Pg.field
                 title   <- Pg.field
                 summary <- Pg.field
                 body    <- Pg.field
                 return Translation { trID = id
                                    , trAID = aID
                                    , trUID = uID
                                    , trLang = lang
                                    , trTitle = title
                                    , trSummary = summary
                                    , trBody = fromMaybe [] $ maybeRead body }

instance Pg.ToRow Translation where
    toRow t = Pg.toRow ( trAID t
                       , trUID t
                       , trLang t
                       , trTitle t
                       , trSummary t
                       , show $ trBody t )

sqlAddTranslation :: Pg.Query
sqlAddTranslation =
    [sql| INSERT INTO translations
                        (article_id, contributor_id, trans_lang, title, summary, body)
                 VALUES (?, ?, ?, ?, ?, ?)
                 RETURNING * |]

insertTranslation :: Translation -> Pg.Connection -> IO Translation
insertTranslation tr dbConn = do [t] <- Pg.query dbConn sqlAddTranslation tr
                                 return t

sqlGetTranslations :: Pg.Query
sqlGetTranslations = [sql| SELECT * FROM translations
                             WHERE article_id = ? AND trans_lang = ? |]

getTranslations :: Int -> Language -> Pg.Connection -> IO [Translation]
getTranslations aID lang dbConn = Pg.query dbConn sqlGetTranslations (aID, lang)

-- Dealing with the body of these types

bodyAsParagraphs :: [[(Int, Text)]] -> [Text]
bodyAsParagraphs = map (T.intercalate " " . map snd)

bodyAsText :: [[(Int, Text)]] -> Text
bodyAsText = T.intercalate "\n\n" . bodyAsParagraphs

textToBody :: Text -> [[(Int, Text)]]
textToBody = innerZip [0..] . body
  where body = map sentences . paragraphs
        sentences = map T.strip . putBackPeriods . T.splitOn ". "
        putBackPeriods xs = map (`T.append` ".") (init xs) ++ [last xs]
        paragraphs = filter (not . T.null) . map T.strip . T.lines

