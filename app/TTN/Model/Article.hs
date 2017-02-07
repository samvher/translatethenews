{-|
Module      : TTN.Model.Article
Description : Interface with the PSQL database for Article-related types.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

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
import Lucid                            ( ToHtml(..) )
import Web.PathPieces

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToRow as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg

-- * Language

data Language = English
              | Russian
              | Turkish
              | Indonesian
                deriving ( Read, Show )

allLanguages :: [Language]
allLanguages = [ English
               , Russian
               , Turkish
               , Indonesian ]

instance Pg.FromField Language where
    fromField f dat = read . unpack <$> Pg.fromField f dat

instance Pg.ToField Language where
    toField = Pg.toField . pack . show

instance PathPiece Language where
    fromPathPiece = maybeRead . unpack
    toPathPiece   = pack . show

instance ToHtml Language where
    toHtml    = toHtml . show
    toHtmlRaw = toHtml

-- | This is for use in for example Google Translate URLs
langCode :: Language -> Text
langCode English = "en"
langCode Russian = "ru"
langCode Turkish = "tr"
langCode Indonesian = "id"

-- * Helper types (used as phantoms)

data Stored = Stored
data UnStored = UnStored

-- * Article

data Article a =
    Article {
      artID      :: Maybe Int,
      artUID     :: Int,
      artPubDate :: Text,
      artTitle   :: Text,
      artAuthor  :: Text,
      artURL     :: Text,
      artSummary :: Maybe Text,
      artOrigLang:: Language,
      artBody    :: [[(Int, Text)]],
      artAvTrans :: [Language] -- ^ Available translation languages
    } deriving ( Read, Show )

artLangAsText :: Article a -> Text
artLangAsText = pack . show . artOrigLang

-- | We are falling back to 0 but this should never happen
artID' :: Article Stored -> Int
artID' = fromMaybe 0 . artID

-- | Syntax is from RecordWildCards extension
markStored :: Article a -> Article Stored
markStored Article {..} = Article {..}

instance Pg.FromRow (Article Stored) where
    fromRow = do aid         <- Pg.field
                 uid         <- Pg.field
                 pub_date    <- Pg.field
                 title       <- Pg.field
                 author      <- Pg.field
                 url         <- Pg.field
                 summary     <- Pg.field
                 orig_lang   <- Pg.field
                 body        <- Pg.field
                 av_trans    <- Pg.field
                 return Article {
                          artID       = aid,
                          artUID      = uid,
                          artPubDate  = pub_date,
                          artTitle    = title,
                          artAuthor   = author,
                          artURL      = url,
                          artSummary  = summary,
                          artOrigLang = orig_lang,
                          artBody     = fromMaybe [] $ maybeRead body,
                          artAvTrans  = fromMaybe [] $ maybeRead av_trans }

-- | This instance is for inserts, when the DB chooses the article ID.
instance Pg.ToRow (Article UnStored) where
    toRow a = Pg.toRow ( artUID a
                       , artPubDate a
                       , artTitle a
                       , artAuthor a
                       , artURL a
                       , artSummary a
                       , artOrigLang a
                       , show $ artBody a
                       , show $ artAvTrans a )

-- | This instance is used for things like updates, when we have an article
--   ID in the database.
instance Pg.ToRow (Article Stored) where
    toRow a = Pg.toRow ( artUID a
                       , artPubDate a
                       , artTitle a
                       , artAuthor a
                       , artURL a
                       , artSummary a
                       , artOrigLang a
                       , show $ artBody a 
                       , show $ artAvTrans a
                       , artID a ) -- ^ At the end for WHERE clause :/

sqlAddArticle :: Pg.Query
sqlAddArticle =
    [sql| INSERT INTO articles
                        (contributor_id, pub_date, title, author,
                         url, summary, orig_lang, body, av_trans)
                 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                 RETURNING * |]

insertArticle :: Article UnStored
              -> Pg.Connection
              -> IO (Article Stored)
insertArticle art dbConn = do [a] <- Pg.query dbConn sqlAddArticle art
                              return a

sqlEditArticle :: Pg.Query
sqlEditArticle =
    [sql| UPDATE articles SET contributor_id = ?
                            , pub_date = ?
                            , title = ?
                            , author = ?
                            , url = ?
                            , summary = ?
                            , orig_lang = ?
                            , body = ?
                            , av_trans = ?
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

-- * Translation

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

-- | With translations we so far assume they're not edited - instead we
--   just add new translations add new translations.
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

sqlGetTransLangs :: Pg.Query
sqlGetTransLangs =
    [sql| SELECT DISTINCT trans_lang FROM translations WHERE article_id = ? |]

getTransLangs :: Int -> Pg.Connection -> IO [Language]
getTransLangs aID dbConn = do
    (langs :: [Pg.Only Language]) <- Pg.query dbConn sqlGetTransLangs (Pg.Only aID)
    return $ map Pg.fromOnly langs

-- * Body transformations

-- | The database stores <show>ed versions of line-number/sentence pairs,
--   grouped per paragraph.
textToBody :: Text -> [[(Int, Text)]]
textToBody = innerZip [0..] . body
  where body = map sentences . paragraphs
        sentences = map T.strip . putBackPeriods . T.splitOn ". "
        putBackPeriods xs = map (`T.append` ".") (init xs) ++ [last xs]
        paragraphs = filter (not . T.null) . map T.strip . T.lines

bodyAsParagraphs :: [[(Int, Text)]] -> [Text]
bodyAsParagraphs = map (T.intercalate " " . map snd)

bodyAsText :: [[(Int, Text)]] -> Text
bodyAsText = T.intercalate "\n\n" . bodyAsParagraphs
