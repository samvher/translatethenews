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
import Data.Time.Clock                  ( UTCTime )
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
                deriving ( Eq, Read, Show )

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
      artID       :: Maybe Int,
      artUID      :: Int,
      artPubDate  :: Text,
      artTitle    :: Text,
      artAuthor   :: Text,
      artURL      :: Text,
      artSummary  :: Maybe Text,
      artOrigLang :: Language,
      artBody     :: [[(Int, Text)]],
      artAvTrans  :: [Language], -- ^ Available translation languages
      artCreated  :: UTCTime,
      artModified :: UTCTime
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
                 created     <- Pg.field
                 modified    <- Pg.field
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
                          artAvTrans  = fromMaybe [] $ maybeRead av_trans,
                          artCreated  = created,
                          artModified = modified }

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
                       , show $ artAvTrans a
                       , artCreated a
                       , artModified a)

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
                       , artCreated a
                       , artModified a
                       , artID a ) -- ^ At the end for WHERE clause :/

sqlAddArticle :: Pg.Query
sqlAddArticle =
    [sql| INSERT INTO articles
                        (contributor_id, pub_date, title, author,
                         url, summary, orig_lang, body, av_trans,
                         created, modified)
                 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
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
                            , created = ?
                            , modified = ?
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
      trBody    :: [[(Int, Text)]],
      trCreated :: UTCTime
    } deriving ( Read, Show )

instance Pg.FromRow Translation where
    fromRow = do id      <- Pg.field
                 aID     <- Pg.field
                 uID     <- Pg.field
                 lang    <- Pg.field
                 title   <- Pg.field
                 summary <- Pg.field
                 body    <- Pg.field
                 created <- Pg.field
                 return Translation { trID = id
                                    , trAID = aID
                                    , trUID = uID
                                    , trLang = lang
                                    , trTitle = title
                                    , trSummary = summary
                                    , trBody = fromMaybe [] $ maybeRead body
                                    , trCreated = created }

-- | With translations we so far assume they're not edited - instead we
--   just add new translations add new translations.
instance Pg.ToRow Translation where
    toRow t = Pg.toRow ( trAID t
                       , trUID t
                       , trLang t
                       , trTitle t
                       , trSummary t
                       , show $ trBody t
                       , trCreated t )

sqlAddTranslation :: Pg.Query
sqlAddTranslation =
    [sql| INSERT INTO translations
                        (article_id, contributor_id, trans_lang, title,
                         summary, body, created)
                 VALUES (?, ?, ?, ?, ?, ?, ?)
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

sqlGetArticlesTranslatedToLang :: Pg.Query
sqlGetArticlesTranslatedToLang =
    [sql| SELECT DISTINCT a.*
          FROM translations AS t
          INNER JOIN articles AS a
            ON t.article_id = a.id
          WHERE t.trans_lang = ? |]

-- TODO: Maybe we want to include articles originally in the language?
getArticlesTranslatedToLang :: Language -> Pg.Connection -> IO [Article Stored]
getArticlesTranslatedToLang lang dbConn =
    Pg.query dbConn sqlGetArticlesTranslatedToLang $ Pg.Only lang

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

-- * ToRow fix

instance (Pg.ToField a, Pg.ToField b, Pg.ToField c, Pg.ToField d,
          Pg.ToField e, Pg.ToField f, Pg.ToField g, Pg.ToField h,
          Pg.ToField i, Pg.ToField j, Pg.ToField k)
    => Pg.ToRow (a,b,c,d,e,f,g,h,i,j,k) where
    toRow (a,b,c,d,e,f,g,h,i,j,k) =
        [Pg.toField a, Pg.toField b, Pg.toField c, Pg.toField d,
         Pg.toField e, Pg.toField f, Pg.toField g, Pg.toField h,
         Pg.toField i, Pg.toField j, Pg.toField k]

instance (Pg.ToField a, Pg.ToField b, Pg.ToField c, Pg.ToField d,
          Pg.ToField e, Pg.ToField f, Pg.ToField g, Pg.ToField h,
          Pg.ToField i, Pg.ToField j, Pg.ToField k, Pg.ToField l)
    => Pg.ToRow (a,b,c,d,e,f,g,h,i,j,k,l) where
    toRow (a,b,c,d,e,f,g,h,i,j,k,l) =
        [Pg.toField a, Pg.toField b, Pg.toField c, Pg.toField d,
         Pg.toField e, Pg.toField f, Pg.toField g, Pg.toField h,
         Pg.toField i, Pg.toField j, Pg.toField k, Pg.toField l]

