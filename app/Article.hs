{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Article where

import Parsers
import Routing
import Types
import Util
import View

import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe                       ( listToMaybe )
import Data.Text                        hiding ( length )
import Data.Time.Clock
import Database.PostgreSQL.Simple.SqlQQ ( sql )
import Lucid
import Text.Digestive.Form
import Text.Digestive.Types
import Text.Digestive.View
import Text.Digestive.Lucid.Html5

import Web.Spock hiding ( text ) -- seems out of place

import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToRow as Pg

data Stored = Stored
data UnStored = UnStored

data Article a =
    Article {
      artID       :: Maybe Int,
      artPubDate  :: Text,
      artTitle    :: Text,
      artAuthor   :: Text,
      artURL      :: Text,
      artSummary  :: Maybe Text,
      artOrigLang :: Language,
      artBody     :: Text
    } deriving ( Read, Show )

artLangText :: Article a -> Text
artLangText = pack . show . artOrigLang

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
                 return Article { artID       = id,
                                  artPubDate  = pub_date,
                                  artTitle    = title,
                                  artAuthor   = author,
                                  artURL      = url,
                                  artSummary  = summary,
                                  artOrigLang = orig_lang,
                                  artBody     = body }

instance Pg.ToRow (Article UnStored) where
    toRow a = Pg.toRow ( artPubDate a
                       , artTitle a
                       , artAuthor a
                       , artURL a
                       , artSummary a
                       , artOrigLang a
                       , artBody a)

instance Pg.ToRow (Article Stored) where
    toRow a = Pg.toRow ( artPubDate a
                       , artTitle a
                       , artAuthor a
                       , artURL a
                       , artSummary a
                       , artOrigLang a
                       , artBody a 
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

getArticleById :: Int -> TTNAction ctx (Maybe (Article Stored))
getArticleById id = listToMaybe <$> runQuery (\c ->
                        Pg.query c sqlGetArticle (Pg.Only id))

-- TODO: Validate URL
-- TODO: Uniqueness validation?
mkArticleForm :: Maybe (Article a)
              -> Form Text (TTNAction ctx) (Article Stored)
mkArticleForm a = "article" .: validateM writeToDb ( Article
    <$> "id"       .: pure (artID =<< a)
    <*> "pub_date" .: check "Date not valid" (testPattern dateP) (text Nothing)
    <*> "title"    .: check "No title supplied"  checkNE (text $ artTitle  <$> a)
    <*> "author"   .: check "No author supplied" checkNE (text $ artAuthor <$> a)
    <*> "url"      .: check "No URL supplied"    checkNE (text $ artURL    <$> a)
    <*> "summary"  .: validate wrapMaybe (text $ artSummary  =<< a)
    <*> "language" .: validate readLang (text $ artLangText <$> a)
    <*> "body"     .: check "No body supplied"   checkNE (text $ artBody   <$> a))
  where date _    = Success . pack . show <$> liftIO getCurrentTime
        wrapMaybe x = if T.length x > 0
                        then Success $ Just x
                        else Success Nothing
        readLang x = let x' = maybeRead $ unpack x
                      in maybe (Error "Language not valid") Success x'
        writeToDb d = let q = case artID =<< a of
                                Nothing -> insertArticle
                                Just _  -> updateArticle . markStored
                       in Success <$> runQuery (q d)

-- TODO: Move next couple of functions to another module, maybe
renderArticleForm :: Text -> Token -> View (Html ()) -> Html ()
renderArticleForm target tok view = pageTemplate $
    form_ [method_ "post", action_ target]
          (do errorList "article" view
              inputText_ "article.pub_date" "Publication date (yyyy-mm-dd)" view
              inputText_ "article.title"    "Title"    view
              inputText_ "article.author"   "Author"   view
              inputText_ "article.url"      "URL"      view
              inputText_ "article.summary"  "Summary"  view
              inputText_ "article.language" "Language" view
              inputText_ "article.body"     "Body"     view
              csrf tok
              submit "Submit article")

processArticle :: TTNAction ctx a
processArticle = serveForm "article" articleForm renderer $ \a ->
                      lucid . pageTemplate . toHtml $ show a
  where articleForm = mkArticleForm Nothing
        renderer = renderArticleForm $ renderRoute newArticleR

editArticle :: Int -> TTNAction ctx a
editArticle aID = do art <- getArticleById aID
                     let articleForm = mkArticleForm art
                         renderer = renderArticleForm $ renderRoute editArticleR aID
                     serveForm "article" articleForm renderer $ \a ->
                         lucid . pageTemplate . toHtml $ show a

viewArticle :: Int -> TTNAction ctx a
viewArticle aID = do art <- getArticleById aID
                     lucid . pageTemplate . toHtml $ show art

sqlListArticles :: Pg.Query
sqlListArticles = [sql| SELECT * FROM articles |]

-- TODO: Pagination, sorting, etc
listArticles :: TTNAction ctx a
listArticles = do (arts :: [Article Stored]) <- runQuery' sqlListArticles
                  lucid . pageTemplate . toHtml $ show arts

-- TODO: implement Translation, getTranslations, mkTranslateForm, renderTranslate
translateArticle :: Int -> Language -> TTNAction ctx a
translateArticle aID lang = do
    art <- getArticleById aID
    lucid . pageTemplate . toHtml $ "Translating " ++ show art ++ " to " ++ show lang
    -- ts  <- getTranslations aID 
    -- let translateForm = mkTranslateForm art ts
    -- serveForm "translate" translateForm renderTranslate $ \t ->
        -- lucid . pageTemplate . toHtml $ show t

-- mkTranslateForm :: Article Stored
--                 -> [Translation]
--                 -> Form Text (TTNAction ctx) Translation
-- 
-- -- Missing language?
-- getTranslations :: Int -> Pg.Connection -> TTNAction ctx [Translation]
-- 
-- renderTranslate :: Text -> Token -> View (Html ()) -> Html ()
-- 
