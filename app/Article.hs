{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
import Data.Maybe                       ( fromJust, fromMaybe, listToMaybe )
import Data.Monoid
import Data.Text                        ( Text, pack, unpack )
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
      artBody     :: [[(Int, Text)]]
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
                                  artBody     = read body } -- TODO: robustness

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

getArticleById :: Int -> TTNAction ctx (Maybe (Article Stored))
getArticleById id = listToMaybe <$> runQuery (\c ->
                        Pg.query c sqlGetArticle (Pg.Only id))

-- TODO: Validate URL
-- TODO: Uniqueness validation?
mkArticleForm :: Maybe (Article a)
              -> Form Text (TTNAction ctx) (Article Stored)
mkArticleForm a = "article" .: validateM writeToDb ( Article
    <$> "id"       .: pure (artID =<< a)
    <*> "pub_date" .: check "Date not valid" (testPattern dateP) (text $ artPubDate <$> a)
    <*> "title"    .: check "No title supplied"  checkNE (text $ artTitle  <$> a)
    <*> "author"   .: check "No author supplied" checkNE (text $ artAuthor <$> a)
    <*> "url"      .: check "No URL supplied"    checkNE (text $ artURL    <$> a)
    <*> "summary"  .: validate wrapMaybe (text $ artSummary  =<< a)
    <*> "language" .: validate readLang (text $ artLangText <$> a)
    <*> "body"     .: validateBody (text $ collectBody . artBody <$> a))
  where date _    = Success . pack . show <$> liftIO getCurrentTime
        wrapMaybe x = if T.length x > 0
                        then Success $ Just x
                        else Success Nothing
        readLang x = let x' = maybeRead $ unpack x
                      in maybe (Error "Language not valid") Success x'
        bodyA = check "No body supplied" checkNE -- TODO: This is ugly
        bodyB = Success . readBody
        validateBody = validate bodyB . bodyA
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
              inputTextArea_ (Just 25) (Just 100) "article.body"     "Body"     view
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
                     maybe (lucid . pageTemplate $ toHtml ("Not found!" :: Text))
                           (lucid . pageTemplate . renderArticle)
                           art

sqlListArticles :: Pg.Query
sqlListArticles = [sql| SELECT * FROM articles |]

-- TODO: Pagination, sorting, etc
listArticles :: TTNAction ctx a
listArticles = do (arts :: [Article Stored]) <- runQuery' sqlListArticles
                  lucid . pageTemplate . toHtml $ show arts

renderArticle :: Article Stored -> Html ()
renderArticle a = do
    h1_ . toHtml $ artTitle a
    p_ . em_ . toHtml $ (artPubDate a <> " - " <> artAuthor a)
    p_ . a_ [href_ (artURL a)] $ toHtml ("Original" :: Text)
    maybe (return ()) (p_ . strong_ . toHtml) $ artSummary a
    renderBody $ artBody a

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
    fromRow = do id <- Pg.field
                 aID <- Pg.field
                 uID <- Pg.field
                 lang <- Pg.field
                 title <- Pg.field
                 summary <- Pg.field
                 body <- Pg.field
                 return Translation { trID = id
                                    , trAID = aID
                                    , trUID = uID
                                    , trLang = lang
                                    , trTitle = title
                                    , trSummary = summary
                                    , trBody = read body } -- TODO: robustness

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

getTranslations :: Int -> Language -> TTNAction ctx [Translation]
getTranslations aID lang = runQuery (\c -> Pg.query c sqlGetTranslations (aID, lang))

mkTranslateForm :: Article Stored -> [Translation] -> Language
                -> Form Text (TTNAction ctx) Translation
mkTranslateForm a _ lang = "translate" .: validateM writeToDb ( Translation
    <$> "id"       .: pure Nothing
    <*> "aid"      .: pure (fromJust $ artID a) -- TODO: make robust
    <*> "uid"      .: validateM getUser (pure ())
    <*> "lang"     .: pure lang
    -- TODO: pre-fill with existing translations
    <*> "title"    .: check "No title supplied"  checkNE (text Nothing)
    <*> "summary"  .: validate wrapMaybe (text Nothing)
    <*> "body"     .: listOf mkParagraphForm (Just $ artBody a) )
  where -- mkTranslation _ _ = Translation
        writeToDb t = Success <$> runQuery (insertTranslation t)
        getUser _ = do u <- sessUser <$> readSession
                       return $ case u of
                         Nothing -> Error "Not logged in" -- TODO: not quite right
                         Just u' -> Success $ uID u'
        wrapMaybe x = if T.length x > 0
                        then Success $ Just x
                        else Success Nothing
        mkParagraphForm ss = "paragraph" .: listOf mkSentenceForm ss
        mkSentenceForm (Just (i, s)) = (\b c -> (i,b))
                                         <$> "translation" .: text Nothing
                                         <*> "original"    .: text (Just s)
        mkSentenceForm Nothing = pure (0, "bla") -- TODO: figure this out

renderTranslate :: Article Stored -> Language -> Text -> Token -> View (Html ()) -> Html ()
renderTranslate art lang target tok view = pageTemplate $
    form_ [method_ "post", action_ target]
          (do errorList "translate" view
              renderGTranslate (artOrigLang art) lang (artURL art)
              h2_ $ toHtml ("Title" :: Text)
              p_ . toHtml $ artTitle art
              inputText_ "translate.title" "" view
              h2_ $ toHtml ("Summary" :: Text)
              p_ . toHtml . fromMaybe "" $ artSummary art
              inputText_ "translate.summary" "" view
              h2_ $ toHtml ("Body" :: Text)
              forM_ (listSubViews "translate.body" view) $ \v' ->
                p_ $ forM_ (listSubViews "paragraph" v') $ \v ->
                  do p_ $ label "translation" v (toHtml $ fieldInputText "original" v) 
                     p_ $ inputTextArea (Just 2) (Just 120) "translation" v
                     errorList "translation" v
              csrf tok
              submit "Submit translation")

getArtTranslations :: Int -> Language -> TTNAction ctx (Article Stored, [Translation])
getArtTranslations aID lang = do
    art <- fromJust <$> getArticleById aID -- TODO: not robust
    ts  <- getTranslations aID lang
    return (art, ts)

translateArticle :: Int -> Language -> TTNAction ctx a
translateArticle aID lang = do
    (art, ts) <- getArtTranslations aID lang
    let translateForm = mkTranslateForm art ts lang
        renderer = renderTranslate art lang $ renderRoute newTranslationR aID lang
    serveForm "translate" translateForm renderer $ \t ->
        lucid . pageTemplate $ p_ (toHtml $ show t)

viewTranslation :: Int -> Language -> TTNAction ctx a
viewTranslation aID lang = do
    (art, ts) <- getArtTranslations aID lang
    lucid . pageTemplate $ mapM_ (renderTranslation art) ts

renderTranslation :: Article Stored -> Translation -> Html ()
renderTranslation a t = do
    p_ . em_ . toHtml $ (artPubDate a <> " - " <> artAuthor a)
    h1_ . toHtml $ trTitle t
    p_ . a_ [href_ (artURL a)] . toHtml $ artTitle a
    renderGTranslate (artOrigLang a) (trLang t) (artURL a)
    maybe (return ()) (p_ . strong_ . toHtml) $ trSummary t
    renderBody $ trBody t

collectBody :: [[(Int, Text)]] -> Text
collectBody = T.intercalate "\n\n" . map (T.intercalate " " . map snd)

renderBody :: [[(Int, Text)]] -> Html ()
renderBody = mapM_ renderParagraph
  where renderParagraph = p_ . toHtml . T.intercalate " " . map snd

langCode :: Language -> Text
langCode English = "en"
langCode Russian = "ru"

mkGTranslateURL :: Language -> Language -> Text -> Text
mkGTranslateURL from to url =
    "https://translate.google.com/translate?sl=" <> langCode from <>
      "&tl=" <> langCode to <> "&ie=UTF-8&u=" <> url

renderGTranslate :: Language -> Language -> Text -> Html ()
renderGTranslate from to url =
    p_ . a_ [href_ (mkGTranslateURL from to url)] . toHtml $ ("GTranslate" :: Text)
