{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module TTN.Controller.Article where

import TTN.Util                         ( checkNE
                                        , dateP
                                        , maybeRead
                                        , testPattern )
import TTN.Routes

import TTN.Controller.Core
import TTN.Model.Article
import TTN.Model.Core
import TTN.Model.User
import TTN.View.Article
import TTN.View.Core

import Control.Monad                    ( (=<<) )
import Data.Maybe                       ( fromJust, isNothing )
import Data.Monoid                      ( (<>) )
import Data.Text                        ( Text, unpack )
import Text.Digestive.Form
import Text.Digestive.Types

import qualified Web.Spock as S
import qualified Data.Text as T

-- Article

-- TODO: Validate supplied URL
-- TODO: Uniqueness validation?
-- If an article is supplied, generate an edit-form, otherwise a new-form
mkArticleForm :: Maybe (Article a)
              -> Form Text (TTNAction ctx) (Article Stored)
mkArticleForm a = "article" .: validateM writeToDb ( Article
    <$> "id"       .: pure (artID =<< a)
    <*> "pub_date" .: check "Date not valid"
                            (testPattern dateP)
                            (text $ artPubDate <$> a)
    <*> "title"    .: check "No title supplied"
                            checkNE
                            (text $ artTitle <$> a)
    <*> "author"   .: check "No author supplied"
                            checkNE
                            (text $ artAuthor <$> a)
    <*> "url"      .: check "No URL supplied"
                            checkNE
                            (text $ artURL <$> a)
    <*> "summary"  .: validate wrapMaybe (text $ artSummary =<< a)
    <*> "language" .: validate readLang  (text $ artLangAsText <$> a)
    <*> "body"     .: validate validBody (text $ bodyAsText . artBody <$> a))
  where wrapMaybe x = if T.length x > 0
                        then Success $ Just x
                        else Success Nothing
        readLang x = let x' = maybeRead $ unpack x
                      in maybe (Error "Language not valid") Success x'
        validBody b = if not $ T.null b
                        then Success $ textToBody b
                        else Error "No body supplied"
        writeToDb d = let q = case artID =<< a of
                                Nothing -> insertArticle
                                Just _  -> updateArticle . markStored
                       in Success <$> S.runQuery (q d)

processArticle :: TTNAction ctx a
processArticle = serveForm "article" articleForm renderer $ \a ->
                      renderSimpleStr $ show a
  where articleForm = mkArticleForm Nothing
        renderer = renderArticleForm $ S.renderRoute newArticleR

editArticle :: Int -> TTNAction ctx a
editArticle aID = do
    art <- S.runQuery $ getArticleById aID
    let articleForm = mkArticleForm art
        renderer    = renderArticleForm $ S.renderRoute editArticleR aID
    serveForm "article" articleForm renderer $ \a -> renderSimpleStr $ show a

viewArticle :: Int -> TTNAction ctx a
viewArticle aID = do art <- S.runQuery $ getArticleById aID
                     maybe (renderSimpleStr "Not found!")
                           (renderPage . renderArticle)
                           art

listArticles :: TTNAction ctx a
listArticles = do
    as <- S.runQuery getArticleList
    renderSimpleStr $ show as

-- Translation

mkTranslateForm :: Article Stored -> [Translation] -> Language
                -> Form Text (TTNAction ctx) Translation
mkTranslateForm a _ lang = "translate" .: validateM writeToDb ( Translation
    <$> "id"       .: pure Nothing
    <*> "aid"      .: validate artHasID (pure a)
    <*> "uid"      .: validateM getUser (pure ())
    <*> "lang"     .: pure lang
    -- TODO: pre-fill with existing translations
    <*> "title"    .: check "No title supplied"  checkNE (text Nothing)
    <*> "summary"  .: validate wrapMaybe (text Nothing)
    <*> "body"     .: listOf mkParagraphForm (Just $ artBody a) )
  where writeToDb t = Success <$> S.runQuery (insertTranslation t)
        artHasID a = case artID a of
                       Just aID -> Success aID
                       _        -> Error "Supplied article has no ID"
        getUser _ = do u <- sessUser <$> S.readSession
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

getArtTranslations :: Int -> Language -> TTNAction ctx (Article Stored, [Translation])
getArtTranslations aID lang = do
    art <- fromJust <$> S.runQuery (getArticleById aID) -- TODO: not robust
    ts  <- S.runQuery $ getTranslations aID lang
    return (art, ts)

translateArticle :: Int -> Language -> TTNAction ctx a
translateArticle aID lang = do
    (art, ts) <- getArtTranslations aID lang
    let translateForm = mkTranslateForm art ts lang
        renderer = renderTranslate art lang $ S.renderRoute newTranslationR aID lang
    serveForm "translate" translateForm renderer $ \t ->
        renderSimpleStr $ show t

viewTranslation :: Int -> Language -> TTNAction ctx a
viewTranslation aID lang = do
    (art, ts) <- getArtTranslations aID lang
    renderPage $ mapM_ (renderTranslation art) ts

