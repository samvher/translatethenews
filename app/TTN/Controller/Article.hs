{-|
Module      : TTN.Controller.Article
Description : Processing code for everything Article-related.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module TTN.Controller.Article where

import TTN.Util                         ( checkNE
                                        , dateP
                                        , maybeRead
                                        , testPattern )
import TTN.Routes

import TTN.Controller.Core
import TTN.Controller.User
import TTN.Model.Article
import TTN.Model.Core
import TTN.Model.User
import TTN.View.Article
import TTN.View.Core

import Control.Monad                    ( when, (=<<) )
import Data.Maybe                       ( fromJust, fromMaybe, isNothing )
import Data.Monoid                      ( (<>) )
import Data.Text                        ( Text, unpack )
import Text.Digestive.Form
import Text.Digestive.Types

import qualified Web.Spock as S
import qualified Data.Text as T

-- * Article

-- TODO: Validate supplied URL
-- TODO: Uniqueness validation?
-- | If an article is supplied, generate an edit-form, otherwise a new-form
mkArticleForm :: Maybe (Article a)
              -> Form Text (TTNAction ctx) (Article Stored)
mkArticleForm a = "article" .: validateM writeToDb ( Article
    <$> "id"       .: pure (artID =<< a)
    -- TODO: currently everyone has edit rights and UID gets changed on edit
    <*> "uid"      .: validateM getLoggedInUID (pure ())
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
    <*> "body"     .: validate validBody (text $ bodyAsText . artBody <$> a)
    <*> "av_trans" .: (pure . fromMaybe [] $ artAvTrans <$> a) )
  where wrapMaybe x = if T.length x > 0
                        then Success $ Just x
                        else Success Nothing
        readLang x = let x' = maybeRead $ unpack x
                      in maybe (Error "Language not valid") Success x'
        validBody b = if not $ T.null b
                        then Success $ textToBody b
                        else Error "No body supplied"
        -- | Here something subtle is going on. We're using Stored/UnStored
        --   on Article as phantom types to affect the way Pg.ToRow kicks
        --   in (see TTN.Model.Article). If it's a new article (no ID
        --   assigned, UnStored) it gets INSERTed into the database. If it's an
        --   existing article (which has an ID, Stored) it gets UPDATEd in
        --   the database.
        writeToDb d = let q = case artID =<< a of
                                Nothing -> insertArticle
                                Just _  -> updateArticle . markStored
                       in Success <$> runQuerySafe (q d)

-- | Serve and process new-Article form
newArticle :: TTNAction ctx a
newArticle = serveForm "article" articleForm renderer gotoViewArticle
  where articleForm = mkArticleForm Nothing
        renderer = renderArticleForm $ S.renderRoute newArticleR

-- | Serve and process edit-Article form
editArticle :: Int -> TTNAction ctx a
editArticle aID = do
    art <- runQuerySafe $ getArticleById aID
    let articleForm = mkArticleForm art
        renderer    = renderArticleForm $ S.renderRoute editArticleR aID
    serveForm "article" articleForm renderer gotoViewArticle

-- | Show requested article
viewArticle :: Int -> TTNAction ctx a
viewArticle aID = do art <- runQuerySafe $ getArticleById aID
                     maybe (renderSimpleStr "Not found!")
                           (renderPage . renderArticle)
                           art

-- | Article listing
listArticles :: TTNAction ctx a
listArticles = renderPage . renderArticleList =<< runQuerySafe getArticleList

-- | Update the "available translations" field
updateAvTrans :: Int -> TTNAction ctx (Article Stored)
updateAvTrans aID = do mA <- runQuerySafe $ getArticleById aID
                       -- TODO: Not sure this is entirely safe, i.e. if
                       -- renderSimpleStr breaks out of execution.
                       when (isNothing mA) $ renderSimpleStr errorStr
                       let a = fromJust mA
                       langs <- runQuerySafe $ getTransLangs  aID
                       runQuerySafe $ updateArticle a { artAvTrans = langs }
  where errorStr = "Error: tried to access non-existing article."

-- * Translation

-- | Form for new translations. The supplied Article is the one we are
--   writing a translation for. The Language argument is the target
--   language, the source language is taken from the Article.
mkTranslateForm :: Article Stored -> [Translation] -> Language
                -> Form Text (TTNAction ctx) Translation
mkTranslateForm a _ lang = "translate" .: validateM writeToDb ( Translation
    <$> "id"       .: pure Nothing
    <*> "aid"      .: validate artHasID (pure a)
    <*> "uid"      .: validateM getLoggedInUID (pure ())
    <*> "lang"     .: pure lang
    -- TODO: pre-fill with existing translations
    <*> "title"    .: check "No title supplied"  checkNE (text Nothing)
    <*> "summary"  .: validate wrapMaybe (text Nothing)
    <*> "body"     .: listOf mkParagraphForm (Just $ artBody a) )
  where writeToDb t = Success <$> runQuerySafe (insertTranslation t) -- TODO: fix this
        artHasID  a = case artID a of
                        Just aID -> Success aID
                        _        -> Error "Supplied article has no ID"
        wrapMaybe x = if T.length x > 0
                        then Success $ Just x
                        else Success Nothing
        mkParagraphForm ss = "paragraph" .: listOf mkSentenceForm ss
        -- | Input is a sentence from the source article, output is a form
        --   element for a sentence in the translation. The "original" field is
        --   only added so we can show the original sentence as form label, it
        --   does not actually accept data. (It would be better to use
        --   something other than 'text' to indicate this but I did not find
        --   a suitable option.)
        mkSentenceForm (Just (i, s)) = (\t _ -> (i,t))
                                         <$> "translation" .: text Nothing
                                         <*> "original"    .: text (Just s)
        mkSentenceForm Nothing = pure (0, "bla") -- TODO: figure this out

-- | Fetch the Article with given ID and its translations to given Language.
getArtTranslations :: Int -> Language -> TTNAction ctx (Article Stored, [Translation])
getArtTranslations aID lang = do
    art <- fromJust <$> runQuerySafe (getArticleById aID) -- TODO: not robust
    ts  <- runQuerySafe $ getTranslations aID lang
    return (art, ts)

-- | Serve and process new-Translation form
newTranslation :: Int -> Language -> TTNAction ctx a
newTranslation aID lang = do
    (art, ts) <- getArtTranslations aID lang
    let translateForm = mkTranslateForm art ts lang
        renderer      = renderTranslate art lang $
                            S.renderRoute newTranslationR aID lang
    serveForm "translate" translateForm renderer $ \t -> do
        updateAvTrans aID
        gotoViewTranslation t

-- | Show translations for chosen Article in chosen Language
viewTranslation :: Int -> Language -> TTNAction ctx a
viewTranslation aID lang = do
    (art, ts) <- getArtTranslations aID lang
    renderPage $ mapM_ (renderTranslation art) ts

-- * Redirects

-- | Redirect to the view page for this article
gotoViewArticle :: Article Stored -> TTNAction ctx a
gotoViewArticle = S.redirect . S.renderRoute viewArticleR . fromJust . artID

-- | Redirect to the view page for this translation
gotoViewTranslation :: Translation -> TTNAction ctx a
gotoViewTranslation t =
    S.redirect $ S.renderRoute viewTranslationR (trAID t) (trLang t)

