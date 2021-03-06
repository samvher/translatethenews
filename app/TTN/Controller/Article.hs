{-|
Module      : TTN.Controller.Article
Description : Processing code for everything Article-related.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module TTN.Controller.Article where

import TTN.Hidden
import TTN.Util
import TTN.Routes

import TTN.Controller.Shared
import TTN.Controller.User
import TTN.Model.Article
import TTN.Model.Core
import TTN.Model.Language
import TTN.Model.Translation
import TTN.Model.User
import TTN.View.Article
import TTN.View.Core

import Control.Monad                    ( when, (=<<) )
import Control.Monad.IO.Class           ( liftIO )
import Data.List                        ( sort )
import Data.Maybe                       ( fromJust
                                        , fromMaybe
                                        , isNothing
                                        , listToMaybe )
import Data.Monoid                      ( (<>) )
import Data.Text                        ( Text, pack, unpack )
import Data.Time.Clock                  ( getCurrentTime )

import Text.Digestive.Form
import Text.Digestive.Types

import Database.Persist

import qualified Data.Text as T
import qualified TTN.Bing as B
import qualified Web.Spock as S

-- * Article

-- TODO: Validate supplied URL
-- TODO: Uniqueness validation?
-- | Prefill if an article is supplied
mkArticleForm :: Maybe Article -> Form Text (TTNAction ctx) Article
mkArticleForm a = "article" .: ( Article
    <$> "uid"      .: validateM getLoggedInUID (pure ())
    -- TODO: currently everyone has edit rights and UID gets changed on edit
    <*> "pub_date" .: check "Date not valid"
                            (testPattern dateP)
                            (text $ articlePubDate <$> a)
    <*> "title"    .: check "No title supplied"
                            checkNE
                            (text $ articleTitle <$> a)
    <*> "author"   .: check "No author supplied"
                            checkNE
                            (text $ articleAuthor <$> a)
    <*> "url"      .: check "No URL supplied"
                            checkNE
                            (text $ articleUrl <$> a)
    <*> "summary"  .: validate wrapMaybe (text $ articleSummary =<< a)
    <*> "language" .: choice allLangs (articleOrigLang <$> a)
    <*> "body"     .: validate validBody (text $ bodyAsText . articleBody <$> a)
    <*> "av_trans" .: (pure . fromMaybe [] $ articleAvTrans <$> a) 
    <*> "created"  .: validateM valCreated (pure $ articleCreated <$> a)
    <*> "modified" .: validateM (\_ -> Success <$> now) (pure ())
    <*> pure "[]"
    <*> pure "[]" )
  where wrapMaybe x = if T.length x > 0 then Success $ Just x
                                        else Success Nothing
        allLangs    = zip allLanguages $ map (pack . show) allLanguages
        validBody b = if not $ T.null b then Success $ textToBody b
                                        else Error "No body supplied"
        valCreated   = maybe (Success <$> now) (return . Success)

-- | Serve and process new-Article form
newArticle :: TTNAction ctx a
newArticle = serveForm "article" articleForm renderer processArticle
  where articleForm      = mkArticleForm Nothing
        renderer         = renderArticleForm newArticlePath
        processArticle a = do new <- runSQL $ insert a
                              gotoViewArticle new

-- | Serve and process edit-Article form
editArticle :: Key Article -> TTNAction ctx a
editArticle aID = do
    art <- runSQL $ get aID
    let articleForm      = mkArticleForm art
        renderer         = renderArticleForm $ S.renderRoute editArticleR aID
        processArticle a = do runSQL . replace aID $ a
                              gotoViewArticle aID
    serveForm "article" articleForm renderer processArticle

-- | Show requested article
viewArticle :: Key Article -> TTNAction ctx a
viewArticle aID = do art <- getArticleById aID
                     maybe (renderSimpleStr "Not found!")
                           (renderPage . renderArticle)
                           art

-- | Article listing
listArticles :: TTNAction ctx a
listArticles = renderPage . renderArticleList =<< runSQL getArticleList
  where getArticleList = selectList [] [Desc ArticleCreated]

-- | Update the "available translations" field
-- updateAvTrans :: Int -> TTNAction ctx (Article Stored)
-- updateAvTrans aID = do art   <- findArticle aID
--                        langs <- runQuerySafe $ getTransLangs aID
--                        runQuerySafe $ updateArticle art { artAvTrans = langs }
updateAvTrans :: Entity Article -> Language -> TTNAction ctx ()
updateAvTrans ea@(Entity aID art) lang = runSQL $
    update aID [ArticleAvTrans =. updateLangList (articleAvTrans art) lang]
  where updateLangList current added = if added `elem` current
                                          then current
                                          else sort $ added : current
-- 
-- | Article listing for specific language (where translation to this
--   language is available). -- TODO: This naming confuses even me.
articlesInLang :: Language -> TTNAction ctx a
articlesInLang l = do
    articles <- runSQL $ getArticlesTranslatedToLang l
    renderPage $ renderArticleList articles

-- TODO: Confusing naming with function above (which might not be
-- necessary)
listArticlesInLangs :: TTNAction ctx a
listArticlesInLangs = do
    Just u <- sessUser <$> S.readSession  -- TODO: no Just
    let langs = userTransLangs $ entUser u
    when (null langs) $ renderSimpleStr "No languages chosen! Maybe you still have to set them in your profile?"
    as     <- runSQL $ selectList [ArticleOrigLang <-. langs] [Desc ArticleCreated]
    renderPage $ renderArticleList as

listUserArticles :: TTNAction ctx a
listUserArticles = do
    Just u <- sessUser <$> S.readSession -- TODO: no Just
    as     <- runSQL $ selectList [ArticleContrId ==. userID u] [Desc ArticleCreated]
    renderPage $ renderArticleList as

-- * Translation

-- | Form for new translations. The supplied Article is the one we are
--   writing a translation for. The Language argument is the target
--   language, the source language is taken from the Article.
mkTranslateForm :: Entity Article -> [Entity Translation] -> Language
                -> Form Text (TTNAction ctx) Translation
mkTranslateForm ea@(Entity aid a) ets lang = "translate" .: ( Translation
    <$> "aid"      .: pure aid
    <*> "uid"      .: validateM getLoggedInUID (pure ())
    <*> "lang"     .: pure lang
    <*> "title"    .: check "No title supplied"  checkNE (text transTitle)
    <*> "summary"  .: validate wrapMaybe (text transSumm)
    -- TODO: pre-fill with existing translations
    <*> "body"     .: listOf mkParagraphForm (Just $ zipMaybe' (articleBody a) tBody)
    <*> "created"  .: validateM (\_ -> Success <$> now) (pure ())
    <*> pure "[]" )
  where wrapMaybe x = if T.length x > 0 then Success $ Just x
                                        else Success Nothing
        -- TODO: For now we just use the head article, this can be
        -- sophisticated.
        transTitle = translationTitle    .  entTranslation <$> listToMaybe ets
        transSumm  = translationSummary =<< entTranslation <$> listToMaybe ets
        tBody      = translationBody     .  entTranslation <$> listToMaybe ets
        mkParagraphForm (Just (ss, tss)) =
            "paragraph" .: listOf mkSentenceForm (Just $ zipMaybe' ss tss)
        mkParagraphForm Nothing = "paragraph" .: pure []
        -- TODO: Update doc for this part because it is abracadabra
        -- | Input is a sentence from the source article, output is a form
        --   element for a sentence in the translation. The "original" field is
        --   only added so we can show the original sentence as form label, it
        --   does not actually accept data. (It would be better to use
        --   something other than 'text' to indicate this but I did not find
        --   a suitable option.)
        mkSentenceForm (Just ((i, s), maybeT)) =
            (\t _ -> (i,t)) <$> "translation" .: text (snd <$> maybeT)
                            <*> "original"    .: text (Just s)
        -- I don't think this actually needs to be here... but want to
        -- avoid non-exhaustive pattern matching.
        mkSentenceForm Nothing = pure (0, "bla")

-- | Fetch the Article with given ID and its translations to given Language.
getArtTranslations :: Key Article
                   -> Language
                   -> TTNAction ctx (Entity Article, [Entity Translation])
getArtTranslations aID lang = do
    art <- getArticleById aID
    when (isNothing art) $ renderSimpleStr errorStr
    ts  <- runSQL $ selectList [ TranslationArtId ==. aID,
                                 TranslationLang  ==. lang ]
                               [ Desc TranslationCreated ]
    return (fromJust art, ts)
  where errorStr = "Error: tried to access non-existing article."

-- | Serve and process new-Translation form
newTranslation :: Key Article -> Language -> TTNAction ctx a
newTranslation aID lang = do
    (ea@(Entity _ art), ts) <- getArtTranslations aID lang
    let translateForm = mkTranslateForm ea ts lang
        renderer      = renderTranslate ea lang $
                            S.renderRoute newTranslationR aID lang
    serveForm "translate" translateForm renderer $ \t -> do
        runSQL $ insert t
        updateAvTrans ea lang
        gotoViewTranslation t

getAdminID :: TTNAction ctx (Key User)
getAdminID = do admin <- runSQL $ selectFirst [ UserName ==. "admin" ] []
                when (isNothing admin) $
                    renderSimpleStr "An error occurred: no admin user found"
                let Just (Entity adminID _) = admin
                return adminID

autoTranslation :: Key Article -> Language -> TTNAction ctx a
autoTranslation aID lang = do
    ma <- getArticleById aID
    when (isNothing ma) $ renderSimpleStr "Article not found"
    adminID <- getAdminID
    let Just ea@(Entity _ art) = ma
    res <- liftIO . B.evalBing azureKey $ do
        let trans txt = B.translateM txt (articleOrigLang art) lang
        title   <- trans $ articleTitle   art
        summ    <- mapM trans $ articleSummary art
        body    <- mapM (mapM (mapM trans)) $ articleBody art
        created <- liftIO getCurrentTime
        return $ Translation aID adminID lang title summ body created "[]"
    let insertAndGo r = do tid <- runSQL $ insert r
                           updateAvTrans ea lang
                           S.redirect $ newTranslationPath ea lang
    either (renderSimpleStr . show) insertAndGo res

-- | Show translations for chosen Article in chosen Language
viewTranslation :: Key Article -> Language -> TTNAction ctx a
viewTranslation aID lang = do
    (art, ts) <- getArtTranslations aID lang
    renderPage $ renderTranslation art ts

listPrefTranslations :: TTNAction ctx a
listPrefTranslations = do
    Just (Entity _ u) <- sessUser <$> S.readSession  -- TODO: no Just
    listTranslationsInLangs $ userReadLangs u

listTranslationsIn :: Language -> TTNAction ctx a
listTranslationsIn l = listTranslationsInLangs [l]

listTranslationsInLangs :: [Language] -> TTNAction ctx a
listTranslationsInLangs langs = do
    when (null langs) $ renderSimpleStr "No languages chosen! Maybe you still have to set them in your profile?"
    ts <- runSQL $ getTranslationsInLangs langs
    renderPage $ renderTrans ts

-- * Redirects

-- | Redirect to the view page for this article
gotoViewArticle :: Key Article -> TTNAction ctx a
gotoViewArticle = S.redirect . S.renderRoute viewArticleR

-- | Redirect to the view page for this translation
gotoViewTranslation :: Translation -> TTNAction ctx a
gotoViewTranslation t =
    S.redirect $ S.renderRoute viewTranslationR (translationArtId t)
                                                (translationLang t)

