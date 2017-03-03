{-|
Module      : TTN.View.Article
Description : Code for rendering article-related pages.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module TTN.View.Article where

import TTN.Routes

import TTN.Controller.Shared
import TTN.Model.Article
import TTN.Model.Core
import TTN.View.Core                    ( renderSimpleStr ) -- TODO: This should go
import TTN.View.Shared
import TTN.View.User

import Control.Monad                    ( forM_ )
import Control.Monad.Trans.Class        ( lift )
import Data.Maybe                       ( fromMaybe )
import Data.Monoid                      ( (<>) )
import Data.Text                        ( Text, pack )
import Data.Time.Format                 ( FormatTime
                                        , defaultTimeLocale
                                        , formatTime )
import Lucid
import Text.Digestive.Form
import Text.Digestive.View

import qualified Text.Digestive.Lucid.Html5 as DL

-- | Render the body of an Article or Translation
renderBody :: [[(Int, Text)]] -> TTNView ctx ()
renderBody b = mapM_ (p_ . toHtml) $ bodyAsParagraphs b

-- * Article views

-- | Generate HTML for new/edit article form
renderArticleForm :: Text -> View (TTNView ctx ()) -> TTNView ctx ()
renderArticleForm target view = div_ [id_ "new-article-form"] $ do 
        h2_ "Article"
        form_ [method_ "post", action_ target] $ do
              DL.errorList "article" view
              inputText_ "article.pub_date" "Publication date (yyyy-mm-dd)" view
              inputText_ "article.title"    "Title"    view
              inputText_ "article.author"   "Author"   view
              inputText_ "article.url"      "URL"      view
              inputSelect_ "article.language" "Language" view
              inputTextArea_ (Just 5)  (Just 100) "article.summary"  "Summary"  view
              inputTextArea_ (Just 25) (Just 100) "article.body" "Body" view
              csrf
              submit "Submit article"

getTime :: FormatTime t => t -> String
getTime = formatTime defaultTimeLocale "%e %B %Y, %H:%M"

-- | Generate HTML for showing an Article
renderArticle :: Article Stored -> TTNBlockDef ctx
renderArticle a = blockDef
  where blockDef TTNContent = div_ [id_ "view-article"] $ do
            articleHead a
            -- Switched off for now
            -- div_ [id_ "edit-article-link"] . a_ [href_ $ editArticlePath a] $
                -- h "Edit article"
            maybe (return ()) (p_ . strong_ . toHtml) $ artSummary a
            renderBody $ artBody a
            articleFooter a
        blockDef other      = defaultBlocks other

articleHead :: Article Stored -> TTNView ctx ()
articleHead a = do
    -- p_ . em_ . toHtml $ "Submitted " <> (getTime $ artCreated a)
    -- p_ . em_ . toHtml $ "Last edited " <> (getTime $ artModified a)
    h2_ . toHtml $ artTitle a
    div_ [class_ "art-time-author"] . em_ $ do
      h $ artPubDate a <> " - " <> artAuthor a <> " - "
      a_ [href_ $ artURL a, target_ "_blank"] $ h "Original"

articleFooter :: Article Stored -> TTNView ctx ()
articleFooter a = do
    div_ [class_ "user-badge"] $ do h "Contributed by "
                                    renderProfileBadge $ artUID a
    div_ [id_ "available-translations"] $ do h "Available translations: "
                                             mapM_ translationLink $ artAvTrans a
    div_ [id_ "translate-to"] $ do h "Translate to:"
                                   mapM_ translateLink allLanguages
  where translationLink :: Language -> TTNView ctx ()
        translationLink l = div_ [class_ "translation-link"] $
                              a_ [href_ $ viewTranslationPath a l] $ toHtml l
        translateLink :: Language -> TTNView ctx ()
        translateLink l = div_ [class_ "translate-link"] $
                            a_ [href_ $ newTranslationPath a l] $ toHtml l

-- | For use in listings
renderListArticle :: Article Stored -> TTNView ctx ()
renderListArticle a =
    div_ [class_ "list-elem article"] $ do
      h3_ . a_ [href_ (viewArticlePath a)] . toHtml $ artTitle a
      p_ [class_ "list-article-summary"] . h . fromMaybe "" $ artSummary a
      div_ [class_ "article-meta"] . em_ $ do
          h (artPubDate a <> " - " <> artAuthor a <> " - ")
          a_ [href_ $ artURL a, target_ "_blank"] "Original"

renderArticleList :: [Article Stored] -> TTNBlockDef ctx
renderArticleList as = blockDef
  where blockDef TTNContent = do
          div_ [id_ "new-article-link"] . a_ [href_ newArticlePath] $ h "Add article"
          mapM_ renderListArticle as
        blockDef other      = defaultBlocks other

-- * Translation views

-- | Generate HTML for new-Translation form. 'listSubViews' deals with
--   fields generated by 'listOf'. There is some hacky magic going on here
--   for getting the sentences from the original article (they are stored
--   in form fields which do not accept data). (See 'mkTranslateForm' in
--   TTN.Controller.Article)
renderTranslate :: Article Stored
                -> Language
                -> Text
                -> View (TTNView ctx ())
                -> TTNView ctx ()
renderTranslate art lang target view =
    div_ [id_ "translation-form"] . form_ [method_ "post", action_ target] $ do
        DL.errorList "translate" view
        div_ [id_ "gt-link"] $ renderGTranslate (artOrigLang art) lang
                                                (artURL art)
                                                "View on Google Translate"
        h3_ $ h "Title"
        toHtml $ artTitle art
        inputText_ "translate.title" "" view
        h3_ $ h "Summary"
        toHtml . fromMaybe "" $ artSummary art
        inputTextArea_ (Just 5) (Just 110) "translate.summary" "" view
        h3_ $ h "Body"
        forM_ (listSubViews "translate.body" view) $ \v' ->
          div_ [class_ "body-par"] $ forM_ (listSubViews "paragraph" v') $ \v ->
              div_ [class_ "body-sentence"] $ do
                  DL.label "translation" v (toHtml $ fieldInputText "original" v) 
                  DL.inputTextArea (Just 2) (Just 110) "translation" v
                  DL.errorList "translation" v
        csrf
        submit "Submit translation"

-- | Generate HTML for showing a translation
renderTranslation :: Article Stored -> [Translation] -> TTNBlockDef ctx
renderTranslation a ts = blockDef
  where blockDef TTNContent = mapM_ (renderSingleTrans a) ts
        blockDef other      = defaultBlocks other

-- TODO: This is not quite right here
renderSingleTrans :: Article Stored -> Translation -> TTNView ctx ()
renderSingleTrans a t = do
    -- p_ . em_ . toHtml $ "Submitted " <> show (trCreated t)
    h2_ . toHtml $ trTitle t
    p_ . em_ . toHtml $ (artPubDate a <> " - " <> artAuthor a)
    p_ . a_ [href_ $ artURL a] . h $ "Original: " <> artTitle a
    -- p_ . 
    -- 
    maybe (return ()) (p_ . strong_ . toHtml) $ trSummary t
    renderBody $ trBody t
    div_ [class_ "user-badge"] $ do h "Contributed by "
                                    renderProfileBadge $ trUID t
    div_ [id_ "original-link"] $ renderGTranslate (artOrigLang a)
                                                  (trLang t)
                                                  (artURL a)
                                                  "View on Google Translate"

-- | For use in listings
renderListTrans :: Article Stored -> Translation -> TTNView ctx ()
renderListTrans a t =
    div_ [class_ "list-elem translation"] $ do
      h3_ . a_ [href_ (viewTranslationPath a (trLang t))] . toHtml $ trTitle t
      p_ [class_ "list-translation-summary"] . h . fromMaybe "" $ trSummary t
      div_ [class_ "translation-meta"] . em_ $ do
          h (artPubDate a <> " - " <> artAuthor a <> " - ")
          a_ [href_ $ artURL a, target_ "_blank"] "Original"

renderTrans :: [Translation] -> TTNBlockDef ctx
renderTrans ts = blockDef
  where renderSingle t = do
          a' <- lift $ runQuerySafe (getArticleById $ trAID t)
          a <- maybe (lift $ renderSimpleStr "Something strange happened!") -- TODO: set right
                     return a'
          renderListTrans a t
        blockDef TTNContent =
          if null ts then p_ $ h "No translations found... Feel free to contribute!"
                     else mapM_ renderSingle ts
        blockDef other      = defaultBlocks other
          
-- * Google Translate URLs

-- | Generate URL for Google Translate version
mkGTranslateURL :: Language -> Language -> Text -> Text
mkGTranslateURL source target article_url =
    "https://translate.google.com/translate?sl=" <> langCode source <>
      "&tl=" <> langCode target <> "&ie=UTF-8&u=" <> article_url

-- | Generate hyperlink
renderGTranslate :: Language -> Language -> Text -> Text -> TTNView ctx ()
renderGTranslate from to url label =
    a_ [href_ (mkGTranslateURL from to url)] $ toHtml label

