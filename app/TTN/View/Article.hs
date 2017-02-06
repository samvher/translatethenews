{-# LANGUAGE OverloadedStrings #-}

module TTN.View.Article where

import TTN.Model.Article
import TTN.View.Core

import Control.Monad                    ( forM_ )
import Data.Maybe                       ( fromMaybe )
import Data.Monoid                      ( (<>) )
import Data.Text                        ( Text )
import Lucid
import Text.Digestive.Form
import Text.Digestive.View

import qualified Text.Digestive.Lucid.Html5 as DL

renderArticleForm :: Text -> Token -> View (Html ()) -> Html ()
renderArticleForm target tok view = pageTemplate $
    form_ [method_ "post", action_ target]
          (do DL.errorList "article" view
              inputText_ "article.pub_date" "Publication date (yyyy-mm-dd)" view
              inputText_ "article.title"    "Title"    view
              inputText_ "article.author"   "Author"   view
              inputText_ "article.url"      "URL"      view
              inputText_ "article.summary"  "Summary"  view
              inputText_ "article.language" "Language" view
              inputTextArea_ (Just 25) (Just 100) "article.body" "Body" view
              csrf tok
              submit "Submit article")

renderArticle :: Article Stored -> Html ()
renderArticle a = do
    h1_ . toHtml $ artTitle a
    p_ . em_ . toHtml $ (artPubDate a <> " - " <> artAuthor a)
    p_ . a_ [href_ (artURL a)] $ toHtml ("Original" :: Text)
    maybe (return ()) (p_ . strong_ . toHtml) $ artSummary a
    renderBody $ artBody a

renderTranslate :: Article Stored -> Language -> Text -> Token -> View (Html ()) -> Html ()
renderTranslate art lang target tok view = pageTemplate $
    form_ [method_ "post", action_ target]
          (do DL.errorList "translate" view
              renderGTranslate (artOrigLang art) lang (artURL art) "GT"
              h2_ $ toHtml ("Title" :: Text)
              p_ . toHtml $ artTitle art
              inputText_ "translate.title" "" view
              h2_ $ toHtml ("Summary" :: Text)
              p_ . toHtml . fromMaybe "" $ artSummary art
              inputText_ "translate.summary" "" view
              h2_ $ toHtml ("Body" :: Text)
              forM_ (listSubViews "translate.body" view) $ \v' ->
                p_ $ forM_ (listSubViews "paragraph" v') $ \v ->
                  do p_ $ DL.label "translation" v (toHtml $ fieldInputText "original" v) 
                     p_ $ DL.inputTextArea (Just 2) (Just 120) "translation" v
                     DL.errorList "translation" v
              csrf tok
              submit "Submit translation")

renderTranslation :: Article Stored -> Translation -> Html ()
renderTranslation a t = do
    p_ . em_ . toHtml $ (artPubDate a <> " - " <> artAuthor a)
    h1_ . toHtml $ trTitle t
    p_ . a_ [href_ (artURL a)] . toHtml $ artTitle a
    renderGTranslate (artOrigLang a) (trLang t) (artURL a) "GT"
    maybe (return ()) (p_ . strong_ . toHtml) $ trSummary t
    renderBody $ trBody t

renderBody :: [[(Int, Text)]] -> Html ()
renderBody b = mapM_ (p_ . toHtml) $ bodyAsParagraphs b

-- Google Translate URLs

mkGTranslateURL :: Language -> Language -> Text -> Text
mkGTranslateURL from to url =
    "https://translate.google.com/translate?sl=" <> langCode from <>
      "&tl=" <> langCode to <> "&ie=UTF-8&u=" <> url

renderGTranslate :: Language -> Language -> Text -> Text -> Html ()
renderGTranslate from to url label =
    p_ . a_ [href_ (mkGTranslateURL from to url)] $ toHtml label
