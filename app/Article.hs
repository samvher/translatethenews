{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Article where

import Types
import Util
import View

import Control.Monad
import Control.Monad.IO.Class
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
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg

data Language = English
              | Russian
              | Turkish
              | Indonesian
                deriving ( Read, Show )

instance Pg.FromField Language where
    fromField f dat = read . unpack <$> Pg.fromField f dat

instance Pg.ToField Language where
    toField = Pg.toField . pack . show

data Article =
    Article {
      artPubDate  :: UTCTime,
      artTitle    :: Text,
      artAuthor   :: Text,
      artURL      :: Text,
      artSummary  :: Maybe Text,
      artOrigLang :: Language,
      artBody     :: Text
    } deriving ( Read, Show )

artLangText :: Article -> Text
artLangText = pack . show

instance Pg.FromRow Article where
    fromRow = do (id :: Int) <- Pg.field
                 pub_date    <- Pg.field
                 title       <- Pg.field
                 author      <- Pg.field
                 url         <- Pg.field
                 summary     <- Pg.field
                 orig_lang   <- Pg.field
                 body        <- Pg.field
                 return Article { artPubDate  = pub_date,
                                  artTitle    = title,
                                  artAuthor   = author,
                                  artURL      = url,
                                  artSummary  = summary,
                                  artOrigLang = orig_lang,
                                  artBody     = body }

instance Pg.ToRow Article where
    toRow a = Pg.toRow ( artPubDate a
                       , artTitle a
                       , artAuthor a
                       , artURL a
                       , artSummary a
                       , artOrigLang a
                       , artBody a )

sqlAddArticle :: Pg.Query
sqlAddArticle =
    [sql| INSERT INTO articles
                        (pub_date, title, author, url, summary, orig_lang, body)
                 VALUES (?, ?, ?, ?, ?, ?, ?) |]

insertArticle :: Article -> Pg.Connection -> IO ()
insertArticle art dbConn = void $ Pg.execute dbConn sqlAddArticle art

-- TODO: Fix time
-- TODO: Validate URL
-- TODO: Uniqueness validation?
mkArticleForm :: Maybe Article -> Form Text (TTNAction ctx) Article
mkArticleForm a = "article" .: (Article
    <$> "pub_date" .: validateM date (text Nothing)
    <*> "title"    .: check "No title supplied"  checkNE (text $ artTitle  <$> a)
    <*> "author"   .: check "No author supplied" checkNE (text $ artAuthor <$> a)
    <*> "url"      .: check "No URL supplied"    checkNE (text $ artURL    <$> a)
    <*> "summary"  .: validate wrapMaybe (text $ artSummary  =<< a)
    <*> "language" .: validate readLang  (text $ artLangText <$> a)
    <*> "body"     .: check "No body supplied"   checkNE (text $ artBody   <$> a))
  where date _    = Success <$> liftIO getCurrentTime
        wrapMaybe x = if T.length x > 0
                        then Success $ Just x
                        else Success Nothing
        readLang x = let x' = maybeRead $ unpack x
                      in maybe (Error "Language not valid") Success x'

-- TODO: Move next two articles away, maybe
renderArticleForm :: Token -> View (Html ()) -> Html ()
renderArticleForm tok view = pageTemplate $
    form_ [method_ "post", action_ "/article"]
          (do errorList "article" view
              inputText_ "article.pub_date" "Publication date (ignored)" view
              inputText_ "article.title"    "Title"                      view
              inputText_ "article.author"   "Author"                     view
              inputText_ "article.url"      "URL"                        view
              inputText_ "article.summary"  "Summary"                    view
              inputText_ "article.language" "Language"                   view
              inputText_ "article.body"     "Body"                       view
              csrf tok
              submit "Submit article")

processArticle :: TTNAction ctx a
processArticle = serveForm "article" articleForm renderArticleForm $ \a ->
                   do runQuery $ insertArticle a
                      lucid . pageTemplate . toHtml $ show a
  where articleForm = mkArticleForm Nothing

