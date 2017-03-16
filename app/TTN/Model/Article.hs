{-|
Module      : TTN.Model.Article
Description : Model code for Article.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module TTN.Model.Article where

import TTN.Util                         ( innerZip )

import TTN.Model.Language
import TTN.Model.User

import Data.Text                        ( Text )
import Data.Time.Clock                  ( UTCTime )

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

import qualified Data.Text as T

type ArticleBody = [[(Int, Text)]]

-- * Article

share [mkPersist sqlSettings, mkMigrate "migrateArticle"] [persistLowerCase|
Article sql=articles
  contrId      UserId                                      sql=contributor_id
  pubDate      Text                                        sql=pub_date
  title        Text                                        sql=title
  author       Text                                        sql=author
  url          Text                                        sql=url
  summary      Text           Maybe                        sql=summary
  origLang     Language                                    sql=orig_lang
  body         ArticleBody    default='[]'                 sql=body_new
  avTrans      [Language]     default='[]'                 sql=av_trans_new
  created      UTCTime        default=CURRENT_TIMESTAMP    sql=created
  modified     UTCTime        default=CURRENT_TIMESTAMP    sql=modified
  bodyOld      String                                      sql=body
  avTransOld   String                                      sql=av_trans
  Id sql=id
  deriving Read Show
|]

artLangAsText :: Article -> Text
artLangAsText = langAsText . articleOrigLang

articleID :: Entity Article -> Key Article
articleID (Entity id _) = id

entArticle :: Entity Article -> Article
entArticle (Entity _ a) = a

-- * Body transformations

-- | The database stores <show>ed versions of line-number/sentence pairs,
--   grouped per paragraph.
textToBody :: Text -> [[(Int, Text)]]
textToBody = innerZip [0..] . body
  where body              = map sentences . paragraphs
        sentences         = map T.strip . putBackPeriods . T.splitOn ". "
        putBackPeriods xs = map (`T.append` ".") (init xs) ++ [last xs]
        paragraphs        = filter (not . T.null) . map T.strip . T.lines

bodyAsParagraphs :: [[(Int, Text)]] -> [Text]
bodyAsParagraphs = map (T.intercalate " " . map snd)

bodyAsText :: [[(Int, Text)]] -> Text
bodyAsText = T.intercalate "\n\n" . bodyAsParagraphs

