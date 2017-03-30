{-|
Module      : TTN.Model.Translation
Description : Model code for Translation.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module TTN.Model.Translation where

import TTN.Model.Article
import TTN.Model.Language
import TTN.Model.User

import Data.Text                        ( Text )
import Data.Time.Clock                  ( UTCTime )

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

import qualified Data.Text as T

-- * Translation

share [mkPersist sqlSettings, mkMigrate "migrateTranslation"] [persistLowerCase|
Translation sql=translations
  artId        ArticleId                                   sql=article_id
  contrId      UserId                                      sql=contributor_id
  lang         Language                                    sql=trans_lang
  title        Text                                        sql=title
  summary      Text           Maybe                        sql=summary
  body         ArticleBody    default='[]'                 sql=body_new
  created      UTCTime        default=CURRENT_TIMESTAMP    sql=created
  bodyOld      String                                      sql=body
  Id sql=id
  deriving Read Show
|]

entTranslation :: Entity Translation -> Translation
entTranslation (Entity _ t) = t

getArticlesTranslatedToLang :: Language -> SqlPersistM [Entity Article]
getArticlesTranslatedToLang lang = rawSql query [toPersistValue lang]
  where query = T.unwords [ "SELECT DISTINCT ?? FROM translations"
                          , "INNER JOIN articles"
                          , "ON translations.article_id = articles.id"
                          , "WHERE translations.trans_lang = ?" ]

getTranslationsInLangs :: [Language] -> SqlPersistM [Entity Translation]
getTranslationsInLangs langs = rawSql query $ map toPersistValue langs
  where holes = T.intercalate "," . take (length langs) $ repeat "?"
        query = T.unwords [ "SELECT ??"
                          , "FROM ( SELECT DISTINCT ON (a.id)"
                          , "a.created AS a_created, t.* "
                          , "FROM translations AS t"
                          , "INNER JOIN articles AS a"
                          , "ON t.article_id = a.id"
                          , "WHERE t.trans_lang IN ("
                          , holes
                          , ")"
                          , "ORDER BY a.id, t.created DESC) translations"
                          , "ORDER BY translations.a_created DESC" ]

