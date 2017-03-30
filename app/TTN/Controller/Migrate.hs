{-|
Module      : TTN.Controller.Migrate
Description : Code for updating stored data to new formats.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE ScopedTypeVariables #-}

module TTN.Controller.Migrate where

import TTN.Controller.Shared
import TTN.Model.Article
import TTN.Model.Core
import TTN.Model.Translation
import TTN.View.Core

import Database.Persist

migrate1 :: TTNAction ctx a
migrate1 = do
    (as :: [Entity Article]) <- runSQL $ selectList [] []
    mapM_ migrateArticle as
    (ts :: [Entity Translation]) <- runSQL $ selectList [] []
    mapM_ migrateTranslation ts
    renderSimpleStr "Ran migration 1"
  where migrateArticle     (Entity aid a) = runSQL $
          update aid [ ArticleBody     =. read (articleBodyOld    a)
                     , ArticleAvTrans  =. read (articleAvTransOld a) ]
        migrateTranslation (Entity tid t) = runSQL $
          update tid [ TranslationBody =. read (translationBodyOld t) ]

