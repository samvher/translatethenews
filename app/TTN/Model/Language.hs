{-|
Module      : TTN.Model.Language
Description : Language types.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module TTN.Model.Language where

import TTN.Util                         ( maybeRead )

import Data.Text                        ( Text, pack, unpack )
import Lucid                            ( ToHtml(..) )
import Web.PathPieces

import Database.Persist.TH

-- * Language

data Language = English
              | Russian
              | Turkish
              | Indonesian
                deriving ( Eq, Read, Show )

derivePersistField "Language"

allLanguages :: [Language]
allLanguages = [ English
               , Russian
               , Turkish
               , Indonesian ]

langAsText :: Language -> Text
langAsText = pack . show

instance PathPiece Language where
    fromPathPiece = maybeRead . unpack
    toPathPiece   = pack . show

instance ToHtml Language where
    toHtml    = toHtml . show
    toHtmlRaw = toHtml

-- | This is for use in for example Google Translate URLs
langCode :: Language -> Text
langCode English    = "en"
langCode Russian    = "ru"
langCode Turkish    = "tr"
langCode Indonesian = "id"

