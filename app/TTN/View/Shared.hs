{-|
Module      : TTN.View.Shared
Description : Shared code for templating.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module TTN.View.Shared where

import TTN.Model.Core
import TTN.Model.User

import Control.Monad.Trans.Class        ( lift )
import Data.Text                        ( Text )
import Data.Text.Lazy                   ( toStrict )

import Lucid

import Text.Digestive.View

import Database.Persist

import qualified Text.Digestive.Lucid.Html5 as DL
import qualified Web.Spock as S

-- * View type aliases

type FormRenderer ctx = View (TTNView ctx ()) -> TTNView ctx ()

-- * Higher level layout functions

lucid :: TTNView ctx () -> TTNAction ctx a
lucid v = S.html . toStrict =<< renderTextT v

-- * Functions for generating form views

-- | Shorter code for generating simple form fields
constructView :: (Text -> View (TTNView ctx ()) -> TTNView ctx ())
                 -- ^ A function that renders a single form element by ref
              -> Text -- ^ Reference of the form element
              -> Text -- ^ Label to be used in the rendered form
              -> View (TTNView ctx ())
              -> TTNView ctx ()
constructView f ref lbl view = div_ [class_ "form-input"] $ do
    DL.label ref view $ h lbl
    f ref view
    DL.errorList ref view

inputText_ :: Text -> Text -> View (TTNView ctx ()) -> TTNView ctx ()
inputText_ = constructView DL.inputText

inputTextArea_ :: Maybe Int -> Maybe Int -- ^ Rows and columns
               -> Text -> Text -> View (TTNView ctx ()) -> TTNView ctx ()
inputTextArea_ r c = constructView $ DL.inputTextArea r c

inputPass_ :: Text -> Text -> View (TTNView ctx ()) -> TTNView ctx ()
inputPass_ = constructView DL.inputPassword

inputSelect_ :: Text -> Text -> View (TTNView ctx ()) -> TTNView ctx ()
inputSelect_ = constructView DL.inputSelect

inputMultiSelect_ :: Text -> Text -> View (TTNView ctx ()) -> TTNView ctx ()
inputMultiSelect_ = constructView inputMultiSelect
  where inputMultiSelect r v = with (DL.inputSelect r v) [multiple_ ""]

submit :: Text -> TTNView ctx ()
submit value = input_ [type_ "submit", value_ value, class_ "input-submit"]

csrf :: TTNView ctx ()
csrf = do tok <- lift S.getCsrfToken
          input_ [name_ "__csrf_token", type_ "hidden", value_ tok]

-- | Avoid annoying ambiguous types
h :: Monad m => Text -> HtmlT m ()
h = toHtml

-- | Avoid annoying ambiguous types
hr :: Monad m => Text -> HtmlT m ()
hr = toHtmlRaw

-- | Set some defaults for block contents
defaultBlocks :: TTNBlockDef ctx
defaultBlocks TTNPageTitle = h "Translate the News"
defaultBlocks TTNNavBar    = h "Nav bar"
defaultBlocks TTNContent   = return ()

-- | Get (Maybe) User
getCurrentUser :: TTNTemplate ctx (Maybe (Entity User))
getCurrentUser = lift . lift $ sessUser <$> S.readSession

