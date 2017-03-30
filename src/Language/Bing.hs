-- Adapted from Ernesto Rodriguez's MicrosoftTranslator-0.1.0.1 on Hackage

{-# Language RecordWildCards, OverloadedStrings #-}

module Language.Bing( BingLanguage(..)
                    , BingContext
                    , BingError(..)
                    , AzureKey
                    , checkToken
                    , evalBing
                    , execBing
                    , getAccessToken
                    , getAccessTokenEither
                    , getBingCtx
                    , runBing
                    , runExceptT
                    , translate
                    , translateM
                    ) where

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Control.Lens                 ( (&), (.~), (^.) )
import Control.Monad.Trans.Except   ( ExceptT , runExceptT , throwE )
import Data.ByteString              ( ByteString )
import Data.ByteString.Char8        ( pack, unpack )
import Data.DateTime                ( DateTime(..)
                                    , getCurrentTime
                                    , diffSeconds )
import Data.List                    ( find )
import Data.Monoid                  ( (<>) )
import Data.Text                    ( Text )
import Network.HTTP.Client          ( HttpException )
import Text.XML.Light.Input         ( parseXML )
import Text.XML.Light.Types         ( Content(..), elName, qName )
import Text.XML.Light.Proc          ( strContent )

import qualified Control.Exception as E
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.Wreq as N

type AzureKey = ByteString

data BingError = BingError ByteString
                 deriving ( Show )

data BingLanguage = English
                  | German
                  | Indonesian
                  | Norwegian
                  | Russian
                  | Spanish
                  | Turkish

-- | Conversion function from Language to language code
toSym bl = case bl of English    -> "en"
                      German     -> "de"
                      Indonesian -> "id"
                      Norwegian  -> "no"
                      Russian    -> "ru"
                      Spanish    -> "es"
                      Turkish    -> "tr"

type AccessToken = ByteString

data BingContext =
  BCTX {
    accessToken :: AccessToken,
    inception :: DateTime,
    azureKey :: ByteString
  } deriving ( Show )

newtype BingMonad m a = BM {runBing :: BingContext -> ExceptT BingError m a}

instance (Monad m, MonadIO m) => Monad (BingMonad m) where
  m >>= f  = BM (\ctx' -> do ctx <- checkToken ctx'
                             res <- runBing m ctx
                             runBing (f res) ctx)
            
  return a = BM (const $ return a)

instance (Monad m, MonadIO m) => Functor (BingMonad m) where
  fmap f bm = do v <- bm
                 return $ f v

instance (Monad m, MonadIO m) => Applicative (BingMonad m) where
  pure    = return
  a <*> b = do a' <- a
               b' <- b
               return (a' b')

instance MonadTrans BingMonad where
  lift m = BM (const $ lift m)

instance MonadIO m => MonadIO (BingMonad m) where
  liftIO io = BM (const $ liftIO io)

instance E.Exception BingError

tokenAuthPage :: String
tokenAuthPage = "https://api.cognitive.microsoft.com/sts/v1.0/issueToken"

translateUrl :: String
translateUrl = "https://api.microsofttranslator.com/v2/Http.svc/Translate"
 
translateArgs text from to = [ "text" N.:= (text       :: ByteString),
                               "from" N.:= (toSym from :: ByteString),
                               "to"   N.:= (toSym to   :: ByteString) ]

bingAction :: MonadIO m =>
              IO (N.Response BL.ByteString)
           -> ExceptT BingError m (N.Response BL.ByteString)
bingAction action = do
  let try = E.try :: IO (N.Response BL.ByteString)
                  -> IO (Either HttpException (N.Response BL.ByteString))
  res <- lift . liftIO $ try action
  case res of Right res -> return res
              Left  ex  -> throwE . BingError . pack $ show ex

post url postable = bingAction (N.post url postable) 

postWith opts url postable = bingAction (N.postWith opts url postable)

getWithAuth opts' url = withContext $ \BCTX{..} -> do
  let opts = opts' & N.header "Authorization" .~ ["Bearer " <> accessToken]
  bingAction (N.getWith opts url)

-- | Request a new access token from Azure using the specified client
-- id and client secret
getAccessToken :: MonadIO m => AzureKey -> ExceptT BingError m BingContext
getAccessToken key = do
  let empty = [] :: [N.FormParam] -- We need some Postable type...
  req <- post (tokenAuthPage ++ "?Subscription-Key=" ++ unpack key) empty
  t   <- liftIO getCurrentTime
  return BCTX{
    accessToken = BL.toStrict $ req ^. N.responseBody,
    inception = t,
    azureKey = key
    }

-- | Check if the access token of the running BingAction is still
-- valid. If the token has expired, renews the token automatically
checkToken :: MonadIO m => BingContext -> ExceptT BingError m BingContext
checkToken ctx@BCTX{..} = do
  t <- liftIO getCurrentTime
  let mustRenew = diffSeconds t inception > 480
  if mustRenew then do
    BCTX{accessToken = tk} <- getAccessToken azureKey
    t' <- liftIO getCurrentTime
    return ctx{accessToken = tk, inception = t'}
  else
    return ctx

withContext = BM

-- | Action that translates text inside a BingMonad context.
translateM :: MonadIO m =>
              Text -> BingLanguage -> BingLanguage -> BingMonad m Text
translateM text from to = do
  let opts =   N.defaults
             & N.param "from"        .~ [toSym from :: Text]
             & N.param "to"          .~ [toSym to]
             & N.param "contentType" .~ ["text/plain"]
             & N.param "category"    .~ ["general"]
             & N.param "text"        .~ [text]
  res <- getWithAuth opts translateUrl
  let trans = parseXML . TE.decodeUtf8 . BLC.toStrict $ res ^. N.responseBody
      elemTest (Elem e) = "string" == qName (elName e)
      elemTest _        = False
  case find elemTest trans of
    Just (Elem e) -> return . T.pack $ strContent e
    _             -> BM $ const (throwE . BingError . pack $ show res)

-- | Helper function that evaluates a BingMonad action. It simply
-- requests and access token and uses the token for evaluation.
evalBing :: MonadIO m => AzureKey -> BingMonad m a -> m (Either BingError a)
evalBing key action = runExceptT $ do
  t <- getAccessToken key
  runBing action t

getBingCtx :: Monad m => BingMonad m BingContext
getBingCtx = BM {runBing = return}

execBing :: MonadIO m =>
            BingContext
         -> BingMonad m a
         -> m (Either BingError (a,BingContext))
execBing ctx action = runExceptT . flip runBing ctx $ do
    res <- action
    ctx <- getBingCtx
    return (res,ctx)

getAccessTokenEither :: AzureKey -> IO (Either BingError BingContext)
getAccessTokenEither = runExceptT . getAccessToken

-- | Toplevel wrapper that translates a text. It is only recommended if translation
-- is invoked less often than every 10 minutes since it always
-- requests a new access token.  For better performance use
-- translateM, runBing and getAccessToken
translate :: AzureKey
          -> Text
          -> BingLanguage
          -> BingLanguage
          -> IO (Either BingError Text)
translate key text from to = evalBing key $ translateM text from to

