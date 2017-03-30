{-|
Module      : TTN.Util
Description : Some mostly generic functions, used in the other modules.
Author      : Sam van Herwaarden <samvherwaarden@protonmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module TTN.Util where

import Control.Monad              ( replicateM_ )
import Data.Maybe                 ( listToMaybe )
import Data.Char                  ( isAlpha, isDigit )
import Data.Text                  ( Text )

import Text.Pandoc                ( def, readMarkdown, writeHtmlString )

import qualified Data.Text as T
import qualified Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Combinator as P

-- | This is a one-to-one copy from Network.CGI.Protocol
--   More info about reads here:
--   https://www.vex.net/~trebla/haskell/reads.xhtml
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Validate non-empty property
checkNE :: T.Text -> Bool
checkNE = (> 0) . T.length

-- | Funky zipper, type is self-explanatory
innerZip :: [a] -> [[b]] -> [[(a,b)]]
innerZip []       __ = []
innerZip __       [] = []
innerZip xs (ys:yss) = zip as ys : innerZip bs yss
    where (as, bs) = splitAt (length ys) xs

zipMaybe :: [a] -> [b] -> [(a, Maybe b)]
zipMaybe (x:xs) (y:ys) = (x, Just y)  : zipMaybe xs ys
zipMaybe (x:xs)    []  = (x, Nothing) : zipMaybe xs []
zipMaybe    []     __  = []

zipMaybe' :: [a] -> Maybe [b] -> [(a, Maybe b)]
zipMaybe' xs Nothing   = zip xs $ repeat Nothing
zipMaybe' xs (Just ys) = zipMaybe xs ys

-- * Functions related to parsing

-- | Like normal parse but does not accept Partial
parseNoPartial :: P.Parser a -> Text -> P.IResult Text a
parseNoPartial p t = finalize result
  where result = P.parse p t
        finalize r = case r of
                       P.Partial f -> finalize $ f ""
                       _ -> r

-- | Only test if the pattern is valid, don't use results
testPattern :: P.Parser a -> Text -> Bool
testPattern p t = test result
  where result = P.parse p t
        test r = case r of
                    P.Fail{}      -> False
                    P.Partial f   -> test $ f ""
                    (P.Done i _)  -> T.null i

-- Some patterns that are sort of generic
-- This holds for all: they are not exactly right... but good enough for now

-- | Characters in the label parts of hostnames
isHostChar :: Char -> Bool
isHostChar c = isDigit c || isAlpha c || c == '-'

-- | Characters in the name part of an email address
isNameChar :: Char -> Bool
isNameChar c = isDigit c || isAlpha c || c `elem` ("_-." :: String)

-- | Simple hostname pattern
hostnameP :: P.Parser ()
hostnameP = do P.takeWhile1 isHostChar
               P.many1 label
               return ()
  where label = do P.satisfy $ \c -> c == '.'
                   P.takeWhile1 isHostChar

-- | Simple email address pattern
emailP :: P.Parser ()
emailP = do P.takeWhile1 isNameChar
            P.satisfy $ \c -> c == '@'
            hostnameP

-- | Date in format YYYY-MM-DD
dateP :: P.Parser ()
dateP = do replicateM_ 4 P.digit
           P.satisfy (== '-')
           replicateM_ 2 P.digit
           P.satisfy (== '-')
           replicateM_ 2 P.digit

mdToHtml :: String -> String
mdToHtml src = case md of
    Left  _ -> "<h3>Error! Sorry...</h3>"
    Right p -> writeHtmlString def p
  where md = readMarkdown def src

