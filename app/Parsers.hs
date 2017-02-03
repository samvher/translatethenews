{-# LANGUAGE OverloadedStrings #-}

module Parsers ( parseNoPartial,
                 testPattern,
                 dateP,
                 hostnameP,
                 emailP,
                 readBody
               ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List.Split
import Data.Text                  ( Text )

import qualified Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Combinator as P
import qualified Data.Text as T

parseNoPartial :: P.Parser a -> Text -> P.IResult Text a
parseNoPartial p t = finalize result
  where result = P.parse p t
        finalize r = case r of
                       P.Partial f -> finalize $ f ""
                       _ -> r

testPattern :: P.Parser a -> Text -> Bool
testPattern p t = test result
  where result = P.parse p t
        test r = case r of
                    P.Fail{}      -> False
                    P.Partial f   -> test $ f ""
                    (P.Done i _)  -> T.null i

hostnameP :: P.Parser ()
hostnameP = do P.takeWhile1 isHostChar
               P.many1 label
               return ()
  where label = do P.satisfy $ \c -> c == '.'
                   P.takeWhile1 isHostChar

isNameChar :: Char -> Bool
isNameChar c = isDigit c || isAlpha c || c `elem` ("_-." :: String)
dateP :: P.Parser ()
dateP = do replicateM_ 4 P.digit
           P.satisfy (== '-')
           replicateM_ 2 P.digit
           P.satisfy (== '-')
           replicateM_ 2 P.digit

isHostChar :: Char -> Bool
isHostChar c = isDigit c || isAlpha c || c == '-'

emailP :: P.Parser ()
emailP = do P.takeWhile1 isNameChar
            P.satisfy $ \c -> c == '@'
            hostnameP

body :: Text -> [[Text]]
body = map sentences . paragraphs
  where paragraphs = filter (not . T.null) . map T.strip . T.lines
        sentences = map T.strip . putBackPeriods . T.splitOn ". "
        putBackPeriods xs = map (`T.append` ".") (init xs) ++ [last xs]

innerZip :: [a] -> [[b]] -> [[(a,b)]]
innerZip []        _ = []
innerZip  _       [] = []
innerZip xs (ys:yss) = zip as ys : innerZip bs yss
    where (as, bs) = splitAt (length ys) xs

readBody :: Text -> [[(Int, Text)]]
readBody = innerZip [0..] . body
