{-# LANGUAGE OverloadedStrings #-}

module Parsers ( testPattern,
                 dateP,
                 hostnameP,
                 emailP
               ) where

import Control.Monad
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.Char
import Data.Text                  ( Text )

import qualified Data.Text as T

testPattern :: Parser a -> Text -> Bool
testPattern p t = test result
  where result = parse p t
        test r = case r of
                    Fail{}      -> False
                    Partial f   -> test $ f ""
                    (Done i _)  -> T.null i

dateP :: Parser ()
dateP = do replicateM_ 4 digit
           satisfy (== '-')
           replicateM_ 2 digit
           satisfy (== '-')
           replicateM_ 2 digit

isHostChar :: Char -> Bool
isHostChar c = isDigit c || isAlpha c || c == '-'

hostnameP :: Parser ()
hostnameP = do takeWhile1 isHostChar
               many1 label
               return ()
  where label = do satisfy $ \c -> c == '.'
                   takeWhile1 isHostChar

isNameChar :: Char -> Bool
isNameChar c = isDigit c || isAlpha c || c `elem` ("_-." :: String)

emailP :: Parser ()
emailP = do takeWhile1 isNameChar
            satisfy $ \c -> c == '@'
            hostnameP

