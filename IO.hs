{-# LANGUAGE FlexibleContexts #-}
module Milib.IO
   ( number
   , number'

   , float
   , float'

   , word
   , word'

   -- from Text.Parsec.Combinator
   , many1
   , count

   -- from Text.Parsec.Char
   , spaces
   , char
   , letter
   , string
   , digit
   , anyChar
   ) where

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char

number' :: (Stream s m Char, Integral a, Read a) => ParsecT s u m a
number' =
       do ds <- many1 digit
          return (read ds)
   <?> "number"

number :: (Stream s m Char, Integral a, Read a) => ParsecT s u m a
number = do spaces; number'

floatDot' :: (Stream s m Char, Floating a, Read a) => ParsecT s u m a
floatDot' =
       do x <- many1 digit
          char '.'
          y <- many1 digit
          return $ read (x ++ "." ++ y)
   <?> "float_dot"

float' :: (Stream s m Char, Floating a, Read a) => ParsecT s u m a
float' =
       try floatDot'
   <|> do x <- number'
          return $ fromIntegral x
   <?> "float"

float :: (Stream s m Char, Floating a, Read a) => ParsecT s u m a
float = do spaces; float'

word' :: Stream s m Char => ParsecT s u m String
word' = many1 letter

word :: Stream s m Char => ParsecT s u m String
word = do spaces; word'

-- vim: set expandtab:
