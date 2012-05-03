{-# LANGUAGE FlexibleContexts #-}
module Milib.IO
   ( number
   ) where

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char

number' :: Stream s m Char => ParsecT s u m Int
number' =
   do  ds <- many1 digit
       return (read ds)
   <?> "number"

number :: Stream s m Char => ParsecT s u m Int
number = do spaces; number'

-- vim: set expandtab:
