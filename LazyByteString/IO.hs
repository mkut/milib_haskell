module Milib.LazyByteString.IO
   ( glines
   , readInt
   , readIntList
   ) where

import qualified Data.ByteString.Lazy.Char8 as C
import IO
import Data.Maybe

glines = fmap (map (C.takeWhile (/='\r')) . C.lines)
readInt = fst . fromJust . C.readInt
readIntList = map readInt . C.words

-- vim: set expandtab:
