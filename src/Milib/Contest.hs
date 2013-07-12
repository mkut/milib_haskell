{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
module Milib.Contest
   ( contestMain
   , hContestMain
   , hContestMainN

   , parserWithoutError
   , parsec
   , gcjParsec
   , gcjPrinter
   , gcjPrinterLn
   ) where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Functor.Identity
import System.IO
import qualified Text.Parsec.ByteString.Lazy
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (runParser, Parsec, Stream, (<?>), uncons)
import Text.Printf (hPrintf)
import Milib.IO

type Printer b = Handle -> b -> IO ()
type Solver a b = a -> b
type Parser err a = Handle -> IO (Either err a)
type ParsecParser a = Stream BS.ByteString Identity Char => Parsec BS.ByteString () a
type CMain err a b = Parser err a -> Solver a b -> Printer b -> IO ()
type HCMain err a b = Handle -> Handle -> CMain err a b

contestMain :: Show err => CMain err a b
contestMain = hContestMain stdin stdout

hContestMain :: Show err => HCMain err a b
hContestMain hin hout parser solver printer = do
   input <- parser hin
   case input of
      Left err -> do { hPutStr stderr "parse error: "; hPrint stderr err }
      Right x  -> printer hout $ solver x

hContestMainN :: Show err => Int -> HCMain err a b
hContestMainN n hin hout printer solver parser = mapM_ f [1..n]
   where
      f _ = hContestMain hin hout printer solver parser

parserWithoutError :: (Handle -> IO a) -> Parser ParseError a
parserWithoutError f h = Right <$> f h

parsec :: ParsecParser a -> Parser ParseError a
parsec p hin = runParser p () "" <$> BS.hGetContents hin

gcjParsec :: ParsecParser a -> Parser ParseError [a]
gcjParsec p = parsec (p' <?> "gcjParsec")
   where
      p' = do
         t <- number
         spaces
         count t p

gcjPrinter :: Printer b -> Printer [b]
gcjPrinter p h xs = mapM_ f $ zip xs ([1..] :: [Int])
   where
      f (x, i) = do
         hPrintf h "Case #$d: " i
         p h x

gcjPrinterLn :: Printer b -> Printer [b]
gcjPrinterLn p h xs = mapM_ f $ zip xs ([1..] :: [Int])
   where
      f (x, i) = do
         hPrintf h "Case #%d:\n" i
         p h x

-- vim: set expandtab:
