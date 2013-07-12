{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
module Milib.Contest
   ( contestMain
   , hContestMain

   , gcjMain
   , hGCJMain
   , gcjMainLn
   , hGCJMainLn
   ) where

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Functor.Identity
import System.IO
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Printf
import Milib.IO

type Printer b = Handle -> b -> IO ()
type Solver a b = a -> b
type Parser a = Stream C.ByteString Identity Char => Parsec C.ByteString () a
type CMain a b = Printer b -> Solver a b -> Parser a -> IO ()
type HCMain a b = Handle -> Handle -> CMain a b

instance Monad m => Stream C.ByteString m Char where
   uncons = return . C.uncons

hContestMain :: HCMain a b
hContestMain hin hout printer solver parser = do
   input <- C.hGetContents hin
   case parse parser "" input of
      Left err -> do { hPutStr stderr "parse err: "; hPrint stderr err }
      Right x  -> printer hout $ solver x

contestMain :: CMain a b
contestMain = hContestMain stdin stdout

hContestMainN :: Int -> HCMain a b
hContestMainN n hin hout printer solver parser = mapM_ f [1..n]
   where
      f _ = hContestMain hin hout printer solver parser

gcjMain :: CMain a b
gcjMain = hGCJMain stdin stdout

hGCJMain :: Handle -> Handle -> CMain a b
hGCJMain hin hout printer solver parser =
   hContestMain hin hout gcjPrinter gcjSolver gcjParser
   where
      gcjPrinter h xs = mapM_ f $ zip xs ([1..] :: [Int])
         where
            f (x, i) = do
               hPrintf h "Case #%d: " i
               printer h x
      gcjSolver = map solver
      gcjParser =
         do  t <- number
             spaces
             count t parser
         <?> "GCJMain"

gcjMainLn :: CMain a b
gcjMainLn = hGCJMainLn stdin stdout

hGCJMainLn :: Handle -> Handle -> CMain a b
hGCJMainLn hin hout printer solver parser =
   hContestMain hin hout gcjPrinter gcjSolver gcjParser
   where
      gcjPrinter h xs = mapM_ f $ zip xs ([1..] :: [Int])
         where
            f (x, i) = do
               hPrintf h "Case #%d:\n" i
               printer h x
      gcjSolver = map solver
      gcjParser =
         do  t <- number
             spaces
             count t parser
         <?> "GCJMain"

-- vim: set expandtab:
