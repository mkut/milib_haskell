{-# LANGUAGE RankNTypes #-}
module Milib.Data.Map.ST
   ( STMap

   , newMap
   , newListMap

   , insertMap
   , deleteMap
   , memberMap
   ) where

import Data.Map
import Data.STRef
import Control.Monad.ST
import Milib.Data.STRef

type STMap s a b = STRef s (Map a b)

newMap :: Ord a => ST s (STMap s a b)
newMap = newSTRef empty

newListMap :: Ord a => [(a, b)] -> ST s (STMap s a b)
newListMap = newSTRef . fromList

-- Lifted functions
runSTMap :: Ord a => (forall s. ST s (STMap s a b)) -> Map a b
runSTMap = liftRunSTRef

insertMap :: Ord a => a -> b -> STMap s a b -> ST s ()
insertMap x y = liftSet (uncurry insert) (x, y)

deleteMap :: Ord a => a -> STMap s a b -> ST s ()
deleteMap = liftSet delete

memberMap :: Ord a => a -> STMap s a b -> ST s Bool
memberMap = liftGet member

-- vim: set expandtab:
