{-# LANGUAGE RankNTypes #-}
module Milib.Data.Heap.ST
   ( STHeap

   , newHeap
   , newListHeap

   , runSTHeap
   , insertHeap
   , mergeHeap
   , isEmptyHeap
   , findMinHeap
   , deleteMinHeap
   , splitMinHeap
   ) where

import Data.Graph.Inductive.Internal.Heap
import Data.STRef
import Control.Monad.ST
import Milib.Data.STRef

type STHeap s a b = STRef s (Heap a b)

newHeap :: Ord a => ST s (STHeap s a b)
newHeap = newSTRef empty

newListHeap :: Ord a => [(a, b)] -> ST s (STHeap s a b)
newListHeap = newSTRef . build

-- Lifted functions
runSTHeap :: Ord a => (forall s. ST s (STHeap s a b)) -> Heap a b
runSTHeap = liftRunSTRef

insertHeap :: Ord a => (a, b) -> STHeap s a b -> ST s ()
insertHeap = liftSet insert

mergeHeap :: Ord a => Heap a b -> STHeap s a b -> ST s ()
mergeHeap = liftSet merge

isEmptyHeap :: Ord a => STHeap s a b -> ST s Bool
isEmptyHeap = liftGet0 isEmpty

findMinHeap :: Ord a => STHeap s a b -> ST s (a, b)
findMinHeap = liftGet0 findMin

deleteMinHeap :: Ord a => STHeap s a b -> ST s ()
deleteMinHeap = liftSet0 deleteMin

splitMinHeap :: Ord a => STHeap s a b -> ST s (a, b)
splitMinHeap = lift0 f
   where
      f h = let (a, b, h') = splitMin h in ((a, b), h')

-- vim: set expandtab:
