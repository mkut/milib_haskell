{-# LANGUAGE RankNTypes #-}
module Milib.Data.Set.ST
   ( STSet

   , newSet
   , newListSet

   , insertSet
   , deleteSet
   , memberSet
   ) where

import Data.Set
import Data.STRef
import Control.Monad.ST
import Milib.Data.STRef

type STSet s a = STRef s (Set a)

newSet :: Ord a => ST s (STSet s a)
newSet = newSTRef empty

newListSet :: Ord a => [a] -> ST s (STSet s a)
newListSet = newSTRef . fromList

-- Lifted functions
runSTSet :: Ord a => (forall s. ST s (STSet s a)) -> Set a
runSTSet = liftRunSTRef

insertSet :: Ord a => a -> STSet s a -> ST s ()
insertSet = liftSet insert

deleteSet :: Ord a => a -> STSet s a -> ST s ()
deleteSet = liftSet delete

memberSet :: Ord a => a -> STSet s a -> ST s Bool
memberSet = liftGet member

-- vim: set expandtab:
