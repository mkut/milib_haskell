{-# LANGUAGE RankNTypes #-}
module Milib.Data.STRef
   ( liftRunSTRef
   , lift
   , liftSet
   , liftGet
   , lift0
   , liftSet0
   , liftGet0
   ) where

import Data.STRef

import Control.Monad
import Control.Monad.ST

liftRunSTRef :: (forall s. ST s (STRef s a)) -> a
liftRunSTRef x'' = runST $ do
   x' <- x''
   x <- readSTRef x'
   return x

lift :: (b -> a -> (c, a)) -> b -> STRef s a -> ST s c
lift f x y' = do
   y <- readSTRef y'
   let (ret, ny) = id $! f x y
   writeSTRef y' ny
   return ret

liftSet :: (b -> a -> a) -> b -> STRef s a -> ST s ()
liftSet f x y' = do
   y <- readSTRef y'
   writeSTRef y' $! f x y

liftGet :: (b -> a -> c) -> b -> STRef s a -> ST s c
liftGet f x y' = do
   y <- readSTRef y'
   return $! f x y

lift0 :: (a -> (c, a)) -> STRef s a -> ST s c
lift0 f y' = do
   y <- readSTRef y'
   let (ret, ny) = id $! f y
   writeSTRef y' ny
   return ret

liftSet0 :: (a -> a) -> STRef s a -> ST s ()
liftSet0 f y' = do
   y <- readSTRef y'
   writeSTRef y' $! f y

liftGet0 :: (a -> c) -> STRef s a -> ST s c
liftGet0 f y' = do
   y <- readSTRef y'
   return $! f y

-- vim: set expandtab:
