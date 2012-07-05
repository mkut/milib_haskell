module Milib.Algorithm.Dijkstra
   ( dijkstra
   ) where

import Milib.Data.Heap.ST
import Milib.Data.Map.ST
import Data.STRef
import Data.Maybe
import Data.Graph.Inductive.Graph
import Control.Monad.ST
import Control.Monad

{- TODO
 - return path
 -}

dijkstra :: (Graph gr, Ord c) => gr a b -> [(c, Node)] -> (a -> b -> c -> c) -> (Node -> Bool) -> Maybe (c, Path)
dijkstra gr s f gf = runST $ do
   initHeap <- newListHeap $ map g s
   visited <- newMap
   dijkstra' gr f gf initHeap visited
   where
      g (a, b) = (a, (Nothing, b))

dijkstra' :: (Graph gr, Ord c) => gr a b -> (a -> b -> c -> c) -> (Node -> Bool) -> STHeap s c (Maybe Node, Node) -> STMap s Node (Maybe Node) -> ST s (Maybe (c, Path))
dijkstra' gr f gf h s = do
   emptyFlag <- isEmptyHeap h
   if emptyFlag then return Nothing else do
      (cost, (prev, node)) <- splitMinHeap h
      visited <- memberMap node s
      if visited then dijkstra' gr f gf h s else
         if gf node then return (Just (cost, [])) else do
            insertMap node prev s 
            forM_ (lsuc gr node) $ \(n, b) -> do
               visited <- memberMap n s
               let a = fromJust $ lab gr n
               if visited then return () else
                  insertHeap (f a b cost, (Just node, n)) h
            dijkstra' gr f gf h s

-- vim: set expandtab:
