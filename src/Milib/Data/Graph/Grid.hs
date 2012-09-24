module Milib.Data.Graph.Grid
   ( mkGraph
   ) where

import qualified Data.Graph.Inductive.Graph as G

mkGraph :: (G.Graph gr, Show b) => Int -> Int -> [a] -> (G.Node -> G.Node -> b) -> gr a b
mkGraph n m as f = G.mkGraph ns es
   where
      ns = zip [1..n*m] as
      es = [ (i, i', f i i')
           | i <- [1..n*m], let (x, y) = fromId i
           , (dx, dy) <- ds, let x' = x + dx, let y' = y + dy
           , x' >= 1, x' <= n, y' >= 1, y' <= m
           , let i' = toId x' y'
           ]
      fromId i = ((i-1) `div` m + 1, (i-1) `mod` m + 1)
      toId x y = (x-1) * m + (y-1) + 1
      ds = [(1, 0), (-1, 0), (0, 1), (0, -1)]

-- vim: set expandtab:
