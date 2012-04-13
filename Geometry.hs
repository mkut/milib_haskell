module Milib.Geometry
   ( cross
   , dot
   , norm2
   , ccw
   , convex_hull
   ) where

cross :: (Num a) => (a, a) -> (a,a) -> a
cross (x1,y1) (x2,y2) = x1 * y2 - y1 * x2

dot :: (Num a) => (a, a) -> (a,a) -> a
dot   (x1,y1) (x2,y2) = x1 * x2 + y1 * y2

norm2 :: (Num a) => (a, a) -> a
norm2  (x1,y1) = x1 * x1 + y1 * y1

data Turn = CounterClockwise
          | Clockwise
          | Straight
          | OverBackward
          | Backward

ccw :: (Num a, Ord a) => (a, a) -> (a, a) -> (a, a) -> Turn
ccw (xa,ya) (xb,yb) (xc,yc)
   | cross p q > 0     = CounterClockwise
   | cross p q < 0     = Clockwise
   | dot p q   < 0     = OverBackward
   | norm2 p < norm2 q = Straight
   | otherwise         = Backward
   where
      p = (xb - xa, yb - ya)
      q = (xc - xa, yc - ya)

convex_hull :: (Num a, Ord a) => [(a, a)] -> [(a, a)]
convex_hull x = lower ++ upper
   where
      lower = tail $ convex_hull' x []
      upper = tail $ convex_hull' (reverse x) []

convex_hull' []     ys  = ys
convex_hull' (x:xs) []  = convex_hull' xs [x]
convex_hull' (x:xs) [y] = convex_hull' xs [x,y]
convex_hull' (x:xs) (y1:y2:ys) = case ccw y2 y1 x of
   Clockwise -> convex_hull' (x:xs) (y2:ys)
   CounterClockwise -> convex_hull' xs (x:y1:y2:ys)
   Straight -> convex_hull' xs (x:y1:y2:ys)
   otherwise -> [] -- err

-- vim: set expandtab:

