module Collisions where

import Tools
import Data.List (sortBy, groupBy)
import Debug

linearCol :: [(Object, Vector)] -> (Object, Vector) -> (Object, Vector)
linearCol all o@((m, (x, y)), v@(dx, dy)) =
  if null candidates then o else
    ((m, (colX, colY)), (0, 0))
  where
    candidates = dbgVal $ filter cf all
    cf ((_, (cx, cy)), (cdx, cdy)) =
      cdx == 0 && cdy == 0 &&
      (cx /= x || cy /= y) &&
      inRange dx (x + dx - cx) &&
      inRange dy (y + dy - cy) &&
      if abs (x-dx) == abs (y-dy) then
        abs (x-cx) == abs (y-cy) else True
    cs ((_, v1), _) ((_, v2), _) = compare (distance v v2) (distance v v1)
    ((_, (colX, colY)), _) = head $ sortBy cs candidates

inRange _   0   = True
inRange max val = signum max == signum val && abs max >= abs val

staticCol :: [Object] -> [Object]
staticCol =
  map (
    foldr (\(m1, (x, y)) -> \(m2, _) -> (m1+m2, (x, y))) (0, (0, 0))
  ) . groupBy (vPred vecEq) . sortBy (vPred vecCm)
  where
    vecEq (x1, y1) (x2, y2) = x1 == x2 && y1 == y2
    vecCm (x1, y1) (x2, y2)
      | x1 < x2   = LT
      | x1 > x2   = GT
      | y1 < y2   = LT
      | y1 > y2   = GT
      | otherwise = EQ
    vPred op = \(_, v1) -> \(_, v2) -> op v1 v2
