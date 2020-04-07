module Collisions where

import Tools
import Data.List (groupBy, sortBy, sortOn)

linearCol :: [(Object, Vector)] -> (Object, Vector) -> (Object, Vector)
linearCol all o@((m, (x, y)), v@(dx, dy)) =
  if null candidates then o else
    ((m, (colX, colY)), (0, 0))
  where
    candidates = filter cf all
    cf ((_, (cx, cy)), (cdx, cdy)) =
      cdx == 0 && cdy == 0 &&     -- doesn't move
      (cx /= x || cy /= y) &&     -- is not us
      inRange dx (x + dx - cx) && -- x range
      inRange dy (y + dy - cy) && -- y range
      if abs dx == abs dy then    -- if we move diagonal
        tx >= 0 && ty >= 0 && tx == ty -- transformed diffs match our direction and are equal
      else True
      where
        tx = (cx-x)*signum dx
        ty = (cy-y)*signum dy

    cs ((_, v1), _) ((_, v2), _) = compare (distance v v2) (distance v v1)
    ((_, (colX, colY)), _) = head $ sortBy cs candidates

inRange :: Int -> Int -> Bool
inRange _   0   = True
inRange max val = signum max == signum val && abs max >= abs val

staticCol :: [Object] -> [Object]
staticCol =
  map (
    foldr1 (\(m1, (x, y)) -> \(m2, _) -> (m1+m2, (x, y)))
  ) . groupBy (\(_, p1) -> \(_, p2) -> p1 == p2) . sortOn snd
