module Movement where

import Data.Maybe (fromMaybe)
import Tools

noSwapSlow :: Bool
noSwapSlow = True

moveAll :: [Object] -> [Vector]
moveAll objs = map (\o -> foldr (vadd . affect o) (0, 0) objs) objs

--        Affected  Affector  Movement
affect :: Object -> Object -> Vector
affect (m1, v1@(x1, y1)) (m2, v2@(x2, y2))
  | x1 == x2 && y1 == y2 =
    (0, 0)
  | m1 > m2 || distance v1 v2 > length forceList =
    (0, 0)
  | otherwise =
    slow m1 force $
      if x1 == x2 || y1 == y2 -- linear, pull
        then (signum (x2 - x1) * force, signum (y2 - y1) * force)
        else
          if adx == ady -- diagonal, push
            then (signum (x1 - x2) * force, signum (y1 - y2) * force)
            else-- orbit
              (dy - dx, - (dx + dy))
  where
    dv@(dx, dy) = v1 `vsub` v2
    (adx, ady) = vmap abs dv
    dist = distance v1 v2
    forceList = fromMaybe [0] $ lookup m2 forceMap
    force = forceList !! (dist -1)

--      Mass   Force
slow :: Int -> Int -> Vector -> Vector
slow 1 _ v = v
slow mass force (x, y) = (trunc x x'', trunc y y'')
  where
    x' = apply x
    y' = apply y
    x'' = interfere y y' x'
    y'' = interfere x x' y'
    amount = force - mass - 1
    apply a = if amount > 0 then a else a + signum a * amount
    interfere a a' b' = if signdiff a a' then b' + (if signdiff a' b' then 1 else -1) * a' else b'
    trunc a a' = if noSwapSlow && signdiff a a' then 0 else a'
    signdiff a a' = signum a /= signum a'

finishMove :: [(Object, Vector)] -> [Object]
finishMove = map (\((m, v1), v2) -> (m, v1 `vadd` v2))
