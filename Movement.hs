module Movement where

import Tools

no_swap_slow = True

moveAll :: [Object] -> [Vector]
moveAll objs = map (\o -> foldr vadd (0, 0) $ map (affect o) objs) objs

--        Affected  Affector  Movement
affect :: Object -> Object -> Vector
affect (m1, v1@(x1, y1)) (m2, v2@(x2, y2)) =
  if x1 == x2 && y1 == y2 then (0, 0) else
  if m1 > m2 || distance v1 v2 > length forceList then (0, 0) else
  slow m1 force $
  if x1 == x2 || y1 == y2 then -- linear, pull
    (signum (x2-x1) * force, signum (y2-y1) * force)
  else if adx == ady then -- diagonal, push
    (signum (x1-x2) * force, signum (y1-y2) * force)
  else -- orbit
    (dy-dx, -(dx+dy))
  where
    dv@(dx, dy) = v1 `vsub` v2
    (adx, ady)  = vmap abs dv
    dist        = distance v1 v2
    forceList   = maybe [0] id $ lookup m2 forceMap
    force       = forceList !! (dist-1)

--      Mass   Force
slow :: Int -> Int -> Vector -> Vector
slow 1    _     v       = v
slow mass force (x,y)   = (trunc x x', trunc y y')
  where
    x'         = apply x
    y'         = apply y
    amount     = force - mass - 1
    apply a    = if amount > 0 then a else a + signum a * amount
    trunc a a' = if no_swap_slow && signum a /= signum a' then 0 else a'

finishMove :: [(Object, Vector)] -> [Object]
finishMove = map (\((m, v1), v2) -> (m, v1 `vadd` v2))
