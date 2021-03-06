module Tools where

import System.Random

type Model = ([Object], Int, StdGen)

--             X    Y
type Vector = (Int, Int)

--             Mass  Position
type Object = (Int, Vector)

forceMap :: [(Int, [Int])]
forceMap = map (\a -> (a, forces a)) [1 ..]

vmerge :: (Int -> Int -> Int) -> Vector -> Vector -> Vector
vmerge op (x1, y1) (x2, y2) = (op x1 x2, op y1 y2)

vmap :: (Int -> Int) -> Vector -> Vector
vmap f (x, y) = (f x, f y)

vadd :: Vector -> Vector -> Vector
vadd = vmerge (+)

vsub :: Vector -> Vector -> Vector
vsub = vmerge (-)

itod :: (Real a, Fractional b) => a -> b
itod = fromRational . toRational

round' :: (RealFrac a, Integral b) => a -> b
round' a = if abs (a - itod down) >= 0.5 then up else down
  where
    swap f1 f2 x = if signum x >= 0 then f1 x else f2 x
    down = swap floor ceiling a
    up = swap ceiling floor a

forces :: Int -> [Int]
forces 1 = [1]
forces f = f : forces (round' (sqrt $ itod f :: Double))

distance :: Vector -> Vector -> Int
distance v1 v2 = max x y
  where
    (x, y) = vmap abs $ v1 `vsub` v2

distinctRandoms :: (Random a, Integral a, RandomGen g) => a -> Int -> g -> [a]
distinctRandoms upperBound count gen
  | count > fromEnum upperBound = []
  | otherwise =
    helper count [] ((`mod` upperBound) . abs <$> randoms gen)
  where
    helper count' have (new : rans)
      | length have == count' = have
      | otherwise =
        if new `elem` have
          then helper count' have rans
          else helper count' (new : have) rans
    helper _ _ _ = []
