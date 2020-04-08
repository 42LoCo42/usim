module Update where

import Movement
import Collisions
import Tools
import Modulus

import System.Random (split)

uCycle :: Int -> [Object] -> [Object]
uCycle t objs =
  staticCol $ map (modulObj max min) $ finishMove $ linCols
  where
    linCols = map (linearCol withVec) withVec
    withVec = zip objs $ moveAll objs
    max     = t-1
    min     = -t

update :: Model -> Model
update (objs, time, gen) = (objs'', time+1, gen')
  where
    objs'       = uCycle time objs
    objs''      = objs' ++ map ((,) 1 . ix2vec (time+1)) rawRans
    (use, gen') = split gen
    rawRans     = distinctRandoms (8*time-4) (4*time) use :: [Int]

ix2vec :: Int -> Int -> Vector
ix2vec t ix
  | ix < 2*t-1 = (t-1, 1-t + ix)
  | ix < 4*t-2 = (3*t-3 - ix, t-1)
  | ix < 6*t-3 = (-t, 5*t-4 - ix)
  | otherwise  = (-7*t+4 + ix, -t)
