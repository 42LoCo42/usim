module Main where

import Movement
import Collisions
import Tools
import Modulus
import Debug

uCycle :: Int -> [Object] -> [Object]
uCycle t objs =
  map (modulObj max min) $ staticCol $ finishMove $ linCols
  where
    linCols = dbgVal $ map (linearCol withVec) withVec
    withVec = zip objs $ moveAll objs
    max     = t-1
    min     = -t

main = do
  let objs = [
        (1, (0, 0)),
        (1, (0, 1)),
        (1, (1, 0)),
        (1, (1, 1)),
        (1, (-1, 0)),
        (1, (-2, 1)),
        (1, (-1, -1)),
        (1, (-1, -2)),
        (1, (-2, -1)),
        (1, (-2, -2)),
        (1, (0, -1)),
        (1, (1, -2))
        ]
  print $ uCycle 2 objs
