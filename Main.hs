module Main where

import Movement
import Collisions
import Tools
import Modulus

uCycle :: Int -> [Object] -> [Object]
uCycle t objs =
  staticCol $ map (modulObj max min) $ finishMove $ linCols
  where
    linCols = map (linearCol withVec) withVec
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
