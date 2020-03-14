module Main where

import Movement
import Collisions
import Tools
import Modulus

uCycle :: Int -> [Object] -> [Object]
uCycle t objs =
  map (modulObj max min) $ staticCol $ finishMove $ linCols
  where
    linCols = map (linearCol withVec) withVec
    withVec = zip objs $ moveAll objs
    max     = t-1
    min     = -t

main = do
  let objs = [
        (10, (-1, 0)),
        (10, (1, 0)),
        (1, (0, 1)),
        (2, (0, 5)),
        (3, (0, 7))
        ]
  print $ uCycle 1 objs
