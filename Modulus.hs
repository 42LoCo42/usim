module Modulus where

import Tools

modulCoord :: Int -> Int -> Int -> Int
modulCoord max min c
  | c < min   = modulCoord max min (c-2*min)
  | c > max   = modulCoord max min (c+2*min)
  | otherwise = c

modulObj :: Int -> Int -> Object -> Object
modulObj max min (m, (x, y)) = (m, (x', y'))
  where
    x' = modulCoord max min x
    y' = modulCoord max min y
