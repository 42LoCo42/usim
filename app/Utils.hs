-- vim: shiftwidth=2:expandtab
module Utils where

import Debug.Trace (trace)
import Linear.V2   as L

pr :: Show a => a -> a
pr a = trace ("\n\ESC[7m" ++ show a ++ "\ESC[27m\n") a

(%) :: a -> (a -> b) -> b
(%) = flip ($)
infixl 0 %

safeHead :: a -> [a] -> a
safeHead a [] = a
safeHead _ as = head as

to :: Int -> [Int]
to n = tail [0, signum n .. n]

c :: (Enum a, Enum b) => a -> b
c = toEnum . fromEnum

vabs :: L.V2 Double -> Double
vabs (L.V2 x y) = sqrt (x ** 2.0 + y ** 2.0)
