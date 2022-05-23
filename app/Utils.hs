-- vim: shiftwidth=2:expandtab
module Utils where

pr :: Show a => a -> String
pr a = "\n\ESC[7m" ++ show a ++ "\ESC[27m\n"

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
