-- vim: shiftwidth=2:expandtab
{-# LANGUAGE TupleSections #-}
module Universe
  {-  ( Vector (..)
  , Universe (..)
  , nextTimeStep
  , movements
  , collisions
  , movePoints
  )-}
    where

import           Data.Bifunctor  (second)
import qualified Data.Map.Strict as M
import           Debug.Trace     (trace)
import           GHC.Stack       (HasCallStack)

import           Utils

data Vector = Vector
  { getX :: Int
  , getY :: Int
  }
  deriving (Eq, Ord)

instance Show Vector where
  show (Vector x y) = "(" <> show x <> " " <> show y <> ")"

instance Num Vector where
  (Vector x1 y1) + (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)
  (Vector x1 y1) * (Vector x2 y2) = Vector (x1 * x2) (y1 * y2)
  abs (Vector x y)                = Vector (abs x) (abs y)
  signum _                        = 1
  fromInteger _                   = Vector 0 0
  negate (Vector x y)             = Vector (-x) (-y)

vplus :: Vector -> Vector -> Vector
vplus (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)

vsum :: [Vector] -> Vector
vsum = foldl1 vplus

data Universe = Universe
  { step   :: Int
  , points :: M.Map Vector Int
  }

canCollide :: Vector -> Bool
canCollide (Vector 0 0) = False
canCollide (Vector 0 _) = True
canCollide (Vector _ 0) = True
canCollide (Vector x y) = abs x == abs y

offsetGen :: Vector -> [Vector]
offsetGen (Vector x y) = zipWith Vector (to x) (to y)

fieldStrengths :: Int -> [Int]
fieldStrengths mass = mass : map helper (fieldStrengths mass)
  where
    helper 0 = 0
    helper 1 = 0
    helper m = c m % sqrt % round

fieldRadius :: Int -> Int
fieldRadius = length . takeWhile (/= 0) . fieldStrengths

distance :: Vector -> Vector -> Int
distance (Vector x1 y1) (Vector x2 y2) = max
  (x1 - x2 % abs) (y1 - y2 % abs)

onMainAxes :: Vector -> Bool
onMainAxes (Vector x y) = 0 `elem` [x, y]

onDiagonals :: Vector -> Bool
onDiagonals (Vector x y) = abs x == abs y

fieldTrace :: [(Vector, [Int])] -> [(Vector, [Int])]
fieldTrace fields = trace (map (second (take 10)) fields % pr) fields

movements :: HasCallStack => M.Map Vector Int -> M.Map Vector (Int, Vector)
movements masses = M.mapWithKey moveOne masses
  where
    fields :: HasCallStack => [(Vector, [Int])]
    fields = M.toList masses % map (second fieldStrengths)

    moveOne :: HasCallStack => Vector -> Int -> (Int, Vector)
    moveOne this mass = (mass, map (affect this) fields % sum)

    affect :: HasCallStack => Vector -> (Vector, [Int]) -> Vector
    affect affected (affector, field)
      | affected     == affector     = Vector 0 0
      | strengthHere == 0            = Vector 0 0
      | onMainAxes relative          = Vector 1 1
      | otherwise                    = Vector 1 1
      where
        strengthHere = field !! (distance affected affector - 1)
        relative     = affected - affector

collisions :: M.Map Vector (Int, Vector) -> M.Map Vector (Int, Vector)
collisions m = M.mapWithKey helper m
  where
    helper :: Vector -> (Int, Vector) -> (Int, Vector)
    helper pos p@(mass, move)
      | canCollide move =
        offsetGen move -- for all offsets:
        % map (pos +)  -- convert to absolute position
        % filter (\v -> m M.!? v -- get point at position
          % maybe False ((==) (Vector 0 0) . snd)) -- doesn't move?
        % safeHead move -- first result or original movement
        % (mass,)       -- recombine with mass
      | otherwise = p

movePoints :: M.Map Vector (Int, Vector) -> M.Map Vector Int
movePoints =
  M.fromListWith (+) -- add masses with same position
  . map (\(pos, (mass, move)) -> (pos + move, mass))
  . M.toList

nextTimeStep :: Universe -> Universe
nextTimeStep u = Universe
  { step = step u + 1
  , points =
      points u
      % movements
      % collisions
      % movePoints
  }
-- TODO increase size & add new mass

