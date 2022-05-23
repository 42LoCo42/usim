-- vim: shiftwidth=2:expandtab
{-# LANGUAGE TupleSections #-}
module Universe
  ( Vector (..)
  , Universe (..)
  , nextTimeStep
  , collisions
  , movePoints
  ) where

import qualified Data.Map.Strict as M

import Utils

data Vector = Vector
  { getX :: Int
  , getY :: Int
  }
  deriving (Eq, Ord)

instance Show Vector where
  show (Vector x y) = "(" <> show x <> " " <> show y <> ")"

instance Num Vector where
  (+) (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)
  (*)         = undefined
  abs         = undefined
  fromInteger = undefined
  negate      = undefined
  signum      = undefined

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

-- TODO cache largest mass?
movements :: M.Map Vector Int -> M.Map Vector (Int, Vector)
movements = undefined

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

