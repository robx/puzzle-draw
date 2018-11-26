module Data.Sudoku
  ( sudokuborders
  , sudokubordersg
  )
where

import qualified Data.Map.Strict               as Map

import           Data.Grid
import           Data.GridShape

msqrt :: Integral a => a -> Maybe a
msqrt x = if r ^ (2 :: Int) == x then Just r else Nothing
  where r = round . (sqrt :: Double -> Double) . fromIntegral $ x

mhalf :: Integral a => a -> Maybe a
mhalf x = if even x then Just (x `div` 2) else Nothing

-- | Determine the internal borders of a standard sudoku of the
--   given size.
sudokuborders :: Int -> [Edge N]
sudokuborders s = case msqrt s of
  Just r  -> squareborders r
  Nothing -> case mhalf s of
    Just h  -> rectborders h
    Nothing -> error "no sudoku layout of this size"
 where
  squareborders r =
    [ E (N (r * x) y) Vert | x <- [1 .. r - 1], y <- [0 .. r * r - 1] ]
      ++ [ E (N x (r * y)) Horiz | x <- [0 .. r * r - 1], y <- [1 .. r - 1] ]
  rectborders h =
    [ E (N h y) Vert | y <- [0 .. 2 * h - 1] ]
      ++ [ E (N x (2 * y)) Horiz | x <- [0 .. 2 * h - 1], y <- [1 .. h - 1] ]

-- | Determine the internal borders of a standard sudoku of the
--   on the given grid.
sudokubordersg :: Grid C a -> [Edge N]
sudokubordersg = sudokuborders . fst . size . Map.mapKeys toCoord
