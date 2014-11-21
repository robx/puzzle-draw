module Data.Puzzles.Sudoku (
    sudokuborders,
    sudokubordersg
  ) where

import qualified Data.Map as Map

import Data.Puzzles.Grid
import Data.Puzzles.GridShape

msqrt :: Integral a => a -> Maybe a
msqrt x = if r ^ (2 :: Int) == x then Just r else Nothing
    where r = round . (sqrt :: Double -> Double) . fromIntegral $ x

mhalf :: Integral a => a -> Maybe a
mhalf x = if even x then Just (x `div` 2) else Nothing

-- | Determine the internal borders of a standard sudoku of the
--   given size.
sudokuborders :: Int -> [Edge]
sudokuborders s =
    case msqrt s of
        Just r  -> squareborders r
        Nothing -> case mhalf s of
                       Just h  -> rectborders h
                       Nothing -> error "no sudoku layout of this size"
    where squareborders r = [ E (r*x, y) Vert | x <- [1..r-1], y <- [0..r*r-1] ]
                            ++ [ E (x, r*y) Horiz | x <- [0..r*r-1], y <- [1..r-1] ]
          rectborders h = [ E (h, y) Vert | y <- [0..2*h-1] ]
                          ++ [ E (x, 2*y) Horiz | x <- [0..2*h-1], y <- [1..h-1] ]

-- | Determine the internal borders of a standard sudoku of the
--   on the given grid.
sudokubordersg :: SGrid a -> [Edge]
sudokubordersg = sudokuborders
               . (+1) . maximum . map (uncurry max) . Map.keys
               . contents
