module Data.Puzzles.Sudoku where

import Data.Puzzles.Grid

msqrt :: Integral a => a -> Maybe a
msqrt x = if r ^ 2 == x then Just r else Nothing
    where r = round . sqrt . fromIntegral $ x

mhalf :: Integral a => a -> Maybe a
mhalf x = if even x then Just (x `div` 2) else Nothing

sudokuborders :: Int -> [Edge]
sudokuborders s =
    case msqrt s of
        Just r  -> squareborders r
        Nothing -> case mhalf s of
                       Just h  -> rectborders h
                       Nothing -> error "no sudoku layout of this size"
    where squareborders r = [ E (r*x, y) V | x <- [1..r-1], y <- [0..r*r-1] ]
                            ++ [ E (x, r*y) H | x <- [0..r*r-1], y <- [1..r-1] ]
          rectborders h = [ E (h, y) V | y <- [0..2*h-1] ]
                          ++ [ E (x, 2*y) H | x <- [0..2*h-1], y <- [1..h-1] ]

sudokubordersg :: Grid a -> [Edge]
sudokubordersg g = sudokuborders s
   where (w, h) = size g
         s | w == h    = w
           | otherwise = error "non-square sudoku grid?"