module Data.Puzzles.Grid where

import Data.Maybe
import Text.Read

newtype Grid a = GG {unGG :: [[a]]}
    deriving Show

type Point = (Int, Int)

(!) :: Grid a -> Point -> a
(!) g@(GG g') (x, y) = g' !! (sy - y - 1) !! x
    where (_, sy) = size g

instance Functor Grid where
    fmap f = GG . map (map f) . unGG

fromListList :: [[a]] -> Grid a
fromListList = GG

size :: Grid a -> (Int, Int)
size (GG g) = (length (head g), length g)

inBounds :: Grid a -> Point -> Bool
inBounds g (x, y) = x >= 0 && y >= 0 && x < sx && y < sy
    where (sx, sy) = size g

neighbours :: Grid a -> Point -> [Point]
neighbours g p = filter (inBounds g) . map (add p) $ deltas
    where deltas = [ (dx, dy)
                   | dx <- [-1..1], dy <- [-1..1]
                   , dx /= 0 || dy /= 0
                   ]
          add :: Point -> Point -> Point
          add (x, y) (x', y') = (x + x', y + y')

points :: Grid a -> [Point]
points g = [ (x, y) | x <- [0..sx-1], y <- [0..sy-1] ]
    where (sx, sy) = size g

clues :: Grid (Maybe a) -> [(Point, a)]
clues g = [ (p, fromJust $ g ! p) | p <- points g
                                  , isJust $ g ! p ]

data Dir = V | H
    deriving (Eq, Ord, Show)

data Edge = E Point Dir
    deriving (Show, Eq, Ord)

borders :: Eq a => Grid a -> [Edge]
borders g = [ E p V | p <- vborders ] ++ [ E p H | p <- hborders ]
    where
        borders' f (sx, sy) = [ (x + 1, y) | x <- [0 .. sx - 2]
                                           , y <- [0 .. sy - 1]
                                           , f (x, y) /= f (x + 1, y) ]
        vborders = borders' (g !) (size g)
        hborders = map swap $ borders' ((g !) . swap) (swap . size $ g)
        swap (x, y) = (y, x)

data OutsideClues a = OC { left :: [a], right :: [a], bottom :: [a], top :: [a] }

clueso (OC l r b t) = catMaybes . map liftMaybe . concat $
                             [ zipWith (\ y c -> ((-1, y), c)) [0..h-1] l
                             , zipWith (\ y c -> (( w, y), c)) [0..h-1] r
                             , zipWith (\ x c -> (( x,-1), c)) [0..w-1] b
                             , zipWith (\ x c -> (( x, h), c)) [0..w-1] t
                             ]
    where w = length b
          h = length l
          liftMaybe (p, Just x) = Just (p, x)
          liftMaybe (p, Nothing) = Nothing
