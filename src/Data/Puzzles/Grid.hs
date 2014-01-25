{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Data.Puzzles.Grid where

import Data.Maybe

newtype CharGrid = CG {unCG :: [[Char]]}
newtype NumGrid = NG {unNG :: [[Char]]}

class Grid a b | a -> b where
    size :: a -> (Int, Int)
    (!) :: a -> (Int, Int) -> b

instance Grid CharGrid Char where
    size (CG g) = (length (head g), length g)
    (CG g) ! (x, y) = g !! (sy - y - 1) !! x
        where (_, sy) = size (CG g)

newtype IntClue = IC {unIC :: Maybe Int}

instance Grid NumGrid IntClue where
    size (NG g) = (length (head g), length g)
    (NG g) ! (x, y) = v
        where (_, sy) = size (CG g)
              c = g !! (sy - y - 1) !! x
              v | '0' <= c && c <= '9'  = IC . Just $ fromEnum c - fromEnum '0'
                | c == ' ' || c == '.'  = IC Nothing


type Point = (Int, Int)

points g = [ (x, y) | x <- [0..sx-1], y <- [0..sy-1] ]
    where (sx, sy) = size g

clues g = [ (p, fromJust . unIC $ g ! p) | p <- points g
                                         , isJust . unIC $ g ! p ]

data Dir = V | H
--    deriving Show
data Edge = E Point Dir
    deriving Show

instance Show Dir where
    show V = "|"
    show H = "-"

swap (x, y) = (y, x)

borders :: (Eq b, Grid a b) => a -> [Edge]
borders g = [ E p V | p <- vborders ] ++ [ E p H | p <- hborders ]
    where
        borders' f (sx, sy) = [ (x + 1, y) | x <- [0 .. sx - 2]
                                           , y <- [0 .. sy - 1]
                                           , f (x, y) /= f (x + 1, y) ]
        vborders = borders' (g !) (size g)
        hborders = map swap $ borders' ((g !) . swap) (swap . size $ g)
