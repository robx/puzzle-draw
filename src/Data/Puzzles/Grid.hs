module Data.Puzzles.Grid where

import Data.Maybe

newtype Grid a = GG {unGG :: [[a]]}
    deriving Show

type CharGrid = Grid Char

type IntClue = Maybe Int
type CharClue = Maybe Char
type IntGrid = Grid IntClue

data MasyuPearl = MWhite | MBlack
type MasyuClue = Maybe MasyuPearl

type MasyuGrid = Grid MasyuClue

size :: Grid a -> (Int, Int)
size (GG g) = (length (head g), length g)

type Point = (Int, Int)

(!) :: Grid a -> Point -> a
g@(GG g') ! (x, y) = g' !! (sy - y - 1) !! x
    where (_, sy) = size g

instance Functor Grid where
    fmap f = GG . map (map f) . unGG

fromListList :: [[a]] -> Grid a
fromListList = GG

charToIntClue c
    | '0' <= c && c <= '9'  = Just $ fromEnum c - fromEnum '0'
    | c == ' ' || c == '.'  = Nothing

charToCharClue c
    | c == ' ' || c == '.'  = Nothing
    | otherwise             = Just c

charToMasyuClue :: Char -> MasyuClue
charToMasyuClue '*' = Just MBlack
charToMasyuClue 'o' = Just MWhite
charToMasyuClue c
    | c == ' ' || c == '.'  = Nothing

points g = [ (x, y) | x <- [0..sx-1], y <- [0..sy-1] ]
    where (sx, sy) = size g

clues g = [ (p, fromJust $ g ! p) | p <- points g
                                  , isJust $ g ! p ]

data Dir = V | H
    deriving (Eq, Ord)

instance Show Dir where
    show V = "|"
    show H = "-"

data Edge = E Point Dir
    deriving (Show, Eq, Ord)

swap (x, y) = (y, x)

borders :: Eq a => Grid a -> [Edge]
borders g = [ E p V | p <- vborders ] ++ [ E p H | p <- hborders ]
    where
        borders' f (sx, sy) = [ (x + 1, y) | x <- [0 .. sx - 2]
                                           , y <- [0 .. sy - 1]
                                           , f (x, y) /= f (x + 1, y) ]
        vborders = borders' (g !) (size g)
        hborders = map swap $ borders' ((g !) . swap) (swap . size $ g)
