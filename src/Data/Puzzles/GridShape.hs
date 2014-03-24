{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Data.Puzzles.GridShape where

class Show (Cell a) => GridShape a where
    type GridSize a :: *
    type Cell     a :: *
    type Vertex   a :: *

    size :: a -> GridSize a
    cells :: a -> [Cell a]
    vertices :: a -> [Vertex a]
    neighbours :: a -> Cell a -> [Cell a]

data Square = Square Int Int
    deriving Show

instance GridShape Square where
    type GridSize Square = (Int, Int)
    type Cell Square     = (Int, Int)
    type Vertex Square   = (Int, Int)

    size (Square w h)       = (w, h)
    cells (Square w h)      = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
    vertices (Square w h)   = [(x, y) | x <- [0..w], y <- [0..h]]
    neighbours (Square w h) c = filter inBounds . map (add c) $ deltas
      where
        inBounds (x, y) = x >= 0 && x < w && y >= 0 && y < h
        deltas = [ (dx, dy)
                 | dx <- [-1..1], dy <- [-1..1]
                 , dx /= 0 || dy /= 0
                 ]
        add (x, y) (x', y') = (x + x', y + y')


data Dir = V | H
    deriving (Eq, Ord, Show)

data Edge = E (Cell Square) Dir
    deriving (Show, Eq, Ord)

type Coord = Cell Square
type Size = GridSize Square
