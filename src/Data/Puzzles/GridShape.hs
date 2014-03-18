{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Data.Puzzles.GridShape where

class Show (Cell a) => GridShape a where
    type GridSize a :: *
    type Cell     a :: *
    type Vertex   a :: *

    size :: a -> GridSize a
    cells :: a -> [Cell a]
    vertices :: a -> [Vertex a]

data Square = Square Int Int
    deriving Show

instance GridShape Square where
    type GridSize Square = (Int, Int)
    type Cell Square     = (Int, Int)
    type Vertex Square   = (Int, Int)

    size (Square w h)     = (w, h)
    cells (Square w h)    = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
    vertices (Square w h) = [(x, y) | x <- [0..w], y <- [0..h]]

--data Dir = V | H
--type EdgeI = (VertexI, Dir)
--size (SquareGrid w h) = (w, h)

--cellPos :: SquareGrid -> CellI -> P2
--vertexPos :: SquareGrid -> VertexI -> P2

--Ord CellI, VertexI?

--Edge = (CellI, CellI)? (VertexI, VertexI)?
--DualEdge = (CellI, CellI)?
