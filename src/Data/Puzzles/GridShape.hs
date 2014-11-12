{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

-- | Grid shapes.
module Data.Puzzles.GridShape where

import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.List (partition)
import Data.VectorSpace ((^+^))

-- | The geometry of a grid.
class Show (Cell a) => GridShape a where
    type GridSize a :: *
    type Cell     a :: *
    type Vertex   a :: *

    size :: a -> GridSize a
    cells :: a -> [Cell a]
    vertices :: a -> [Vertex a]
    vertexNeighbours :: a -> Cell a -> [Cell a]
    edgeNeighbours :: a -> Cell a -> [Cell a]

-- | A standard square grid, with cells and vertices
--   indexed by pairs of integers in mathematical coordinates.
--   The bottom-left corner is vertex (0, 0), the bottom-left
--   cell is cell (0, 0).
data Square = Square !Int !Int
    deriving (Show, Eq)

squareNeighbours :: [(Int, Int)] -> Square -> Cell Square -> [Cell Square]
squareNeighbours deltas (Square w h) c = filter inBounds . map (add c) $ deltas
  where
    inBounds (x, y) = x >= 0 && x < w && y >= 0 && y < h
    add (x, y) (x', y') = (x + x', y + y')

instance GridShape Square where
    type GridSize Square = (Int, Int)
    type Cell Square     = (Int, Int)
    type Vertex Square   = (Int, Int)

    size (Square w h)       = (w, h)
    cells (Square w h)      = [(x, y) | x <- [0..w-1], y <- [0..h-1]]
    vertices (Square w h)   = [(x, y) | x <- [0..w], y <- [0..h]]
    vertexNeighbours = squareNeighbours [ (dx, dy)
                                        | dx <- [-1..1], dy <- [-1..1]
                                        , dx /= 0 || dy /= 0
                                        ]
    edgeNeighbours = squareNeighbours [ (dx, dy)
                                      | dx <- [-1..1], dy <- [-1..1]
                                      , dx /= 0 || dy /= 0
                                      , dx == 0 || dy == 0
                                      ]

-- | Edge direction in a square grid, vertical or horizontal.
data Dir = V | H
    deriving (Eq, Ord, Show)

-- | An edge in a square grid, going up or right from the given cell
--   centre.
data Edge = E (Cell Square) Dir
    deriving (Show, Eq, Ord)

type Coord = Cell Square
type Size = GridSize Square

-- | Oriented edge direction in a square grid.
data Dir' = U | D | L | R
    deriving (Eq, Ord, Show)

-- | An oriented edge in a square grid.
--   @a@ should be @Cell Square@ or @Vertex Square@.
data Edge' a = E' a Dir'
    deriving (Eq, Ord, Show)

-- | The edge between two neighbouring cells, with the first cell
--   on the left.
orientedEdge :: Cell Square -> Cell Square -> Edge' (Vertex Square)
orientedEdge (x,y) (x',y')
    | x' == x && y' == y+1  = E' (x,y+1) R
    | x' == x && y' == y-1  = E' (x+1,y) L
    | x' == x+1 && y' == y  = E' (x+1,y+1) D
    | x' == x-1 && y' == y  = E' (x,y) U
    | otherwise             = error $ "not neighbours: " ++
                                      show (x,y) ++ " " ++  show (x',y')

-- | @edges@ computes the outer and inner edges of a set of cells.
--   The set is given via fold and membership predicate, the result
--   is a pair @(outer, inner)@ of lists of edges, where the outer
--   edges are oriented such that the outside is to the left.
edges :: Foldable f =>
           f (Cell Square) -> (Cell Square -> Bool) ->
           ([Edge' (Vertex Square)], [Edge' (Vertex Square)])
edges cs isc = F.foldr f ([], []) cs
  where
    f c (outer, inner) = (newout ++ outer, newin ++ inner)
      where
        nbrs = [ c ^+^ d | d <- [(-1,0), (0,1), (1,0), (0,-1)] ]
        (ni, no) = partition isc nbrs
        newout = map (orientedEdge c) no
        newin = map (orientedEdge c) . filter (c >=) $ ni
