{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

-- | Grid shapes.
module Data.Puzzles.GridShape where

import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.List (partition)
import qualified Data.Map as Map
import Data.VectorSpace ((^+^), (^-^))
import Control.Arrow ((***))

-- | The geometry of a grid.
class Show (Cell a) => GridShape a where
    type Cell     a :: *
    type Vertex   a :: *

    vertexNeighbours :: a -> Cell a -> [Cell a]
    edgeNeighbours :: a -> Cell a -> [Cell a]

-- | A standard square grid, with cells and vertices
--   indexed by pairs of integers in mathematical coordinates.
--   The bottom-left corner is vertex (0, 0), the bottom-left
--   cell is cell (0, 0).
data Square = Square
    deriving (Show, Eq)

squareNeighbours :: [(Int, Int)] -> Square -> Cell Square -> [Cell Square]
squareNeighbours deltas Square c = map (c ^+^) deltas

instance GridShape Square where
    type Cell Square     = (Int, Int)
    type Vertex Square   = (Int, Int)

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
type Size = (Int, Int)

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

dualEdge :: Cell Square -> Cell Square -> Edge
dualEdge p q = case q ^-^ p of
    (1, 0)  -> E p H
    (-1,0)  -> E q H
    (0, 1)  -> E p V
    (0,-1)  -> E q V
    _       -> error "non-neighbouring cells"

forgetOrientation :: Edge' (Cell Square) -> Edge
forgetOrientation (E' x U) = E x V
forgetOrientation (E' x R) = E x H
forgetOrientation (E' x D) = E (x ^-^ (0,1)) V
forgetOrientation (E' x L) = E (x ^-^ (1,0)) H

unorientedEdge :: Cell Square -> Cell Square -> Edge
unorientedEdge p q = forgetOrientation $ orientedEdge p q

-- | @edges@ computes the outer and inner edges of a set of cells.
--   The set is given via fold and membership predicate, the result
--   is a pair @(outer, inner)@ of lists of edges, where the outer
--   edges are oriented such that the outside is to the left.
edgesPair :: Foldable f =>
             f (Cell Square) -> (Cell Square -> Bool) ->
             ([(Cell Square, Cell Square)],
              [(Cell Square, Cell Square)])
edgesPair cs isc = F.foldr f ([], []) cs
  where
    f c (outer, inner) = (newout ++ outer, newin ++ inner)
      where
        nbrs = [ c ^+^ d | d <- [(-1,0), (0,1), (1,0), (0,-1)] ]
        (ni, no) = partition isc nbrs
        newout = map ((,) c) no
        newin = map (sortPair . (,) c) . filter (c >=) $ ni
        sortPair (x, y) = if x < y then (x, y) else (y, x)

edgesPairM :: Map.Map (Cell Square) a ->
              ([(Cell Square, Cell Square)],
               [(Cell Square, Cell Square)])
edgesPairM m = edgesPair (Map.keysSet m) (`Map.member` m)

-- | @edges@ computes the outer and inner edges of a set of cells.
--   The set is given via fold and membership predicate, the result
--   is a pair @(outer, inner)@ of lists of edges, where the outer
--   edges are oriented such that the outside is to the left.
edges :: Foldable f =>
           f (Cell Square) -> (Cell Square -> Bool) ->
           ([Edge' (Vertex Square)], [Edge' (Vertex Square)])
edges cs isc = map (uncurry orientedEdge) *** map (uncurry orientedEdge)
             $ edgesPair cs isc
