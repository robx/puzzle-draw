{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Grid shapes.
module Data.GridShape
  ( Coord,
    Size,
    Square (..),
    Dir (..),
    Edge (..),
    Dir' (..),
    Edge' (..),
    CornerDir (..),
    Dual2D (..),
    Key,
    Dual',
    C (..),
    N (..),
    ShiftC (..),
    FromCoord (..),
    ToCoord (..),
    edge,
    edge',
    edgeBetween,
    edgeBetween',
    orient,
    ends',
    revEdge,
    edges,
    edgesM,
    ends,
    unorient,
    dualE,
    vertexNeighbours,
    edgeNeighbours,
    rows,
    shift,
    mapEdge,
  )
where

import Data.AffineSpace
import qualified Data.Foldable as F
import Data.List
  ( groupBy,
    partition,
    sortOn,
  )
import qualified Data.Map.Strict as Map

type Coord = (Int, Int)

class FromCoord a where
  fromCoord :: Coord -> a

class ToCoord a where
  toCoord :: a -> Coord

data C = C !Int !Int
  deriving (Show, Eq, Ord)

instance FromCoord C where
  fromCoord = uncurry C

instance ToCoord C where
  toCoord (C x y) = (x, y)

instance AffineSpace C where

  type Diff C = (Int, Int)

  (C x y) .-. (C x' y') = (x - x', y - y')

  (C x y) .+^ (x', y') = C (x + x') (y + y')

newtype ShiftC = ShiftC C
  deriving (Show, Eq, Ord)

instance ToCoord ShiftC where
  toCoord (ShiftC c) = toCoord c

instance FromCoord ShiftC where
  fromCoord = ShiftC . fromCoord

data N = N !Int !Int
  deriving (Show, Eq, Ord)

instance FromCoord N where
  fromCoord = uncurry N

instance ToCoord N where
  toCoord (N x y) = (x, y)

instance AffineSpace N where

  type Diff N = (Int, Int)

  (N x y) .-. (N x' y') = (x - x', y - y')

  (N x y) .+^ (x', y') = N (x + x') (y + y')

-- | A standard square grid, with cells and vertices
--   indexed by pairs of integers in mathematical coordinates.
--   The bottom-left corner is vertex (0, 0), the bottom-left
--   cell is cell (0, 0).
data Square = Square
  deriving (Show, Eq)

squareNeighbours :: [(Int, Int)] -> C -> [C]
squareNeighbours deltas c = map (c .+^) deltas

vertexNeighbours :: C -> [C]
vertexNeighbours =
  squareNeighbours
    [(dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]

edgeNeighbours :: C -> [C]
edgeNeighbours = squareNeighbours [(1, 0), (-1, 0), (0, 1), (0, -1)]

-- | Edge direction in a square grid, vertical or horizontal.
data Dir = Vert | Horiz
  deriving (Eq, Ord, Show)

-- | An edge in a square grid, going up or right from the given cell
--   centre.
data Edge a = E a Dir
  deriving (Show, Eq, Ord)

mapEdge :: (a -> b) -> Edge a -> Edge b
mapEdge f (E x d) = E (f x) d

type Size = (Int, Int)

data CornerDir = UL | UR | DL | DR
  deriving (Eq, Ord, Show)

-- | Oriented edge direction in a square grid.
data Dir' = U | D | L | R
  deriving (Eq, Ord, Show)

toDir' :: (Int, Int) -> Dir'
toDir' (1, 0) = R
toDir' (0, 1) = U
toDir' (-1, 0) = L
toDir' (0, -1) = D
toDir' _ = error "non-primitive vector"

-- | An oriented edge in a square grid.
data Edge' a = E' a Dir'
  deriving (Eq, Ord, Show)

edge' :: (AffineSpace a, Diff a ~ (Int, Int)) => a -> a -> Edge' a
edge' p q = E' p (toDir' (q .-. p))

edge :: (AffineSpace a, Diff a ~ (Int, Int)) => a -> a -> Edge a
edge p q = unorient $ edge' p q

class Dual2D a where

  type Dual a :: *

  dualE' :: Edge' a -> Edge' (Dual a)

dualE :: Dual' a => Edge a -> Edge (Dual a)
dualE = unorient . dualE' . orient

type Key k = (AffineSpace k, Diff k ~ (Int, Int), Ord k, FromCoord k)

type Dual' k =
  ( Key k,
    Dual2D k,
    Key (Dual k),
    Dual2D (Dual k),
    Dual (Dual k) ~ k
  )

instance Dual2D N where

  type Dual N = C

  dualE' (E' (N x y) R) = E' (C x (y -1)) U
  dualE' (E' (N x y) U) = E' (C x y) L
  dualE' (E' (N x y) L) = E' (C (x -1) y) D
  dualE' (E' (N x y) D) = E' (C (x -1) (y -1)) R

instance Dual2D C where

  type Dual C = N

  dualE' (E' (C x y) R) = E' (N (x + 1) y) U
  dualE' (E' (C x y) U) = E' (N (x + 1) (y + 1)) L
  dualE' (E' (C x y) L) = E' (N x (y + 1)) D
  dualE' (E' (C x y) D) = E' (N x y) R

ends :: (AffineSpace a, Diff a ~ (Int, Int)) => Edge a -> (a, a)
ends (E x Vert) = (x, x .+^ (0, 1))
ends (E x Horiz) = (x, x .+^ (1, 0))

ends' :: (AffineSpace a, Diff a ~ (Int, Int)) => Edge' a -> (a, a)
ends' (E' x U) = (x, x .+^ (0, 1))
ends' (E' x R) = (x, x .+^ (1, 0))
ends' (E' x D) = (x, x .+^ (0, -1))
ends' (E' x L) = (x, x .+^ (-1, 0))

revEdge :: (AffineSpace a, Diff a ~ (Int, Int)) => Edge' a -> Edge' a
revEdge = uncurry edge' . swap . ends' where swap (x, y) = (y, x)

unorient :: (AffineSpace a, Diff a ~ (Int, Int)) => Edge' a -> Edge a
unorient (E' x U) = E x Vert
unorient (E' x R) = E x Horiz
unorient (E' x D) = E (x .-^ (0, 1)) Vert
unorient (E' x L) = E (x .-^ (1, 0)) Horiz

orient :: Edge a -> Edge' a
orient (E x Vert) = E' x U
orient (E x Horiz) = E' x R

edgeBetween' :: Dual' k => k -> k -> Edge' (Dual k)
edgeBetween' p q = dualE' (edge' p q)

edgeBetween :: Dual' k => k -> k -> Edge (Dual k)
edgeBetween p q = unorient $ edgeBetween' p q

-- | @edges@ computes the outer and inner edges of a set of cells.
--   The set is given via fold and membership predicate, the result
--   is a pair @(outer, inner)@ of lists of edges, where the outer
--   edges are oriented such that the outside is to the left.
edges ::
  (Dual' k, Foldable f) =>
  f k ->
  (k -> Bool) ->
  ([Edge' (Dual k)], [Edge (Dual k)])
edges cs isc = F.foldr f ([], []) cs
  where
    f c (outer, inner) = (newout ++ outer, newin ++ inner)
      where
        nbrs = [c .+^ d | d <- [(-1, 0), (0, 1), (1, 0), (0, -1)]]
        (ni, no) = partition isc nbrs
        newout = [edgeBetween' q c | q <- no]
        newin = [edgeBetween q c | q <- ni, c >= q]

edgesM :: Dual' k => Map.Map k a -> ([Edge' (Dual k)], [Edge (Dual k)])
edgesM m = edges (Map.keysSet m) (`Map.member` m)

rows :: Map.Map C a -> [[a]]
rows g =
  map (map snd) $
    grouped byRow (Map.toList g)
      ++ grouped
        byCol
        (Map.toList g)
  where
    byRow (C _ y, _) = y
    byCol (C x _, _) = x
    grouped :: (Ord b, Eq b) => (a -> b) -> [a] -> [[a]]
    grouped f = map (map snd) . groupOn fst . sortOn fst . map (\x -> (f x, x))
    groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
    groupOn f = groupBy (\x y -> f x == f y)

shift :: (AffineSpace a, Diff a ~ (Int, Int)) => (Int, Int) -> Edge a -> Edge a
shift delta (E x dir) = E (x .+^ delta) dir
