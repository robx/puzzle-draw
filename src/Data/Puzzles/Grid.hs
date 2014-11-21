{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Puzzle grids.
module Data.Puzzles.Grid
    (
      Grid
    , AreaGrid
    , ShadedGrid
    , size
    , clues
    , nodeGrid
    , cellGrid

    , borders
    , edgesGen

    , OutsideClues(..)
    , outsideSize
    , outsideClues
    , multiOutsideClues
    , outsideGrid
    ) where

import qualified Data.Map as Map
import Data.AffineSpace
import Data.VectorSpace

import Data.Puzzles.GridShape

type Grid k a = Map.Map k a

type AreaGrid = Grid C Char
type ShadedGrid = Grid C Bool

-- | For a grid with value type @Maybe a@, return an association
--   list of cells and @Just@ values.
clues :: Grid k (Maybe a) -> Grid k a
clues = Map.mapMaybe id

edgesGen :: Dual' k
         => (a -> a -> Bool) -> (a -> Bool) -> Map.Map k a -> [Edge (Dual k)]
edgesGen p n m = filter (uncurry p' . ends . dualE) es
  where
    (outer, inner) = edgesM m
    es = map unorient outer ++ inner
    p' c d = p'' (Map.lookup c m)
                 (Map.lookup d m)
    p'' (Just e) (Just f) = p e f
    p'' (Just e) Nothing  = n e
    p'' Nothing (Just e)  = n e
    p'' _        _        = False

-- | The inner edges of a grid that separate unequal cells.
borders :: Eq a => Grid C a -> [Edge N]
borders = edgesGen (/=) (const False)

corners :: C -> [N]
corners c = map (.+^ (c .-. C 0 0)) [N 0 0, N 1 0, N 0 1, N 1 1]

-- | A grid of empty nodes with all nodes of the cells of the
-- first grid.
nodeGrid :: Grid C a -> Grid N ()
nodeGrid = Map.unions . map cornersM . Map.keys
  where
    cornersM = Map.fromList . map (flip (,) ()) . corners

cellGrid :: Grid N a -> Grid C ()
cellGrid m = Map.fromList
           . map (flip (,) ())
           . filter (all (`Map.member` m) . corners)
           . map cellUpRight
           . Map.keys
           $ m
  where
    cellUpRight :: N -> C
    cellUpRight = fromCoord . toCoord

-- | Clues along the outside of a square grid.
-- Ordered such that coordinates increase.
data OutsideClues k a = OC { left :: [a], right :: [a], bottom :: [a], top :: [a] }
    deriving (Show, Eq)

instance Functor (OutsideClues k) where
    fmap f (OC l r b t) = OC (fmap f l) (fmap f r) (fmap f b) (fmap f t)

outsideSize :: OutsideClues k a -> Size
outsideSize (OC l r b t) = (w, h)
  where
    w = max (length t) (length b)
    h = max (length l) (length r)

-- | Create a dummy grid matching the given outside clues in size.
outsideGrid :: (Ord k, FromCoord k) => OutsideClues k a -> Grid k ()
outsideGrid = sizeGrid . outsideSize

-- | Create a dummy grid of the given size.
sizeGrid :: (Ord k, FromCoord k) => Size -> Grid k ()
sizeGrid (w, h) = Map.mapKeys fromCoord
                . Map.fromList
                $ [ ((x, y), ()) | x <- [0..w-1], y <- [0..h-1] ]

data OClue = OClue
    { ocBase :: (Int, Int)
    , ocDir  :: (Int, Int)
    }
  deriving (Show, Eq, Ord)

oClues :: OutsideClues k a -> Map.Map OClue a
oClues ocs@(OC l r b t) = Map.fromList . concat $
    [ zipWith (\y c -> (OClue (-1, y) (-1, 0), c)) [0..h-1] l
    , zipWith (\y c -> (OClue ( w, y) ( 1, 0), c)) [0..h-1] r
    , zipWith (\x c -> (OClue ( x,-1) ( 0,-1), c)) [0..w-1] b
    , zipWith (\x c -> (OClue ( x, h) ( 0, 1), c)) [0..w-1] t
    ]
  where
    (w, h) = outsideSize ocs

outsideClues :: (Ord k, FromCoord k) => OutsideClues k a -> Map.Map k a
outsideClues = Map.mapKeys (fromCoord . ocBase) . oClues

multiOutsideClues :: (Ord k, FromCoord k) => OutsideClues k [a] -> Map.Map k a
multiOutsideClues = Map.mapKeys fromCoord
                  . Map.fromList . concatMap distrib . Map.toList
                  . oClues
  where
    distrib (OClue o d, xs) = zip [o ^+^ i *^ d | i <- [0..]] xs

size :: Grid Coord a -> Size
size m = foldr (both max) (0, 0) (Map.keys m) ^+^ (1, 1)
  where
    both f (x, y) (x', y') = (f x x', f y y')
