{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts,
             GADTs, StandaloneDeriving #-}

-- | Puzzle grids.
module Data.Puzzles.Grid where

import Data.Maybe
import qualified Data.Map as Map
import Data.Foldable (Foldable, fold)
import Data.Traversable (Traversable, traverse)
import Control.Applicative ((<$>))
import Data.VectorSpace

import Data.Puzzles.GridShape hiding (size, cells)
import qualified Data.Puzzles.GridShape as GS
import Data.Puzzles.Elements

-- | A generic grid, with the given shape and contents.
data Grid s a where
    Grid :: { shape :: s
            , contents :: Map.Map (Cell s) a} -> Grid s a

deriving instance (Show a, Show s, GridShape s) => Show (Grid s a)

-- | Standard square grid.
type SGrid = Grid Square

type CharGrid = SGrid Char
type AreaGrid = CharGrid
type ShadedGrid = SGrid Bool
type CharClueGrid = SGrid (Maybe Char)
type IntGrid = SGrid (Clue Int)

-- | Lookup a grid value at a given cell. Unsafe.
(!) :: (GridShape s, Ord (Cell s)) => Grid s a -> Cell s -> a
(!) (Grid _ m) = (m Map.!)

instance Functor (Grid s) where
    fmap f (Grid s m) = Grid s (fmap f m)

instance Foldable (Grid s) where
    fold (Grid _ m) = fold m

instance Traversable (Grid s) where
    traverse f (Grid s m) = Grid s <$> (traverse f m)

filterG :: (a -> Bool) -> (Grid s a) -> (Grid s a)
filterG p (Grid s m) = Grid s (Map.filter p m)

-- | Initialize a square grid from a list of lists. The grid
--   might be incomplete if some rows are shorter.
fromListList :: [[a]] -> Grid Square a
fromListList g = Grid s m
  where
    w = maximum . map length $ g
    h = length g
    s = Square w h
    m = Map.fromList . concat
      . zipWith (\y -> zipWith (\x -> (,) (x, y)) [0..]) [h-1,h-2..]
      $ g

size :: GridShape s => Grid s a -> GridSize s
size = GS.size . shape

cells :: GridShape s => Grid s a -> [Cell s]
cells = GS.cells . shape

inBounds :: (GridShape s, Eq (Cell s)) => Grid s a -> Cell s -> Bool
inBounds g c = c `elem` cells g

-- | For a grid with value type @Maybe a@, return an association
--   list of cells and @Just@ values.
clues :: GridShape s => Grid s (Maybe a) -> [(Cell s, a)]
clues g = [ (k, v) | (k, Just v) <- values g ]

-- | Association list of cells and values.
values :: GridShape s => Grid s a -> [(Cell s, a)]
values (Grid _ m) = Map.toList m

-- | The inner edges of a grid that separate unequal cells.
borders :: Eq a => Grid Square a -> [Edge]
borders g = [ E p V | p <- vborders ] ++ [ E p H | p <- hborders ]
  where
    borders' f (sx, sy) = [ (x + 1, y) | x <- [0 .. sx - 2]
                                       , y <- [0 .. sy - 1]
                                       , f (x, y) /= f (x + 1, y) ]
    vborders = borders' (g !) (size g)
    hborders = map swap $ borders' ((g !) . swap) (swap . size $ g)
    swap (x, y) = (y, x)

-- | Clues along the outside of a square grid.
data OutsideClues a = OC { left :: [a], right :: [a], bottom :: [a], top :: [a] }
    deriving (Show, Eq)

instance Functor OutsideClues where
    fmap f (OC l r b t) = OC (fmap f l) (fmap f r) (fmap f b) (fmap f t)

outsideSize :: OutsideClues a -> (Int, Int)
outsideSize (OC l _ _ t) = (length t, length l)

-- | Convert outside clues to association list mapping coordinate to value.
outsideClues :: OutsideClues (Maybe a) -> [((Int, Int), a)]
outsideClues o@(OC l r b t) = mapMaybe liftMaybe . concat $
                             [ zipWith (\ y c -> ((-1, y), c)) [0..h-1] l
                             , zipWith (\ y c -> (( w, y), c)) [0..h-1] r
                             , zipWith (\ x c -> (( x,-1), c)) [0..w-1] b
                             , zipWith (\ x c -> (( x, h), c)) [0..w-1] t
                             ]
  where
    (w, h) = outsideSize o
    liftMaybe (p, Just x)  = Just (p, x)
    liftMaybe (_, Nothing) = Nothing

multiOutsideClues :: OutsideClues [a] -> [((Int, Int), a)]
multiOutsideClues = concatMap distrib . outsideClues . fmap Just . dired
  where
    dired (OC l r b t) = OC (z (-1,0) l) (z (1,0) r) (z (0,-1) b) (z (0,1) t)
    z x ys = zip (repeat x) ys
    distrib (o, (d, xs)) = zip [o ^+^ i *^ d | i <- [0..]] xs
