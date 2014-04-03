{-# LANGUAGE FlexibleContexts, GADTs, StandaloneDeriving #-}

module Puzzles.Data.Grid where

import Data.Maybe
import qualified Data.Map as Map
import Data.Foldable (Foldable, fold)
import Data.Traversable (Traversable, traverse)
import Control.Applicative ((<$>))

import Puzzles.Data.GridShape hiding (size, cells)
import qualified Puzzles.Data.GridShape as GS
import Puzzles.Data.Elements

data Grid s a where
    Grid :: { shape :: s
            , contents :: Map.Map (Cell s) a} -> Grid s a

deriving instance (Show a, Show s, GridShape s) => Show (Grid s a)

type SGrid = Grid Square

type CharGrid = SGrid Char
type AreaGrid = CharGrid
type ShadedGrid = SGrid Bool
type CharClueGrid = SGrid (Maybe Char)
type IntGrid = SGrid (Clue Int)

(!) :: (GridShape s, Ord (Cell s)) => Grid s a -> Cell s -> a
(!) (Grid _ m) = (m Map.!)

instance Functor (Grid s) where
    fmap f (Grid s m) = Grid s (fmap f m)

instance Foldable (Grid s) where
    fold (Grid _ m) = fold m

instance Traversable (Grid s) where
    traverse f (Grid s m) = Grid s <$> (traverse f m)

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

clues :: GridShape s => Grid s (Maybe a) -> [(Cell s, a)]
clues g = [ (k, v) | (k, Just v) <- values g ]

values :: GridShape s => Grid s a -> [(Cell s, a)]
values (Grid _ m) = Map.toList m


neighbours :: Grid Square a -> Cell Square -> [Cell Square]
neighbours g p = filter (inBounds g) . map (add p) $ deltas
    where deltas = [ (dx, dy)
                   | dx <- [-1..1], dy <- [-1..1]
                   , dx /= 0 || dy /= 0
                   ]
          add (x, y) (x', y') = (x + x', y + y')

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
    deriving Show

outsideclues :: OutsideClues (Maybe a) -> [((Int, Int), a)]
outsideclues (OC l r b t) = mapMaybe liftMaybe . concat $
                             [ zipWith (\ y c -> ((-1, y), c)) [0..h-1] l
                             , zipWith (\ y c -> (( w, y), c)) [0..h-1] r
                             , zipWith (\ x c -> (( x,-1), c)) [0..w-1] b
                             , zipWith (\ x c -> (( x, h), c)) [0..w-1] t
                             ]
  where
    w = length b
    h = length l
    liftMaybe (p, Just x)  = Just (p, x)
    liftMaybe (_, Nothing) = Nothing
