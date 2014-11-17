{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts,
             GADTs, StandaloneDeriving #-}

-- | Puzzle grids.
module Data.Puzzles.Grid where

import Data.Maybe
import qualified Data.Map as Map
import Data.Foldable (Foldable, foldMap)
import Data.Traversable (Traversable, traverse)
import Control.Applicative ((<$>))
import Data.VectorSpace

import Data.Puzzles.GridShape
import Data.Puzzles.Elements

-- | A generic grid, with the given shape and contents.
data Grid s a where
    Grid :: { shape :: s
            , contents :: Map.Map (Cell s) a} -> Grid s a

deriving instance (Show a, Show s, GridShape s) => Show (Grid s a)
deriving instance (Eq s, Eq (Cell s), Eq a) => Eq (Grid s a)

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
    foldMap f (Grid _ m) = foldMap f m

instance Traversable (Grid s) where
    traverse f (Grid s m) = Grid s <$> traverse f m

filterG :: (a -> Bool) -> Grid s a -> Grid s a
filterG p (Grid s m) = Grid s (Map.filter p m)

-- | Initialize a square grid from a list of lists. The grid
--   might be incomplete if some rows are shorter.
fromListList :: [[a]] -> Grid Square a
fromListList g = Grid Square m
  where
    h = length g
    m = Map.fromList . concat
      . zipWith (\y -> zipWith (\x -> (,) (x, y)) [0..]) [h-1,h-2..]
      $ g

cells :: GridShape s => Grid s a -> [Cell s]
cells = Map.keys . contents

-- | For a grid with value type @Maybe a@, return an association
--   list of cells and @Just@ values.
clues :: GridShape s => Grid s (Maybe a) -> [(Cell s, a)]
clues g = [ (k, v) | (k, Just v) <- values g ]

-- | Association list of cells and values.
values :: GridShape s => Grid s a -> [(Cell s, a)]
values (Grid _ m) = Map.toList m

edgesGen :: (a -> a -> Bool) -> (a -> Bool) -> Grid Square a -> [Edge]
edgesGen p n g = map (uncurry unorientedEdge) . filter (uncurry p') $ es
  where
    es = uncurry (++) $ edgesPairM (contents g)
    p' c d = p'' (Map.lookup c (contents g))
                 (Map.lookup d (contents g))
    p'' (Just e) (Just f) = p e f
    p'' (Just e) Nothing  = n e
    p'' Nothing (Just e)  = n e
    p'' _        _        = False

edgesP :: (a -> a -> Bool) -> Grid Square a -> [Edge]
edgesP p g = edgesGen p (const False) g

dualEdgesP :: (a -> a -> Bool) -> Grid Square a -> [Edge]
dualEdgesP p g = map (uncurry dualEdge) . filter (uncurry p') $ es
  where
    es = uncurry (++) $ edgesPairM (contents g)
    p' c d = p'' (Map.lookup c (contents g))
                 (Map.lookup d (contents g))
    p'' (Just e) (Just f) = p e f
    p'' _        _        = False

-- | The inner edges of a grid that separate unequal cells.
borders :: Eq a => Grid Square a -> [Edge]
borders = edgesP (/=)

-- | Clues along the outside of a square grid.
data OutsideClues a = OC { left :: [a], right :: [a], bottom :: [a], top :: [a] }
    deriving (Show, Eq)

instance Functor OutsideClues where
    fmap f (OC l r b t) = OC (fmap f l) (fmap f r) (fmap f b) (fmap f t)

outsideSize :: OutsideClues a -> Size
outsideSize (OC l r b t) = (w, h)
  where
    w = max (length t) (length b)
    h = max (length l) (length r)

-- | Create a dummy grid matching the given outside clues in size.
outsideGrid :: OutsideClues a -> SGrid ()
outsideGrid = sizeGrid . outsideSize

-- | Create a dummy grid of the given size.
sizeGrid :: Size -> SGrid ()
sizeGrid (w, h) =
    Grid Square $ Map.fromList [ ((x, y), ()) | x <- [0..w-1], y <- [0..h-1] ]

data OutsideClue a = OClue
    { ocBase  :: (Int, Int)
    , ocDir   :: (Int, Int)
    , ocValue :: a
    }

instance Functor OutsideClue where
    fmap f (OClue b d x) = OClue b d (f x)

outsideClueList :: OutsideClues a -> [OutsideClue a]
outsideClueList o@(OC l r b t) =
    concat
       [ zipWith (\ y c -> OClue (0,y)   (-1, 0) c) [0..h-1] l
       , zipWith (\ y c -> OClue (w-1,y) ( 1, 0) c) [0..h-1] r
       , zipWith (\ x c -> OClue (x,0)   ( 0,-1) c) [0..w-1] b
       , zipWith (\ x c -> OClue (x,h-1) ( 0, 1) c) [0..w-1] t
       ]
  where
    (w, h) = outsideSize o

-- | Convert outside clues to association list mapping coordinate to value.
outsideClues :: OutsideClues (Maybe a) -> [((Int, Int), a)]
outsideClues = mapMaybe (liftMaybe . toCell) . outsideClueList
  where
    toCell (OClue (bx, by) (dx, dy) v) = ((bx + dx, by + dy), v)
    liftMaybe (p, Just x)  = Just (p, x)
    liftMaybe (_, Nothing) = Nothing

multiOutsideClues :: OutsideClues [a] -> [((Int, Int), a)]
multiOutsideClues = concatMap distrib . outsideClues . fmap Just . dired
  where
    dired (OC l r b t) = OC (z (-1,0) l) (z (1,0) r) (z (0,-1) b) (z (0,1) t)
    z x ys = zip (repeat x) ys
    distrib (o, (d, xs)) = zip [o ^+^ i *^ d | i <- [0..]] xs

size :: Grid Square a -> Size
size (Grid _ m) = foldr (both max) (0, 0) (Map.keys m) ^+^ (1, 1)
  where
    both f (x, y) (x', y') = (f x x', f y y')
