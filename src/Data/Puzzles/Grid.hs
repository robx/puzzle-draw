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
import Control.Monad.State

import Data.Puzzles.GridShape hiding (size, cells)
import qualified Data.Puzzles.GridShape as GS
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
cells = Map.keys . contents

inBounds :: (GridShape s, Eq (Cell s)) => Grid s a -> Cell s -> Bool
inBounds g c = c `elem` cells g

-- | For a grid with value type @Maybe a@, return an association
--   list of cells and @Just@ values.
clues :: GridShape s => Grid s (Maybe a) -> [(Cell s, a)]
clues g = [ (k, v) | (k, Just v) <- values g ]

-- | Association list of cells and values.
values :: GridShape s => Grid s a -> [(Cell s, a)]
values (Grid _ m) = Map.toList m

edgesGen :: (a -> a -> Bool) -> (a -> Bool) -> Grid Square a -> [Edge]
edgesGen p n g = [ E pt V | pt <- vedges ] ++ [ E pt H | pt <- hedges ]
  where
    edges' f (sx, sy) = [ (x + 1, y) | x <- [-1 .. sx - 1]
                                     , y <- [-1 .. sy]
                                     , p' (f (x, y)) (f (x + 1, y)) ]

    vedges = edges' id (size g)
    hedges = map swap $ edges' swap (swap . size $ g)
    swap (x, y) = (y, x)
    p' c d = p'' (Map.lookup c (contents g))
                 (Map.lookup d (contents g))
    p'' (Just e) (Just f) = p e f
    p'' (Just e) Nothing  = n e
    p'' Nothing (Just e)  = n e
    p'' _        _        = False

edgesP :: (a -> a -> Bool) -> Grid Square a -> [Edge]
edgesP p g = edgesGen p (const False) g

dualEdgesP :: (a -> a -> Bool) -> Grid Square a -> [Edge]
dualEdgesP p g = [ E pt H | pt <- hedges ] ++
                 [ E pt V | pt <- vedges ]
  where
    edges' f (sx, sy) = [ (x, y) | x <- [0 .. sx - 2]
                                 , y <- [0 .. sy - 1]
                                 , p' (f (x, y)) (f (x + 1, y)) ]

    hedges = edges' id (size g)
    vedges = map swap $ edges' swap (swap . size $ g)
    swap (x, y) = (y, x)
    p' c d = p'' (Map.lookup c (contents g))
                 (Map.lookup d (contents g))
    p'' (Just e) (Just f) = p e f
    p'' _        _        = False

-- | The inner edges of a grid that separate unequal cells.
borders :: Eq a => Grid Square a -> [Edge]
borders = edgesP (/=)

-- | Colour a graph.
colourM :: (Ord k, Eq a) => (k -> [k]) -> Map.Map k a -> Map.Map k Int
colourM nbrs m = fmap fromRight . snd . runState colour' $ start
  where
    fromRight (Right r) = r
    fromRight (Left _)  = error "expected Right"

    start = fmap (const $ Left [1..]) m
    colour' = sequence_ (map pickAndFill (Map.keys m))

    -- choose a colour for the given node, and spread it to
    -- equal neighbours, removing it from unequal neighbours
    pickAndFill x = do
        v <- (Map.! x) <$> get
        case v of
            Left (c:_) -> fill (m Map.! x) c x
            Left _     -> error "empty set of candidates"
            Right _    -> return ()

    fill a c x = do
        v <- (Map.! x) <$> get
        case v of
            Left _     -> do
                if m Map.! x == a
                    then do modify (Map.insert x (Right c))
                            sequence_ . map (fill a c) $ nbrs x
                    else modify (del x c)
            Right _    -> return ()

    -- remove the given colour from the list of candidates
    del x c = Map.adjust f x
      where
        f (Left cs) = Left $ filter (/= c) cs
        f (Right c') = Right c'

colour :: Eq a => SGrid a -> SGrid Int
colour (Grid s m) = Grid s $ colourM (edgeNeighbours s) m

-- | Clues along the outside of a square grid.
data OutsideClues a = OC { left :: [a], right :: [a], bottom :: [a], top :: [a] }
    deriving (Show, Eq)

instance Functor OutsideClues where
    fmap f (OC l r b t) = OC (fmap f l) (fmap f r) (fmap f b) (fmap f t)

outsideSize :: OutsideClues a -> (Int, Int)
outsideSize (OC l r b t) = ( max (length t) (length b)
                           , max (length l) (length r)
                           )

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

collectLines :: Eq a => SGrid (Maybe a) -> [Edge]
collectLines = dualEdgesP eq
  where
    eq (Just x) (Just y) = x == y
    eq _        _        = False
