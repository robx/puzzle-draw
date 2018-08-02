{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Puzzle grids.
module Data.Grid
    (
      Grid
    , AreaGrid
    , ShadedGrid
    , nodes
    , size
    , sizeGrid
    , clues
    , nodeGrid
    , cellGrid

    , dominoGrid
    , litsGrid
    , litsoGrid
    , pentominoGrid

    , borders
    , skeletons
    , edgesGen
    , colour
    , collectLines
    , rows

    , Sides(..)
    , OutsideClues(..)
    , outsideSize
    , outsideClues
    , multiOutsideClues
    , outsideGrid
    , outsideValues
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.AffineSpace
import Data.VectorSpace
import Control.Monad.State
import Data.Foldable (fold)

import Data.Elements
import Data.GridShape

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

nodes :: Grid N a -> Set.Set N
nodes = Map.keysSet

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

-- | Colour a graph.
colourM :: (Ord k, Eq a) => (k -> [k]) -> Map.Map k a -> Map.Map k Int
colourM nbrs m = fmap fromRight . execState colour' $ start
  where
    fromRight (Right r) = r
    fromRight (Left _)  = error "expected Right"

    start = fmap (const $ Left [1..]) m
    colour' = mapM_ pickAndFill (Map.keys m)

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
            Left _     -> if m Map.! x == a
                    then do modify (Map.insert x (Right c))
                            mapM_ (fill a c) (nbrs x)
                    else modify (del x c)
            Right _    -> return ()

    -- remove the given colour from the list of candidates
    del x c = Map.adjust f x
      where
        f (Left cs) = Left $ filter (/= c) cs
        f (Right c') = Right c'

colour :: Eq a => Grid C a -> Grid C Int
colour m = colourM edgeNeighbours' m
  where
    edgeNeighbours' p = [ q | q <- edgeNeighbours p
                            , q `Map.member` m ]

data Sides a = Sides
  { _sideLeft :: a
  , _sideRight :: a
  , _sideBottom :: a
  , _sideTop :: a
  }

instance Functor Sides where
    fmap f (Sides l r b t) = Sides (f l) (f r) (f b) (f t)

instance Foldable Sides where
    foldMap f (Sides l r b t) = f l <> f r <> f b <> f t

-- | Clues along the outside of a square grid.
-- Ordered such that coordinates increase.
data OutsideClues k a = OC { left :: [a], right :: [a], bottom :: [a], top :: [a] }
    deriving (Show, Eq)

outsideValues :: OutsideClues k a -> [a]
outsideValues (OC l r b t) = l ++ r ++ b ++ t

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
    , _ocDir :: (Int, Int)
    }
  deriving (Show, Eq, Ord)

oClues :: OutsideClues k a -> Sides (Map.Map OClue a)
oClues ocs@(OC l r b t) = fmap Map.fromList $ Sides
    (zipWith (\y c -> (OClue (-1, y) (-1, 0), c)) [0..h-1] l)
    (zipWith (\y c -> (OClue ( w, y) ( 1, 0), c)) [0..h-1] r)
    (zipWith (\x c -> (OClue ( x,-1) ( 0,-1), c)) [0..w-1] b)
    (zipWith (\x c -> (OClue ( x, h) ( 0, 1), c)) [0..w-1] t)
  where
    (w, h) = outsideSize ocs

outsideClues :: (Ord k, FromCoord k) => OutsideClues k a -> Map.Map k a
outsideClues = fold . fmap (Map.mapKeys (fromCoord . ocBase)) . oClues

multiOutsideClues :: (Ord k, FromCoord k) => OutsideClues k [a] -> Sides (Map.Map k a)
multiOutsideClues = fmap (Map.mapKeys fromCoord . Map.fromList . concatMap distrib . Map.toList)
                  . oClues
  where
    distrib (OClue o d, xs) = zip [o ^+^ i *^ d | i <- [0..]] xs

dualEdgesP :: Key k
           => (a -> a -> Bool) -> Grid k a -> [Edge k]
dualEdgesP p m = concatMap f (Map.keys m)
  where
    f c = [ edge c d | d <- map (c .+^) [(0,1), (1,0)]
                     , d `Map.member` m && p (m Map.! c) (m Map.! d) ]

collectLines :: (Key k, Eq a) => Grid k (Maybe a) -> [Edge k]
collectLines = dualEdgesP eq
  where
    eq (Just x) (Just y) = x == y
    eq _        _        = False

-- | The skeletons of connected equal cells.
skeletons :: Eq a => Grid C a -> [Edge C]
skeletons = map dualE . edgesGen (==) (const False)

dominoGrid :: DigitRange -> Grid C (Int, Int)
dominoGrid (DigitRange x y) =
    Map.mapKeys fromCoord . Map.fromList $
        [ ((a, s - b), (b + x, a + x))
        | a <- [0..s], b <- [0..s], b <= a ]
  where
    s = y - x

size :: Grid Coord a -> Size
size m = foldr (both max) (0, 0) (Map.keys m) ^+^ (1, 1)
  where
    both f (x, y) (x', y') = (f x x', f y y')

polyominoGrid :: [ ((Int, Int), Char) ] -> [ (Int,Int) ] -> Grid C (Maybe Char)
polyominoGrid ls ps = Map.mapKeys fromCoord . Map.fromList $
    [ (a, Just c) | (a, c) <- ls ] ++
    [ (a, Nothing) | a <- ps ]

{-
A grid with (Just p) for the capital letters,
Nothing for the lowercase letters.

    Vvv..N.L..X...Ff.Zz..W
    v...nn.l.xxx.ff..z..ww
    v...n..l..x...f.zz.ww.
    ..T.n.ll..............
    ttt.......U.u..Y...Pp.
    ..t.Iiiii.uuu.yyyy.ppp
-}
pentominoGrid :: Grid C (Maybe Char)
pentominoGrid = polyominoGrid ls ps
  where
    ls = [ ((0,5), 'V')
         , ((5,5), 'N')
         , ((7,5), 'L')
         , ((10,5), 'X')
         , ((14,5), 'F')
         , ((17,5), 'Z')
         , ((21,5), 'W')
         , ((2,2), 'T')
         , ((4,0), 'I')
         , ((10,1), 'U')
         , ((15,1), 'Y')
         , ((19,1), 'P')
         ]
    ps = [ (1,5), (2,5), (0,4), (0,3)
         , (0,1), (1,1), (2,1), (2,0)
         , (5,0), (6,0), (7,0), (8,0)
         , (10,0), (11,0), (12,0), (12,1)
         , (14,0), (15,0), (16,0), (17,0)
         , (19,0), (20,0), (20,1), (21,0)
         , (4,2), (4,3), (4,4), (5,4)
         , (6,2), (7,2), (7,3), (7,4)
         , (9,4), (10,4), (11,4), (10,3)
         , (13,4), (14,4), (14,3), (15,5)
         , (18,5), (17,4), (16,3), (17,3)
         , (19,3), (20,3), (20,4), (21,4)
         ]


{-
A grid with (Just p) for the capital letters,
Nothing for the lowercase letters.

    L  I
    l  i Ttt  Ss Oo
    ll i  t  ss  oo
       i
-}
litsoGrid :: Grid C (Maybe Char)
litsoGrid = polyominoGrid ls ps
  where
    ls = [ ((0,3), 'L')
         , ((3,3), 'I')
         , ((5,2), 'T')
         , ((10,2), 'S')
         , ((13,2), 'O')
         ]
    ps = [ (0,1), (0,2), (1,1)
         , (3,0), (3,1), (3,2)
         , (6,2), (6,1), (7,2)
         , (9,1), (10,1), (11,2)
         , (13,1), (14,1), (14,2)
         ]

{-
A grid with (Just p) for the capital letters,
Nothing for the lowercase letters.

    L  I
    l  i Ttt  Ss
    ll i  t  ss
       i
-}
litsGrid :: Grid C (Maybe Char)
litsGrid = polyominoGrid ls ps
  where
    ls = [ ((0,3), 'L')
         , ((3,3), 'I')
         , ((5,2), 'T')
         , ((10,2), 'S')
         ]
    ps = [ (0,1), (0,2), (1,1)
         , (3,0), (3,1), (3,2)
         , (6,2), (6,1), (7,2)
         , (9,1), (10,1), (11,2)
         ]
