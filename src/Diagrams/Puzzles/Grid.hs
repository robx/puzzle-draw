{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Diagrams.Puzzles.Grid where

import Data.Char (isUpper)
import qualified Data.Map as M
import qualified Data.Set as S

import Diagrams.Prelude
import Diagrams.TwoD.Offset

import Data.Puzzles.Util
import Data.Puzzles.Grid
import Data.Puzzles.GridShape hiding (edge)

import Diagrams.Puzzles.Style
import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Widths

class ToPoint a where
    toPoint :: a -> P2

instance ToPoint C where
    toPoint c = p2 (1/2, 1/2) .+^ r2i (c .-. C 0 0)

instance ToPoint N where
    toPoint c = origin .+^ r2i (c .-. N 0 0)

-- | Draw a small black dot with no envelope.
dot :: Backend' b => Diagram b R2
dot = circle 0.05 # fc black # smash

grid :: Backend' b
     => GridStyle -> Grid C a -> Diagram b R2
grid s g =
    (placeGrid . fmap (const vertex) . nodeGrid $ g)
    <> stroke inner # linestyle (_line s)
    <> stroke outer # linestyle (_border s)
    <> frm
  where
    vertex = case _vertex s of
        VertexDot    -> dot
        VertexNone   -> mempty
    linestyle LineNone   = const mempty
    linestyle LineThin   = lwG gridwidth
    linestyle LineDashed = gridDashing . lwG gridwidth
    linestyle LineThick  = lwG edgewidth
    frm = case _frame s of
        Just (FrameStyle f c)  -> outLine f outer # fc c
        Nothing                -> mempty

    (outer, inner) = irregularGridPaths g

outLine :: Backend' b => Double -> Path R2 -> Diagram b R2
outLine f p = lwG 0 . stroke $ pin <> pout
  where
    pout = reversePath $ offsetPath (f * onepix - e) p
    pin = offsetPath (-e) p
    e = onepix / 2

bgdashingG :: (Semigroup a, HasStyle a, V a ~ R2) =>
             [Double] -> Double -> AlphaColour Double -> a -> a
bgdashingG ds offs c x = x # dashingG ds offs <> x # lcA c

dashes :: [Double]
dashes = [5 / 40, 3 / 40]

dashoffset :: Double
dashoffset = 2.5 / 40

gridDashing :: (Semigroup a, HasStyle a, V a ~ R2) => a -> a
gridDashing = bgdashingG dashes dashoffset white'
  where
    white' = black `withOpacity` 0.05

-- | `irregularGridPaths g` is a pair `(outer, inner)` of paths.
--
-- `outer` consists of the loops that make up the border of the
-- grid (assuming the grid is connected orthogonally). They are
-- reoriented to be compatible with `outLine`; for some reason,
-- reversePath on the immediate result does not work.
--
-- `inner` consists of the individual inner segments.
irregularGridPaths :: Grid C a -> (Path R2, Path R2)
irregularGridPaths m = (toPath' (map revEdge outer), toPath inner)
  where
    (outer, inner) = edges (M.keysSet m) (`M.member` m)
    toPath  es = mconcat . map (conn . ends) $ es
    toPath' es = case loops (map ends' es) of
        Just ls   -> mconcat . map (pathFromLoopVertices . map toPoint) $ ls
        Nothing   -> mempty
    pathFromLoopVertices = pathFromLocTrail
                         . mapLoc (wrapLoop . closeLine)
                         . fromVertices
    conn (v, w) = toPoint v ~~ toPoint w

irregPathToVertices :: (Path R2, Path R2) -> (S.Set P2, S.Set P2, S.Set P2)
irregPathToVertices (pouter, pinner) = (outer, inner S.\\ outer, inner `S.union` outer)
  where
    outer = S.fromList . mconcat . pathVertices $ pouter
    inner = S.fromList . mconcat . pathVertices $ pinner

onGrid :: (Transformable a, Monoid a, V a ~ R2) =>
          Double -> Double -> (t -> a) -> [(Coord, t)] -> a
onGrid dx dy f = mconcat . map g
  where
    g (p, c) = f c # translate (r2coord p)
    r2coord (x, y) = r2 (dx * fromIntegral x, dy * fromIntegral y)

placeGrid :: (ToPoint k, HasOrigin a, Transformable a, Monoid a, V a ~ R2)
          => Grid k a -> a
placeGrid = M.foldMapWithKey (moveTo . toPoint)

edge :: (ToPoint k) => Edge k -> Path R2
edge (E c d) = rule d # translate (toPoint c .-. origin)
  where
    rule Vert = vrule 1.0 # alignB
    rule Horiz = hrule 1.0 # alignL

edgeStyle :: (HasStyle a, V a ~ R2) => a -> a
edgeStyle = lineCap LineCapSquare . lwG edgewidth

thinEdgeStyle :: (HasStyle a, V a ~ R2) => a -> a
thinEdgeStyle = lineCap LineCapSquare . lwG onepix

drawEdges :: (ToPoint k, Backend' b) => [Edge k] -> Diagram b R2
drawEdges = edgeStyle . stroke . mconcat . map edge

drawThinEdges :: (ToPoint k, Backend' b) => [Edge k] -> Diagram b R2
drawThinEdges = thinEdgeStyle . stroke . mconcat . map edge

drawAreas :: (Backend' b, Eq a) =>
             Grid C a -> Diagram b R2
drawAreas = drawEdges . borders

fillBG :: Backend' b => Colour Double -> Diagram b R2
fillBG c = square 1 # lwG onepix # fc c # lc c

shadeGrid :: Backend' b =>
             Grid C (Maybe (Colour Double)) -> Diagram b R2
shadeGrid = placeGrid . fmap fillBG . clues

drawShade :: Backend' b =>
             Grid C Bool -> Diagram b R2
drawShade = shadeGrid . fmap f
  where
    f True  = Just gray
    f False = Nothing

drawAreasGray :: Backend' b =>
                 Grid C Char -> Diagram b R2
drawAreasGray = drawAreas <> shadeGrid . fmap cols
  where
    cols c | isUpper c  = Just (blend 0.1 black white)
           | otherwise  = Nothing

-- Place a list of diagrams along a ray, with steps of size
-- @f@.
distrib :: (Transformable c, Monoid c, V c ~ R2) =>
           R2 -> (Int, Int) -> Double -> [c] -> c
distrib base dir f xs =
    translate (0.75 *^ dir' ^+^ base) . mconcat $
        zipWith (\i d -> translate (fromIntegral i *^ dir') d) [(0 :: Int)..] xs
  where
    dir' = f *^ r2i dir
