{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Draw.Grid where

import Data.Maybe (catMaybes)
import Data.Char (isUpper)
import qualified Data.Map.Strict as Map

import Diagrams.Prelude hiding (size, E, N, dot, outer)
import Diagrams.TwoD.Offset (offsetPath)

import qualified Data.AffineSpace as AS

import Data.Util
import Data.Grid
import Data.GridShape hiding (edge)

import Draw.Style
import Draw.Lib
import Draw.Widths

(.--.) :: AS.AffineSpace p => p -> p -> AS.Diff p
(.--.) = (AS..-.)

class ToPoint a where
    toPoint :: a -> P2 Double

instance ToPoint C where
    toPoint c = p2 (1/2, 1/2) .+^ r2i (c .--. C 0 0)

instance ToPoint N where
    toPoint c = origin .+^ r2i (c .--. N 0 0)

-- | Draw a small black dot with no envelope.
dot :: Backend' b => Diagram b
dot = circle 0.05 # fc black # smash

grid :: Backend' b
     => GridStyle -> Grid C a -> Diagram b
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

outLine :: Backend' b => Double -> Path V2 Double -> Diagram b
outLine f p = lwG 0 . stroke $ pin <> pout
  where
    pout = reversePath $ offsetPath (f * onepix - e) p
    pin = offsetPath (-e) p
    e = onepix / 2

bgdashingG :: (Semigroup a, HasStyle a, InSpace V2 Double a) =>
             [Double] -> Double -> AlphaColour Double -> a -> a
bgdashingG ds offs c x = x # dashingG ds offs <> x # lcA c

dashes :: [Double]
dashes = [5 / 40, 3 / 40]

dashoffset :: Double
dashoffset = 2.5 / 40

gridDashing :: (Semigroup a, HasStyle a, InSpace V2 Double a) => a -> a
gridDashing = bgdashingG dashes dashoffset white'
  where
    white' = black `withOpacity` (0.05 :: Double)

-- | `irregularGridPaths g` is a pair `(outer, inner)` of paths.
--
-- `outer` consists of the loops that make up the border of the
-- grid (assuming the grid is connected orthogonally). They are
-- reoriented to be compatible with `outLine`; for some reason,
-- reversePath on the immediate result does not work.
--
-- `inner` consists of the individual inner segments.
irregularGridPaths :: Grid C a -> (Path V2 Double, Path V2 Double)
irregularGridPaths m = (path' (map revEdge outer), path inner)
  where
    (outer, inner) = edges (Map.keysSet m) (`Map.member` m)
    path  es = mconcat . map (conn . ends) $ es
    path' es = case loops (map ends' es) of
        Just ls   -> mconcat . map (pathFromLoopVertices . map toPoint) $ ls
        Nothing   -> mempty
    pathFromLoopVertices = pathFromLocTrail
                         . mapLoc (wrapLoop . closeLine)
                         . fromVertices
    conn (v, w) = toPoint v ~~ toPoint w

offsetBorder :: Double -> [C] -> Path V2 Double
offsetBorder off cs =
    pathFromLoopVertices . map offsetCorner . corners . map toPoint $ loop
  where
    pathFromLoopVertices = pathFromLocTrail
                         . mapLoc (wrapLoop . closeLine)
                         . fromVertices
    outer :: [Edge' N]
    (outer, _) = edges cs (`elem` cs)
    loop :: [N]
    loop = case loops (map ends' outer) of
        Just [l] -> tail l
        _        -> error "broken cage"
    corners :: [P2 Double] -> [(P2 Double, P2 Double, P2 Double)]
    corners vs = catMaybes $ zipWith3
        (\ a b c -> if b .-. a == c .-. b then Nothing else Just (a, b, c))
        vs (tail vs ++ vs) (tail (tail vs) ++ vs)
    offsetCorner :: (P2 Double, P2 Double, P2 Double) -> P2 Double
    offsetCorner (a, b, c) =
      let
        dir = perp (normalize (b .-. a)) ^+^ perp (normalize (c .-. b))
      in
        b .+^ (off *^ dir)

onGrid :: (Transformable a, Monoid a, InSpace V2 Double a) =>
          Double -> Double -> (t -> a) -> [(Coord, t)] -> a
onGrid dx dy f = mconcat . map g
  where
    g (p, c) = f c # translate (r2coord p)
    r2coord (x, y) = r2 (dx * fromIntegral x, dy * fromIntegral y)

placeGrid :: (ToPoint k, HasOrigin a, Monoid a, InSpace V2 Double a)
          => Grid k a -> a
placeGrid = Map.foldMapWithKey (moveTo . toPoint)

placeGrid' :: (HasOrigin a, Monoid a, InSpace V2 Double a)
          => Grid (P2 Double) a -> a
placeGrid' = Map.foldMapWithKey moveTo

edge :: (ToPoint k) => Edge k -> Path V2 Double
edge (E c d) = rule d # translate (toPoint c .-. origin)
  where
    rule Vert = vrule 1.0 # alignB
    rule Horiz = hrule 1.0 # alignL

midPoint :: (AS.AffineSpace k, AS.Diff k ~ (Int, Int), ToPoint k) => Edge k -> P2 Double
midPoint e = c .+^ 0.5 *^ (d .-. c)
  where
    (a, b) = ends e
    c = toPoint a
    d = toPoint b

edgeStyle :: (HasStyle a, InSpace V2 Double a) => a -> a
edgeStyle = lineCap LineCapSquare . lwG edgewidth

thinEdgeStyle :: (HasStyle a, InSpace V2 Double a) => a -> a
thinEdgeStyle = lineCap LineCapSquare . lwG onepix

drawEdges :: (ToPoint k, Backend' b) => [Edge k] -> Diagram b
drawEdges = edgeStyle . stroke . mconcat . map edge

drawThinEdges :: (ToPoint k, Backend' b) => [Edge k] -> Diagram b
drawThinEdges = thinEdgeStyle . stroke . mconcat . map edge

drawAreas :: (Backend' b, Eq a) =>
             Grid C a -> Diagram b
drawAreas = drawEdges . borders

cage :: Backend' b => [C] -> Diagram b
cage cs = dashingG dashes dashoffset
        . lwG onepix . stroke . offsetBorder (-4 * onepix) $ cs

fillBG :: Backend' b => Colour Double -> Diagram b
fillBG c = square 1 # lwG onepix # fc c # lc c

shadeGrid :: Backend' b =>
             Grid C (Maybe (Colour Double)) -> Diagram b
shadeGrid = placeGrid . fmap fillBG . clues

drawShade :: Backend' b =>
             Grid C Bool -> Diagram b
drawShade = shadeGrid . fmap f
  where
    f True  = Just gray
    f False = Nothing

drawAreasGray :: Backend' b =>
                 Grid C Char -> Diagram b
drawAreasGray = drawAreas <> shadeGrid . fmap cols
  where
    cols c | isUpper c  = Just (blend 0.1 black white)
           | otherwise  = Nothing

-- Place a list of diagrams along a ray, with steps of size
-- @f@.
distrib :: (Transformable c, Monoid c, InSpace V2 Double c) =>
           V2 Double -> (Int, Int) -> Double -> [c] -> c
distrib base dir f xs =
    translate (0.75 *^ dir' ^+^ base) . mconcat $
        zipWith (\i d -> translate (fromIntegral i *^ dir') d) [(0 :: Int)..] xs
  where
    dir' = f *^ r2i dir
