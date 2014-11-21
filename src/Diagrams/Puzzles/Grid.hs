{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Diagrams.Puzzles.Grid where

import Data.Char (isUpper)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Arrow (first)

import Diagrams.Prelude
import Diagrams.TwoD.Offset

import Data.Puzzles.Util
import Data.Puzzles.Grid
import Data.Puzzles.GridShape hiding (dualEdge)

import Diagrams.Puzzles.Style
import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Widths

-- | Draw a small black dot with no envelope.
dot :: Backend' b => Diagram b R2
dot = circle 0.05 # fc black # smash

grid :: Backend' b
     => GridStyle -> SGrid a -> Diagram b R2
grid s g =
    atVerticesP (const vertex) (map (\p -> (p, ())) . S.toList $ vall)
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
    (_, _, vall) = irregPathToVertices (inner, outer)

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
irregularGridPaths :: SGrid a -> (Path R2, Path R2)
irregularGridPaths (Grid _ m) = (toPath' (map rev outer), toPath inner)
  where
    (outer, inner) = edges (M.keysSet m) (`M.member` m)
    toPath  es = mconcat . map (conn . ends) $ es
    toPath' es = case loops (map ends es) of
        Just ls   -> mconcat . map (pathFromLoopVertices  . map p2i) $ ls
        Nothing   -> mempty
    pathFromLoopVertices = pathFromLocTrail
                         . mapLoc (wrapLoop . closeLine)
                         . fromVertices
    conn (v, w) = p2i v ~~ p2i w
    ends (E' v R) = (v, v ^+^ (1,0))
    ends (E' v L) = (v, v ^+^ (-1,0))
    ends (E' v U) = (v, v ^+^ (0,1))
    ends (E' v D) = (v, v ^+^ (0,-1))
    rev (E' v R) = E' (v ^+^ (1,0))  L
    rev (E' v L) = E' (v ^+^ (-1,0)) R
    rev (E' v U) = E' (v ^+^ (0,1))  D
    rev (E' v D) = E' (v ^+^ (0,-1)) U


irregPathToVertices :: (Path R2, Path R2) -> (S.Set P2, S.Set P2, S.Set P2)
irregPathToVertices (pouter, pinner) = (outer, inner S.\\ outer, inner `S.union` outer)
  where
    outer = S.fromList . mconcat . pathVertices $ pouter
    inner = S.fromList . mconcat . pathVertices $ pinner

-- | In a square grid, use the first argument to draw things at the centres
--   of cells given by coordinates.
atCentres :: (Transformable a, Monoid a, V a ~ R2) =>
             (t -> a) -> [(Coord, t)] -> a
atCentres dc = translate (r2 (1/2, 1/2)) . atVertices dc

atCentres' :: (Transformable a, V a ~ R2) => SGrid a -> [a]
atCentres' = translate (r2 (1/2, 1/2)) . atVertices'

-- | In a square grid, use the first argument to draw things
--   at the given points..
atVerticesP :: (Transformable a, Monoid a, V a ~ R2) =>
              (t -> a) -> [(P2, t)] -> a
atVerticesP dc = mconcat . map (\ (p, c) -> dc c # translate (p .-. origin))

-- | In a square grid, use the first argument to draw things
--   at the grid vertices given by coordinates.
atVertices :: (Transformable a, Monoid a, V a ~ R2) =>
              (t -> a) -> [(Coord, t)] -> a
atVertices dc = atVerticesP dc . map (first p2i)

atVertices' :: (Transformable a, V a ~ R2) => SGrid a -> [a]
atVertices' g = [ (g ! c) # translatep c | c <- cells g ]

edge :: Edge -> Path R2
edge (E c d) = rule d # translate (r2i c)
  where
    rule Vert = vrule 1.0 # alignB
    rule Horiz = hrule 1.0 # alignL

dualEdge :: Edge -> Path R2
dualEdge = translate (r2 (1/2, 1/2)) . edge

edgeStyle :: (HasStyle a, V a ~ R2) => a -> a
edgeStyle = lineCap LineCapSquare . lwG edgewidth

thinEdgeStyle :: (HasStyle a, V a ~ R2) => a -> a
thinEdgeStyle = lineCap LineCapSquare . lwG onepix

drawEdges :: Backend' b => [Edge] -> Diagram b R2
drawEdges = edgeStyle . stroke . mconcat . map edge

drawDualEdges :: Backend' b => [Edge] -> Diagram b R2
drawDualEdges = edgeStyle . stroke . mconcat . map dualEdge

drawThinDualEdges :: Backend' b => [Edge] -> Diagram b R2
drawThinDualEdges = thinEdgeStyle . stroke . mconcat . map dualEdge

drawAreas :: (Backend' b, Eq a) =>
                  SGrid a -> Diagram b R2
drawAreas = drawEdges . borders

fillBG :: Backend' b => Colour Double -> Diagram b R2
fillBG c = square 1 # lwG onepix # fc c # lc c

shadeGrid :: Backend' b =>
              SGrid (Maybe (Colour Double)) -> Diagram b R2
shadeGrid = mconcat . atCentres' . fmap (maybe mempty fillBG)

drawShade :: Backend' b =>
                  SGrid Bool -> Diagram b R2
drawShade = shadeGrid . fmap f
  where
    f True  = Just gray
    f False = Nothing

drawAreasGray :: Backend' b =>
                    SGrid Char -> Diagram b R2
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

outsideGen :: (Transformable c, Monoid c, V c ~ R2) =>
              (OutsideClue [c] -> R2) -> Double -> [OutsideClue [c]] -> c
outsideGen tobase f ocs = mconcat . map placeOC $ ocs
  where
    placeOC o = distrib (tobase o) (ocDir o) f (ocValue o)

outsideCells :: (Transformable c, Monoid c, V c ~ R2) =>
                Double -> [OutsideClue [c]] -> c
outsideCells = outsideGen base
  where
    base (OClue (bx, by) (dx, dy) _)
        | dx /= 0   = r2 (fromIntegral bx - 1, fromIntegral by - 1/2)
        | dy /= 0   = r2 (fromIntegral bx - 1/2, fromIntegral by)
        | otherwise = error "invalid outside clue"

outsideVertices :: (Transformable c, Monoid c, V c ~ R2) =>
                   Double -> [OutsideClue [c]] -> c
outsideVertices = outsideGen base
  where
    base (OClue (bx, by) (dx, dy) _)
        | dx /= 0   = r2 (fromIntegral bx, fromIntegral by)
        | dy /= 0   = r2 (fromIntegral bx, fromIntegral by)
        | otherwise = error "invalid outside clue"
