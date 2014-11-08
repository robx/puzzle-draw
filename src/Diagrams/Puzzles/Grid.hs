{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Diagrams.Puzzles.Grid where

import Data.Char (isUpper)
import qualified Data.Map as M

import Diagrams.Prelude hiding (size, E, dot, outer)

import Data.Puzzles.Grid
import Data.Puzzles.GridShape hiding (size, cells)

import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Widths

-- | Draw a small black dot with no envelope.
dot :: Backend' b => Diagram b
dot = circle 0.05 # fc black # smash

-- | Draw a Slither Link style grid of dots of the specified size.
slithergrid :: Backend' b =>
               Size -> Diagram b
slithergrid (x, y) =
    hcatsep . replicate (x + 1) . vcatsep . replicate (y + 1) $ dot

fence :: [Double] -> Double -> Path V2 Double
fence xs h = atPoints [p2 (x, 0) | x <- xs] (repeat v)
  where
    v = alignB (vrule h)

-- | The inner grid lines of a square grid of the specified size.
gridlines :: Size -> Path V2 Double
gridlines (w, h) = fence' w h <> mirror (fence' h w)
  where
    fence' n l = fence (map fromIntegral [1..n-1]) (fromIntegral l)

fullgridlines :: Size -> Path V2 Double
fullgridlines (w, h) = fence' w h <> mirror (fence' h w)
  where
    fence' n l = fence (map fromIntegral [0..n]) (fromIntegral l)

-- | Draw a frame around the outside of a rectangle.
outframe :: Backend' b => Size -> Diagram b
outframe (w, h) = strokePointLoop r # lwG fw
    where wd = fromIntegral w
          hd = fromIntegral h
          strokePointLoop = strokeLocTrail . mapLoc (wrapLoop . closeLine)
                            . fromVertices . map p2
          fw = framewidthfactor * gridwidth
          e = fw / 2 - gridwidth / 2
          r = [(-e, -e), (wd + e, -e), (wd + e, hd + e), (-e, hd + e)]

-- | Draw a square grid, applying the given style to the grid lines.
grid' :: Backend' b =>
         (Diagram b -> Diagram b) -> Size -> Diagram b
grid' gridstyle s =
    outframe s
    <> stroke (gridlines s) # lwG gridwidth # gridstyle

-- | Draw a square grid with default grid line style.
grid :: Backend' b =>
        Size -> Diagram b
grid = grid' id

-- | Draw a square grid with thin frame.
plaingrid :: Backend' b =>
             Size -> Diagram b
plaingrid s = stroke (fullgridlines s) # lwG gridwidth

bgdashingG :: (Semigroup a, HasStyle a, InSpace V2 Double a) =>
             [Double] -> Double -> Colour Double -> a -> a
bgdashingG ds offs c x = x # dashingG ds offs <> x # lc c

dashes :: [Double]
dashes = [5 / 40, 3 / 40]

dashoffset :: Double
dashoffset = 2.5 / 40

gridDashing :: (Semigroup a, HasStyle a, InSpace V2 Double a) => a -> a
gridDashing = bgdashingG dashes dashoffset white'
  where
    white' = blend 0.95 white black

-- | Draw a square grid with dashed grid lines. The gaps
--   between dashes are off-white to aid in using filling
--   tools.
dashedgrid :: Backend' b =>
              Size -> Diagram b
dashedgrid = grid' gridDashing

edgePath :: Edge' (Vertex Square) -> Path V2 Double
edgePath (E' v R) = p2i v ~~ (p2i v .+^ r2i (1,0))
edgePath (E' v L) = p2i v ~~ (p2i v .+^ r2i (-1,0))
edgePath (E' v U) = p2i v ~~ (p2i v .+^ r2i (0,1))
edgePath (E' v D) = p2i v ~~ (p2i v .+^ r2i (0,-1))

irregularGridPaths :: SGrid a -> (Path V2 Double, Path V2 Double)
irregularGridPaths (Grid _ m) = (toPath outer, toPath inner)
  where
    (outer, inner) = edges (M.keysSet m) (`M.member` m)
    toPath = mconcat . map edgePath

irregularGrid :: Backend' b =>
                 SGrid a -> Diagram b
irregularGrid g = stroke outer # lwG (3 * gridwidth) # lineCap LineCapSquare <>
                  stroke inner # lwG gridwidth
  where
    (outer, inner) = irregularGridPaths g

-- | In a square grid, use the first argument to draw things at the centres
--   of cells given by coordinates.
atCentres :: (Transformable a, Monoid a, InSpace V2 Double a) =>
             (t -> a) -> [(Coord, t)] -> a
atCentres dc = translate (r2 (1/2, 1/2)) . atVertices dc

atCentres' :: (Transformable a, InSpace V2 Double a) => SGrid a -> [a]
atCentres' = translate (r2 (1/2, 1/2)) . atVertices'

-- | In a square grid, use the first argument to draw things
--   at the grid vertices given by coordinates.
atVertices :: (Transformable a, Monoid a, InSpace V2 Double a) =>
              (t -> a) -> [(Coord, t)] -> a
atVertices dc = mconcat . map (\ (p, c) -> dc c # translatep p)

atVertices' :: (Transformable a, InSpace V2 Double a) => SGrid a -> [a]
atVertices' g = [ (g ! c) # translatep c | c <- cells g ]

edge :: Edge -> Path V2 Double
edge (E c d) = rule d # translate (r2i c)
  where
    rule V = vrule 1.0 # alignB
    rule H = hrule 1.0 # alignL

dualEdge :: Edge -> Path V2 Double
dualEdge = translate (r2 (1/2, 1/2)) . edge

edgeStyle :: (HasStyle a, InSpace V2 Double a) => a -> a
edgeStyle = lineCap LineCapSquare . lwG edgewidth

thinEdgeStyle :: (HasStyle a, InSpace V2 Double a) => a -> a
thinEdgeStyle = lineCap LineCapSquare . lwG onepix

drawEdges :: Backend' b => [Edge] -> Diagram b
drawEdges = edgeStyle . stroke . mconcat . map edge

drawDualEdges :: Backend' b => [Edge] -> Diagram b
drawDualEdges = edgeStyle . stroke . mconcat . map dualEdge

drawThinDualEdges :: Backend' b => [Edge] -> Diagram b
drawThinDualEdges = thinEdgeStyle . stroke . mconcat . map dualEdge

drawAreaGrid :: (Backend' b, Eq a) =>
                  SGrid a -> Diagram b
drawAreaGrid = drawEdges . borders <> grid . size

fillBG :: Backend' b => Colour Double -> Diagram b
fillBG c = square 1 # lwG 0 # fc c

shadeGrid :: Backend' b =>
              SGrid (Maybe (Colour Double)) -> Diagram b
shadeGrid = mconcat . atCentres' . fmap (maybe mempty fillBG)

drawShadedGrid :: Backend' b =>
                  SGrid Bool -> Diagram b
drawShadedGrid = shadeGrid . fmap f
  where
    f True  = Just gray
    f False = Nothing

drawAreaGridGray :: Backend' b =>
                    SGrid Char -> Diagram b
drawAreaGridGray = drawAreaGrid <> shadeGrid . fmap cols
  where
    cols c | isUpper c  = Just (blend 0.1 black white)
           | otherwise  = Nothing

irregAreaGridX :: Backend' b =>
                      SGrid Char -> Diagram b R2
irregAreaGridX = irregularGrid <> drawEdges . borders <> shadeGrid . fmap cols
  where
    cols 'X' = Just gray
    cols _   = Nothing

-- Place a list of diagrams along a ray, with steps of size
-- @f@.
distrib :: (Transformable c, Monoid c, InSpace V2 Double c) =>
           V2 Double -> (Int, Int) -> Double -> [c] -> c
distrib base dir f xs =
    translate (0.75 *^ dir' ^+^ base) . mconcat $
        zipWith (\i d -> translate (fromIntegral i *^ dir') d) [(0 :: Int)..] xs
  where
    dir' = f *^ r2i dir

outsideGen :: (Transformable c, Monoid c, InSpace V2 Double c) =>
              (OutsideClue [c] -> V2 Double) -> Double -> [OutsideClue [c]] -> c
outsideGen tobase f ocs = mconcat . map placeOC $ ocs
  where
    placeOC o = distrib (tobase o) (ocDir o) f (ocValue o)

outsideCells :: (Transformable c, Monoid c, InSpace V2 Double c) =>
                Double -> [OutsideClue [c]] -> c
outsideCells = outsideGen base
  where
    base (OClue (bx, by) (dx, dy) _)
        | dx /= 0   = r2 (fromIntegral bx - 1, fromIntegral by - 1/2)
        | dy /= 0   = r2 (fromIntegral bx - 1/2, fromIntegral by)
        | otherwise = error "invalid outside clue"

outsideVertices :: (Transformable c, Monoid c, InSpace V2 Double c) =>
                   Double -> [OutsideClue [c]] -> c
outsideVertices = outsideGen base
  where
    base (OClue (bx, by) (dx, dy) _)
        | dx /= 0   = r2 (fromIntegral bx, fromIntegral by)
        | dy /= 0   = r2 (fromIntegral bx, fromIntegral by)
        | otherwise = error "invalid outside clue"
