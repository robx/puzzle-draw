{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Diagrams.Puzzles.Grid where

import Data.Char (isUpper)
import qualified Data.Map as M

import Diagrams.Prelude
import Diagrams.TwoD.Offset

import Data.Puzzles.Grid
import Data.Puzzles.GridShape hiding (dualEdge)

import Diagrams.Puzzles.Style
import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Widths

-- | Draw a small black dot with no envelope.
dot :: Backend' b => Diagram b R2
dot = circle 0.05 # fc black # smash

drawGrid :: Backend' b
         => GridStyle -> SGrid a -> Diagram b R2
drawGrid s g = case s of
    GridSlither       -> 
        hcatsep . replicate (x + 1) . vcatsep . replicate (y + 1) $ dot
    GridNormal        -> grid' id 
    GridPlain         -> stroke (fullgridlines sz) # lwG gridwidth
    GridDashed        -> grid' gridDashing
    GridPlainDashed   -> gridDashing (drawGrid GridPlain g)
  where
    sz = size g
    (x, y) = sz
    grid' gridstyle =
        outframe sz
        <> stroke (gridlines sz) # lwG gridwidth # gridstyle

slithergrid :: Backend' b =>
               SGrid a -> Diagram b R2
slithergrid = drawGrid GridSlither

-- | Draw a square grid with default grid line style.
grid :: Backend' b =>
        SGrid a -> Diagram b R2
grid = drawGrid GridNormal

-- | Draw a square grid with thin frame.
plaingrid :: Backend' b =>
             SGrid a -> Diagram b R2
plaingrid = drawGrid GridPlain

-- | Draw a square grid with dashed grid lines. The gaps
--   between dashes are off-white to aid in using filling
--   tools.
dashedgrid :: Backend' b =>
              SGrid a -> Diagram b R2
dashedgrid = drawGrid GridDashed

-- | Draw a square grid with thin frame.
plaindashedgrid :: Backend' b =>
                   SGrid a -> Diagram b R2
plaindashedgrid = drawGrid GridPlainDashed

fence :: [Double] -> Double -> Path R2
fence xs h = decoratePath xspath (repeat v)
  where
    xspath = fromVertices [ p2 (x, 0) | x <- xs ]
    v = alignB (vrule h)

-- | The inner grid lines of a square grid of the specified size.
gridlines :: Size -> Path R2
gridlines (w, h) = fence' w h <> mirror (fence' h w)
  where
    fence' n l = fence (map fromIntegral [1..n-1]) (fromIntegral l)

fullgridlines :: Size -> Path R2
fullgridlines (w, h) = fence' w h <> mirror (fence' h w)
  where
    fence' n l = fence (map fromIntegral [0..n]) (fromIntegral l)

outframe' :: Backend' b => Double -> Size -> Diagram b R2
outframe' f (w, h) = fr # lwG 0 # fc black
  where
    fr = stroke $ rin <> rout
    rout = reversePath $ offsetPath (f * gridwidth - e) r
    rin = offsetPath (-e) r
    r = rect wd hd # alignBL
    wd = fromIntegral w
    hd = fromIntegral h
    e = gridwidth / 2

outframe :: Backend' b => Size -> Diagram b R2
outframe = outframe' framewidthfactor

bgdashingG :: (Semigroup a, HasStyle a, V a ~ R2) =>
             [Double] -> Double -> Colour Double -> a -> a
bgdashingG ds offs c x = x # dashingG ds offs <> x # lc c

dashes :: [Double]
dashes = [5 / 40, 3 / 40]

dashoffset :: Double
dashoffset = 2.5 / 40

gridDashing :: (Semigroup a, HasStyle a, V a ~ R2) => a -> a
gridDashing = bgdashingG dashes dashoffset white'
  where
    white' = blend 0.95 white black

edgePath :: Edge' (Vertex Square) -> Path R2
edgePath (E' v R) = p2i v ~~ p2i (v ^+^ (1,0))
edgePath (E' v L) = p2i v ~~ p2i (v ^+^ (-1,0))
edgePath (E' v U) = p2i v ~~ p2i (v ^+^ (0,1))
edgePath (E' v D) = p2i v ~~ p2i (v ^+^ (0,-1))

irregularGridPaths :: SGrid a -> (Path R2, Path R2)
irregularGridPaths (Grid _ m) = (toPath outer, toPath inner)
  where
    (outer, inner) = edges (M.keysSet m) (`M.member` m)
    toPath = mconcat . map edgePath

irregularGrid :: Backend' b =>
                 SGrid a -> Diagram b R2
irregularGrid g = stroke outer # lwG (3 * gridwidth) # lineCap LineCapSquare <>
                  stroke inner # lwG gridwidth
  where
    (outer, inner) = irregularGridPaths g

-- | In a square grid, use the first argument to draw things at the centres
--   of cells given by coordinates.
atCentres :: (Transformable a, Monoid a, V a ~ R2) =>
             (t -> a) -> [(Coord, t)] -> a
atCentres dc = translate (r2 (1/2, 1/2)) . atVertices dc

atCentres' :: (Transformable a, V a ~ R2) => SGrid a -> [a]
atCentres' = translate (r2 (1/2, 1/2)) . atVertices'

-- | In a square grid, use the first argument to draw things
--   at the grid vertices given by coordinates.
atVertices :: (Transformable a, Monoid a, V a ~ R2) =>
              (t -> a) -> [(Coord, t)] -> a
atVertices dc = mconcat . map (\ (p, c) -> dc c # translatep p)

atVertices' :: (Transformable a, V a ~ R2) => SGrid a -> [a]
atVertices' g = [ (g ! c) # translatep c | c <- cells g ]

edge :: Edge -> Path R2
edge (E c d) = rule d # translate (r2i c)
  where
    rule V = vrule 1.0 # alignB
    rule H = hrule 1.0 # alignL

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

drawAreaGrid :: (Backend' b, Eq a) =>
                  SGrid a -> Diagram b R2
drawAreaGrid = drawEdges . borders <> grid

fillBG :: Backend' b => Colour Double -> Diagram b R2
fillBG c = square 1 # lwG onepix # fc c # lc c

shadeGrid :: Backend' b =>
              SGrid (Maybe (Colour Double)) -> Diagram b R2
shadeGrid = mconcat . atCentres' . fmap (maybe mempty fillBG)

drawShadedGrid :: Backend' b =>
                  SGrid Bool -> Diagram b R2
drawShadedGrid = shadeGrid . fmap f
  where
    f True  = Just gray
    f False = Nothing

drawAreaGridGray :: Backend' b =>
                    SGrid Char -> Diagram b R2
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
