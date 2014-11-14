{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Diagrams.Puzzles.Grid where

import Data.Char (isUpper)
import qualified Data.Map as M

import Diagrams.Prelude

import Data.Puzzles.Grid
import Data.Puzzles.GridShape hiding (size, cells)

import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Widths

-- | Draw a small black dot with no envelope.
dot :: Backend' b => Diagram b R2
dot = circle 0.05 # fc black # smash

-- | Draw a Slither Link style grid of dots of the specified size.
slithergrid :: Backend' b =>
               Size -> Diagram b R2
slithergrid (x, y) =
    hcatsep . replicate (x + 1) . vcatsep . replicate (y + 1) $ dot

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

-- | Draw a frame around the outside of a rectangle.
outframe' :: Backend' b => Double -> Size -> Diagram b R2
outframe' f (w, h) = strokePointLoop r # lwG fw
    where wd = fromIntegral w
          hd = fromIntegral h
          strokePointLoop = strokeLocTrail . mapLoc (wrapLoop . closeLine)
                            . fromVertices . map p2
          fw = f * gridwidth
          e = fw / 2 - gridwidth / 2
          r = [(-e, -e), (wd + e, -e), (wd + e, hd + e), (-e, hd + e)]

outframe :: Backend' b => Size -> Diagram b R2
outframe = outframe' framewidthfactor

-- | Draw a square grid, applying the given style to the grid lines.
grid' :: Backend' b =>
         (Diagram b R2 -> Diagram b R2) -> Size -> Diagram b R2
grid' gridstyle s =
    outframe s
    <> stroke (gridlines s) # lwG gridwidth # gridstyle

-- | Draw a square grid with default grid line style.
grid :: Backend' b =>
        Size -> Diagram b R2
grid = grid' id

-- | Draw a square grid with thin frame.
plaingrid :: Backend' b =>
             Size -> Diagram b R2
plaingrid s = stroke (fullgridlines s) # lwG gridwidth

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

-- | Draw a square grid with dashed grid lines. The gaps
--   between dashes are off-white to aid in using filling
--   tools.
dashedgrid :: Backend' b =>
              Size -> Diagram b R2
dashedgrid = grid' gridDashing

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

irregularGrid' :: Backend' b =>
                  (Diagram b R2 -> Diagram b R2) -> SGrid a -> Diagram b R2
irregularGrid' style g =
    stroke outer # lwG edgewidth # lineCap LineCapSquare <>
    stroke inner # lwG gridwidth # style
  where
    (outer, inner) = irregularGridPaths g

irregularGrid :: Backend' b =>
                 SGrid a -> Diagram b R2
irregularGrid = irregularGrid' id

-- | In a square grid, use the first argument to draw things at the centres
--   of cells given by coordinates.
atCentres :: (Transformable a, Monoid a, V a ~ R2) =>
             (t -> a) -> [(Coord, t)] -> a
atCentres dc = translate (r2 (1/2, 1/2)) . atVertices dc

atCentres' :: (Transformable a, V a ~ R2) => SGrid a -> [a]
atCentres' = translate (r2 (1/2, 1/2)) . atVertices'

onGrid :: (Transformable a, Monoid a, V a ~ R2) =>
          Double -> Double -> (t -> a) -> [(Coord, t)] -> a
onGrid dx dy f = mconcat . map g
  where
    g (p, c) = f c # translate (r2coord p)
    r2coord (x, y) = r2 (dx * fromIntegral x, dy * fromIntegral y)

-- | In a square grid, use the first argument to draw things
--   at the grid vertices given by coordinates.
atVertices :: (Transformable a, Monoid a, V a ~ R2) =>
              (t -> a) -> [(Coord, t)] -> a
atVertices = onGrid 1 1

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

drawThinEdges :: Backend' b => [Edge] -> Diagram b R2
drawThinEdges = thinEdgeStyle . stroke . mconcat . map edge

drawDualEdges :: Backend' b => [Edge] -> Diagram b R2
drawDualEdges = edgeStyle . stroke . mconcat . map dualEdge

drawThinDualEdges :: Backend' b => [Edge] -> Diagram b R2
drawThinDualEdges = thinEdgeStyle . stroke . mconcat . map dualEdge

drawAreaGrid' :: (Backend' b, Eq a) =>
                  (Diagram b R2 -> Diagram b R2) ->
                  SGrid a -> Diagram b R2
drawAreaGrid' style = drawEdges . borders <> grid' style . size

drawAreaGrid :: (Backend' b, Eq a) =>
                  SGrid a -> Diagram b R2
drawAreaGrid = drawAreaGrid' id

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

drawAreaGridGray' :: Backend' b =>
                     (Diagram b R2 -> Diagram b R2) ->
                     SGrid Char -> Diagram b R2
drawAreaGridGray' style = drawAreaGrid' style <> shadeGrid . fmap cols
  where
    cols c | isUpper c  = Just (blend 0.1 black white)
           | otherwise  = Nothing

drawAreaGridGray :: Backend' b =>
                    SGrid Char -> Diagram b R2
drawAreaGridGray = drawAreaGridGray' id

irregAreaGridX :: Backend' b =>
                  (Diagram b R2 -> Diagram b R2) -> SGrid Char -> Diagram b R2
irregAreaGridX style = drawEdges . borders <> irregularGrid' style <> shadeGrid . fmap cols
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
