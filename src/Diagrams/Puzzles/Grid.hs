{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}

module Diagrams.Puzzles.Grid where

import Data.Char (isUpper)

import Diagrams.Prelude

import Data.Puzzles.Grid
import Data.Puzzles.GridShape hiding (size, cells)

import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Widths

-- | Draw a small black dot with no envelope.
dot :: (Renderable (Path R2) b, Backend b R2) => Diagram b R2
dot = circle 0.05 # fc black # smash

-- | Draw a Slither Link style grid of dots of the specified size.
slithergrid :: (Backend b R2, Renderable (Path R2) b) =>
               Size -> Diagram b R2
slithergrid (x, y) =
    hcatsep . replicate (x + 1) . vcatsep . replicate (y + 1) $ dot

-- | The inner grid lines of a square grid of the specified size.
gridlines :: Size -> Path R2
gridlines (w, h) = (decoratePath xaxis . repeat . alignB . vrule . fromIntegral $ h)
            <> (decoratePath yaxis . repeat . alignL . hrule . fromIntegral $ w)
    where xaxis = fromVertices [ p2 (fromIntegral x, 0) | x <- [1..w-1] ]
          yaxis = fromVertices [ p2 (0, fromIntegral y) | y <- [1..h-1] ]

-- | Draw a frame around the outside of a rectangle.
outframe :: Renderable (Path R2) b => Size -> Diagram b R2
outframe (w, h) = strokePointLoop r # lw fw
    where wd = fromIntegral w
          hd = fromIntegral h
          strokePointLoop = strokeLocTrail . mapLoc (wrapLoop . closeLine)
                            . fromVertices . map p2
          fw = framewidthfactor * gridwidth
          e = fw / 2 - gridwidth / 2
          r = [(-e, -e), (wd + e, -e), (wd + e, hd + e), (-e, hd + e)]

-- | Draw a square grid, applying the given style to the grid lines.
grid' :: (Backend b R2, Renderable (Path R2) b) =>
         (Diagram b R2 -> Diagram b R2) -> Size -> Diagram b R2
grid' gridstyle s =
    outframe s
    <> stroke (gridlines s) # lw gridwidth # gridstyle

-- | Draw a square grid with default grid line style.
grid :: (Backend b R2, Renderable (Path R2) b) =>
        Size -> Diagram b R2
grid = grid' id

bgdashing :: (Semigroup a, HasStyle a) =>
             [Double] -> Double -> Colour Double -> a -> a
bgdashing ds offs c x = x # dashing ds offs <> x # lc c

dashes :: [Double]
dashes = [5 / 40, 3 / 40]

dashoffset :: Double
dashoffset = 2.5 / 40

-- | Draw a square grid with dashed grid lines. The gaps
--   between dashes are off-white to aid in using filling
--   tools.
dashedgrid :: (Backend b R2, Renderable (Path R2) b) =>
              Size -> Diagram b R2
dashedgrid = grid' $ bgdashing dashes dashoffset white'
  where
    white' = blend 0.95 white black

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

edgeStyle :: HasStyle a => a -> a
edgeStyle = lineCap LineCapSquare . lw edgewidth

thinEdgeStyle :: HasStyle a => a -> a
thinEdgeStyle = lineCap LineCapSquare . lw onepix

drawEdges :: Renderable (Path R2) b => [Edge] -> Diagram b R2
drawEdges = edgeStyle . stroke . mconcat . map edge

drawDualEdges :: Renderable (Path R2) b => [Edge] -> Diagram b R2
drawDualEdges = edgeStyle . stroke . mconcat . map dualEdge

drawThinDualEdges :: Renderable (Path R2) b => [Edge] -> Diagram b R2
drawThinDualEdges = thinEdgeStyle . stroke . mconcat . map dualEdge

drawAreaGrid :: (Backend b R2, Renderable (Path R2) b, Eq a) =>
                  SGrid a -> Diagram b R2
drawAreaGrid = drawEdges . borders <> grid . size

fillBG :: (Backend b R2, Renderable (Path R2) b) => Colour Double -> Diagram b R2
fillBG c = square 1 # fc c

shadeGrid :: (Backend b R2, Renderable (Path R2) b) =>
              SGrid (Maybe (Colour Double)) -> Diagram b R2
shadeGrid = mconcat . atCentres' . fmap (maybe mempty fillBG)

drawShadedGrid :: (Backend b R2, Renderable (Path R2) b) =>
                  SGrid Bool -> Diagram b R2
drawShadedGrid = shadeGrid . fmap f
  where
    f True  = Just gray
    f False = Nothing

drawAreaGridGray :: (Backend b R2, Renderable (Path R2) b) =>
                    SGrid Char -> Diagram b R2
drawAreaGridGray = drawAreaGrid <> shadeGrid . fmap cols
  where
    cols c | isUpper c  = Just (blend 0.1 black white)
           | otherwise  = Nothing
