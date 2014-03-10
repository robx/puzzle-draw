{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Diagrams.TwoD.Puzzles.Grid where

import Diagrams.Prelude

import Data.Puzzles.Grid

import Diagrams.TwoD.Puzzles.Lib
import Diagrams.TwoD.Puzzles.Widths

-- | Draw a small black dot with no envelope.
dot :: (Renderable (Path R2) b, Backend b R2) => Diagram b R2
dot = circle 0.05 # fc black # smash

-- | Draw a Slither Link style grid of dots of the specified size.
slithergrid :: (Backend b R2, Renderable (Path R2) b) =>
               Size -> Diagram b R2
slithergrid s@(x, y) = dots <> phantom (frame s)
    where dots = hcatsep . replicate (x + 1) . vcatsep . replicate (y + 1) $ dot

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
    <> phantom (frame s)

-- | Draw a square grid with default grid line style.
grid :: (Backend b R2, Renderable (Path R2) b) =>
        Size -> Diagram b R2
grid = grid' id

bgdashing ds offs c x = x # dashing ds offs <> x # lc c
dashes = [5 / 40, 3 / 40]
dashoffset = 2.5 / 40

-- | Draw a square grid with dashed grid lines. The gaps
--   between dashes are off-white to aid in using filling
--   tools.
dashedgrid :: (Backend b R2, Renderable (Path R2) b) =>
              Size -> Diagram b R2
dashedgrid = grid' $ bgdashing dashes dashoffset white'
  where
    white' = blend 0.95 white black

fillBG c = square 1 # fc c # alignBL

-- | In a square grid, use the first argument to draw things at the centres
--   of cells given by coordinates.
atCentres :: (Transformable a, Monoid a, V a ~ R2) => (t -> a) -> [(Coord, t)] -> a
atCentres dc = translate (r2 (0.5, 0.5)) . atVertices dc

-- | In a square grid, use the first argument to draw things at the grid vertices
--   given by coordinates.
atVertices :: (Transformable a, Monoid a, V a ~ R2) => (t -> a) -> [(Coord, t)] -> a
atVertices dc = mconcat . map (\ (p, c) -> dc c # translatep p)

charGridBG g f = mconcat [ maybe mempty (translatep p . fillBG) (f p)
                         | p <- cells g
                         ]
charGridBGcaps g = charGridBG g (\p -> cols (g ! p))
    where cols c | 'A' <= c && c <= 'Z'  = Just (blend 0.1 black white)
                 | otherwise             = Nothing

drawGridBG g f = drawAreaGrid g `atop` charGridBG g f
drawGridBG' g f' = drawGridBG g (\p -> f' (g ! p))

drawAreaGridG g = drawGridBG' g cols
    where cols c | 'A' <= c && c <= 'Z'  = Just (blend 0.1 black white)
                 | otherwise             = Nothing

frame :: Size -> D R2
frame (w, h) = stroke . translate (r2 (-bw, -bw)) . alignBL
               $ rect (fromIntegral w + 2 * bw) (fromIntegral h + 2 * bw)
  where
    bw = borderwidth

edge :: Edge -> Path R2
edge (E c d) = rule d # translate (r2i c)
  where
    rule V = vrule 1.0 # alignB
    rule H = hrule 1.0 # alignL

dualEdge :: Edge -> Path R2
dualEdge = translate (r2 (1/2, 1/2)) . edge

edgeStyle :: HasStyle a => a -> a
edgeStyle = lineCap LineCapSquare . lw edgewidth

drawEdges :: Renderable (Path R2) b => [Edge] -> Diagram b R2
drawEdges = edgeStyle . stroke . mconcat . map edge

drawDualEdges :: Renderable (Path R2) b => [Edge] -> Diagram b R2
drawDualEdges = edgeStyle . stroke . mconcat . map dualEdge

drawAreaGrid :: (Backend b R2, Renderable (Path R2) b, Eq a) =>
                  Grid a -> Diagram b R2
drawAreaGrid = drawEdges . borders <> grid . size

drawShadedGrid :: (Backend b R2, Renderable (Path R2) b) =>
                  Grid Bool -> Diagram b R2
drawShadedGrid = atCentres (const $ fillBG gray # centerXY) . clues . fmap toMaybe
  where
    toMaybe True  = Just ()
    toMaybe False = Nothing

