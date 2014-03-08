{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module Diagrams.TwoD.Puzzles.Draw where

import Diagrams.Prelude hiding (Point)
import Diagrams.Util
import Diagrams.Combinators

import Data.Puzzles.Grid
import Data.Puzzles.Things
import Data.Puzzles.Sudoku

import Diagrams.TwoD.Puzzles.Lib
import Diagrams.TwoD.Puzzles.Widths
import Diagrams.TwoD.Puzzles.Things

dot = circle 0.05 # fc black # smash

slithergrid :: (Backend b R2, Renderable (Path R2) b) => Int -> Int -> QDiagram b R2 Any
slithergrid x y = dots <> phantom (frame x y)
    where dots = hcatsep . replicate (x + 1) . vcatsep . replicate (y + 1) $ dot

drawEdge (E p d) = line # translatep p
    where line = case d of V -> vline 1
                           H -> hline 1

fillBG c = square 1 # fc c # alignBL

drawClues dc = translate (r2 (0.5, 0.5))
             . mconcat
             . map (\ (p, c) -> dc c # translatep p)

drawFillo g = drawIntClues g <> dashedgridpx x y
    where (x, y) = size g

drawClueGrid g = drawClues drawChar (clues g) `atop` gridpx sx sy id
    where (sx, sy) = size g

drawIntClues = drawClues drawInt . clues
drawGrid g = gridpx sx sy id
    where (sx, sy) = size g

drawIntGrid g = drawIntClues g `atop` drawGrid g

drawSlitherGrid g = drawClues drawInt (clues g) `atop` slithergrid sx sy
    where (sx, sy) = size g

drawMasyuGrid g = drawClues pearl (clues g) `atop` gridpx sx sy id
    where (sx, sy) = size g

drawCompassClues g = drawClues drawCompassClue (clues g)

drawCompassGrid g = drawCompassClues g <> drawGrid g

charGridBG g f = mconcat [ maybe mempty (translatep p . fillBG) (f p)
                         | p <- points g
                         ]
charGridBGcaps g = charGridBG g (\p -> cols (g ! p))
    where cols c | 'A' <= c && c <= 'Z'  = Just (blend 0.1 black white)
                 | otherwise             = Nothing

drawGridBG g f = drawAreaGrid g `atop` charGridBG g f
drawGridBG' g f' = drawGridBG g (\p -> f' (g ! p))

drawAreaGridG g = drawGridBG' g cols
    where cols c | 'A' <= c && c <= 'Z'  = Just (blend 0.1 black white)
                 | otherwise             = Nothing

gridlines :: Int -> Int -> Path R2
gridlines w h = (decoratePath xaxis . repeat . alignB . vrule . fromIntegral $ h)
            <> (decoratePath yaxis . repeat . alignL . hrule . fromIntegral $ w)
    where xaxis = fromVertices [ p2 (fromIntegral x, 0) | x <- [1..w-1] ]
          yaxis = fromVertices [ p2 (0, fromIntegral y) | y <- [1..h-1] ]

outframe w h = strokePointLoop r # lw fw
    where wd = fromIntegral w
          hd = fromIntegral h
          strokePointLoop = strokeLocTrail . mapLoc (wrapLoop . closeLine)
                            . fromVertices . map p2
          fw = framewidthfactor * gridwidth
          e = fw / 2 - gridwidth / 2
          r = [(-e, -e), (wd + e, -e), (wd + e, hd + e), (-e, hd + e)]

gridpx w h gridstyle =
    outframe w h 
    <> stroke (gridlines w h) # lw gridwidth # gridstyle
    <> phantom (frame w h)

frame :: Int -> Int -> D R2
frame w h = stroke . translate (r2 (-bw, -bw)) . alignBL
        $ rect ((fromIntegral w) + 2 * bw) ((fromIntegral h) + 2 * bw)
    where bw = borderwidth

bgdashing ds offs c x = x # dashing ds offs <> x # lc c

dashes = [5 / 40, 3 / 40]
dashoffset = 2.5 / 40

dashedgridpx w h = gridpx w h $ bgdashing dashes dashoffset white'
    where white' = blend 0.95 white black

drawedges = lineCap LineCapSquare . lw edgewidth . mconcat . map drawEdge

sudokugrid g = drawedges (sudokubordersg g) `atop` gridpx sx sy id
    where (sx, sy) = size g

drawAreaGrid g = drawedges (borders g) `atop` gridpx sx sy id
    where (sx, sy) = size g

drawShadedGrid g = drawClues (const $ fillBG gray # centerXY) (clues g')
    where g' = fmap toMaybe g
          toMaybe True  = Just ()
          toMaybe False = Nothing

dualEdge :: Edge -> Path R2
dualEdge (E (x, y) d) = rule d # translate p
    where rule V = vrule 1.0 # translate (r2 (0.5, 1))
          rule H = hrule 1.0 # translate (r2 (1.0, 0.5))
          p = r2 (fromIntegral x, fromIntegral y)

drawDualEdges = lw edgewidth . lineCap LineCapSquare . stroke . mconcat . map dualEdge

drawWords ws = spread (-1.0 *^ unitY) (map (centerXY . scale 0.4 . drawText) ws)
               # centerY

drawWordsClues = drawClues drawWords . clues

drawTightGrid d g = drawClues (drawTight d) (clues . fmap Just $ g)
                    <> gridpx sx sy id
                    <> phantom (frame (sx + 2) (sy + 2) # translate (r2 (-1,-1)))
    where (sx, sy) = size g

drawCurve = lw onepix . fit 0.6 . centerXY . mconcat . map drawEdge

drawSlalomGrid g = drawClues drawSlalomClue (clues g) # translate (r2 (-1/2,-1/2))
                   <> gridpx (w-1) (h-1) id
                   <> phantom (frame w h) # translate (r2 (-1/2,-1/2))
    where (w, h) = size g

drawSlalomDiags g = drawClues diag (clues (fmap Just g))
    where diag '/' = stroke ur # lw edgewidth
          diag '\\' = stroke dr # lw edgewidth

drawCrosses g = drawClues c (clues g)
    where c _ = stroke cross # scale 0.8 # lw edgewidth


