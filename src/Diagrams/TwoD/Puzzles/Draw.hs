{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams.TwoD.Puzzles.Draw where

import Diagrams.Prelude
import Diagrams.Util
import Diagrams.Combinators

import Data.Puzzles.Grid

vline n = strokeLine . fromVertices . map p2 $ [(0, 0), (0, n)]
hline n = strokeLine . fromVertices . map p2 $ [(0, 0), (n, 0)]

hcatsep = hcat' with {_sep = 1}
vcatsep = cat' (r2 (0,1)) with {_sep = 1}

gridgen line x y = (hcatsep . map (line V y') $ zip [0..x] [x,x-1..0])
                   `atop` (vcatsep . map (line H x') $ zip [0..y] [y,y-1..0])
    where x' = fromIntegral x
          y' = fromIntegral y

grid = gridgen l
    where l dir len (i, j) = l' dir len # lw (w i j) # lineCap LineCapRound
          l' V = vline
          l' H = hline
          w 0 _ = 0.1
          w _ 0 = 0.1
          w _ _ = 0.01

sudokugrid = gridgen l 9 9
    where l dir len (i, j) = l' dir len # lw (w i j) # lineCap LineCapRound
          l' V = vline
          l' H = hline
          w k _ | k `mod` 3 == 0 = 0.1
          w _ _ = 0.01

smash = withEnvelope (vrule 0 :: D R2)

dot = circle 0.05 # fc black # smash

slithergrid x y = dots
    where dots = hcatsep . replicate (x + 1) . vcatsep . replicate (y + 1) $ dot

translatep (x, y) = translate . r2 $ (fromIntegral x, fromIntegral y)

drawEdge (E p d) = line # translatep p
    where line = case d of V -> vline 1
                           H -> hline 1

fillBG p c = square 1 # fc c # alignBL # translatep p

drawStr p s = text s # fontSize 0.7 # font "Helvetica" # translatep p

drawCharGrid g = edges g # lw 0.08 # lineCap LineCapRound `atop` grid sx sy
    where (sx, sy) = size g
          edges = mconcat . map drawEdge . borders

drawSlitherGrid g = drawClues g # translate (r2 (0.5, 0.5)) `atop` slithergrid sx sy
    where (sx, sy) = size g
          drawClues = mconcat . map (\ (p, c) -> drawStr p (show c)) . clues

charGridBG g f = mconcat [maybe mempty (fillBG p) (f p) | p <- points g]

drawGridBG g f = drawCharGrid g `atop` charGridBG g f
drawGridBG' g f' = drawGridBG g (\p -> f' (g ! p))

drawCharGridG g = drawGridBG' g cols
    where cols c | 'A' <= c && c <= 'Z'  = Just (blend 0.1 black white)
                 | otherwise             = Nothing

frame = extrudeLeft x . extrudeRight x . extrudeTop x . extrudeBottom x . centerXY
    where x = 0.25
