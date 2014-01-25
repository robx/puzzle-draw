{-# LANGUAGE NoMonomorphismRestriction #-}

module Draw where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Util
import Diagrams.Combinators

import Grid

box x y = strokeLoop . closeLine . fromVertices . map p2 $ [(0, 0), (x, 0), (x, y), (0, y)]

vline n = strokeLine . fromVertices . map p2 $ [(0, 0), (0, n)]
hline n = strokeLine . fromVertices . map p2 $ [(0, 0), (n, 0)]

hcatsep = hcat' with {_sep = 1}
vcatsep = cat' (r2 (0,1)) with {_sep = 1}

grid x y = box (fromIntegral x) (fromIntegral y) # lw 0.1
           `atop` (hcatsep (mempty : replicate (x - 1) (vline (fromIntegral y)))) # lw 0.01
           `atop` (vcatsep (mempty : replicate (y - 1) (hline (fromIntegral x)))) # lw 0.01

dot = circle 0.05 # fc black # withEnvelope (vrule 0 :: D R2)

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
drawGridBG' g f' = drawGridBG g (\p -> f' (g `Grid.at` p))

drawCharGridG g = drawGridBG' g cols
    where cols c | 'A' <= c && c <= 'Z'  = Just (blend 0.1 black white)
                 | otherwise             = Nothing

padc = pad 1.05 . centerXY
