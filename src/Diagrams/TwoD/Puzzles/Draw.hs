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
    where l dir len (i, j) = l' dir len # lw (w i j) # lineCap LineCapSquare
          l' V = vline
          l' H = hline
          w 0 _ = 0.1
          w _ 0 = 0.1
          w _ _ = 0.01

sudokugrid = gridgen l 9 9
    where l dir len (i, j) = l' dir len # lw (w i j) # lineCap LineCapSquare
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

fillBG c = square 1 # fc c # alignBL

drawClues dc = translate (r2 (0.5, 0.5))
             . mconcat
             . map (\ (p, c) -> dc c # translatep p)

drawText t = text t # fontSize 0.7 # font "Helvetica"
drawInt s = drawText (show s)
drawChar c = drawText [c]

fillogrid x y = grid x y -- TODO: dashed lines

drawClueGrid g = drawClues drawChar (clues g) `atop` grid sx sy
    where (sx, sy) = size g

drawIntClues = drawClues drawInt . clues
drawGrid g = gridpx sx sy id
    where (sx, sy) = size g

drawIntGrid g = drawIntClues g `atop` drawGrid g

drawSlitherGrid g = drawClues drawInt (clues g) `atop` slithergrid sx sy
    where (sx, sy) = size g

pearl MWhite = circle 0.35 # lw 0.05
pearl MBlack = pearl MWhite # fc black

drawMasyuGrid g = drawClues pearl (clues g) `atop` grid sx sy
    where (sx, sy) = size g

cross :: Path R2
cross = fromVertices [p2 (-1/2,-1/2), p2 (1/2,1/2)]
    <> fromVertices [p2 (1/2,-1/2), p2 (-1/2,1/2)]

drawCompassClue (n, e, s, w) = mconcat
    [ text n # translate (r2 (0,1/3))
    , text e # translate (r2 (1/3,0))
    , text s # translate (r2 (0,-1/3))
    , text w # translate (r2 (-1/3,0))
    , stroke cross
    ] # fontSize 0.3 

charGridBG g f = mconcat [ maybe mempty (translatep p . fillBG) (f p)
                         | p <- points g
                         ]

drawGridBG g f = drawAreaGrid g `atop` charGridBG g f
drawGridBG' g f' = drawGridBG g (\p -> f' (g ! p))

drawAreaGridG g = drawGridBG' g cols
    where cols c | 'A' <= c && c <= 'Z'  = Just (blend 0.1 black white)
                 | otherwise             = Nothing

frame = extrudeLeft x . extrudeRight x . extrudeTop x . extrudeBottom x . centerXY
    where x = 1.0

thermo vs@(v:_) = (bulb `atop` line) # col # translate (r2 (0.5, 0.5))
    where bulb = circle 0.4 # moveTo v
          line = strokeLocLine (fromVertices vs) # lw 0.55 # lineCap LineCapSquare
          col = lc gr . fc gr
          gr = blend 0.6 white black

gridlines :: Int -> Int -> Path R2
gridlines w h = (decoratePath xaxis . repeat . alignB . vrule . fromIntegral $ h)
            <> (decoratePath yaxis . repeat . alignL . hrule . fromIntegral $ w)
    where xaxis = fromVertices [ p2 (fromIntegral x, 0) | x <- [1..w-1] ]
          yaxis = fromVertices [ p2 (0, fromIntegral y) | y <- [1..h-1] ]

gridres = 40
onepix = 1 / (fromIntegral gridres)
twopix = 2 * onepix
fourpix = 4 * onepix

gridwidth = onepix
framewidthfactor = 4
edgewidth = 3 * onepix

borderwidth = 1 / 4 + onepix / 2

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
    <> phantom (f :: D R2)
    where f = stroke . translate (r2 (-bw, -bw)) . alignBL
              $ rect ((fromIntegral w) + 2 * bw) ((fromIntegral h) + 2 * bw)
          bw = borderwidth

bgdashing ds offs c x = x # dashing ds offs <> x # lc c

dashes = [5 / 40, 3 / 40]
dashoffset = 2.5 / 40

dashedgridpx w h = gridpx w h $ bgdashing dashes dashoffset white'
    where white' = blend 0.95 white black

drawAreaGrid g = drawedges g `atop` gridpx sx sy id
    where (sx, sy) = size g
          edges = mconcat . map drawEdge . borders
          drawedges = lineCap LineCapSquare . lw edgewidth . edges
