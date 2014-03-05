{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Diagrams.TwoD.Puzzles.Draw where

import Diagrams.Prelude
import Diagrams.Util
import Diagrams.Combinators
import Diagrams.TwoD.Offset
import Graphics.SVGFonts.ReadFont

import Control.Arrow ((***))

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

smash = withEnvelope (vrule 0 :: D R2)

dot = circle 0.05 # fc black # smash

slithergrid x y = dots <> phantom (frame x y)
    where dots = hcatsep . replicate (x + 1) . vcatsep . replicate (y + 1) $ dot

translatep (x, y) = translate . r2 $ (fromIntegral x, fromIntegral y)

drawEdge (E p d) = line # translatep p
    where line = case d of V -> vline 1
                           H -> hline 1

fillBG c = square 1 # fc c # alignBL

drawClues dc = translate (r2 (0.5, 0.5))
             . mconcat
             . map (\ (p, c) -> dc c # translatep p)

drawCrosses g = drawClues c (clues g)
    where c _ = stroke cross # scale 0.8 # lw edgewidth

text' t = stroke (textSVG' $ TextOpts t bit INSIDE_H KERN False 1 1)
          # lw (0.1 * onepix) # fc black # scale 0.8
-- text' t = text t # fontSize 0.8 # font "Helvetica" # translate (r2 (0.04, -0.07))
          <> phantom (textrect t :: D R2)
textrect t = rect (fromIntegral (length t) * 0.4) 0.7 # lc red
text'' t = text' t `atop` textrect t

drawText = text'

drawInt s = drawText (show s)
drawChar c = drawText [c]

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

pearl MWhite = circle 0.35 # lw 0.05
pearl MBlack = pearl MWhite # fc black

drawMasyuGrid g = drawClues pearl (clues g) `atop` gridpx sx sy id
    where (sx, sy) = size g

cross :: Path R2
cross = ur <> dr

drawCompassClue (CC n e s w) = texts <> stroke cross # lw onepix
    where tx Nothing _ = mempty
          tx (Just x) v = text' (show x) # scale 0.5 # translate (r2 v)
          texts = mconcat . zipWith tx [n, e, s, w] $
                  [(0,f), (f,0), (0,-f), (-f,0)]
          f = 3/10
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

thermo vs@(v:_) = (bulb `atop` line) # col # translate (r2 (0.5, 0.5))
    where bulb = circle 0.4 # moveTo v
          line = strokeLocLine (fromVertices vs) # lw 0.55 # lineCap LineCapSquare
          col = lc gr . fc gr
          gr = blend 0.6 white black

p2i = p2 . (fromIntegral *** fromIntegral)

drawThermos = mconcat . map (thermo . map p2i)

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

interleave [] _ = []
interleave (x:xs) ys = x : interleave ys xs

spread v things = cat v . interleave (repeat (strut vgap)) $ things
    where ds = map (diameter v) things
          gap = (magnitude v - sum ds) / fromIntegral ((length things) + 1)
          vgap = (gap / magnitude v) *^ v

drawWords ws = spread (-1.0 *^ unitY) (map (centerXY . scale 0.4 . drawText) ws)
               # centerY

drawWordsClues = drawClues drawWords . clues

ur :: Path R2
ur = fromVertices [p2 (-1/2,-1/2), p2 (1/2,1/2)]

dr :: Path R2
dr = fromVertices [p2 (1/2,-1/2), p2 (-1/2,1/2)]

drawTight d (Single x) = d x
drawTight d (UR x y) = stroke ur # lw onepix
                       <> d x # scale s # translate (r2 (-t,t))
                       <> d y # scale s # translate (r2 (t,-t))
    where t = 1/5
          s = 2/3
drawTight d (DR x y) = stroke dr # lw onepix
                       <> d x # scale s # translate (r2 (-t,-t))
                       <> d y # scale s # translate (r2 (t,t))
    where t = 1/5
          s = 2/3

drawTightGrid d g = drawClues (drawTight d) (clues . fmap Just $ g)
                    <> gridpx sx sy id
                    <> phantom (frame (sx + 2) (sy + 2) # translate (r2 (-1,-1)))
    where (sx, sy) = size g

stackWords = vcat' with {_sep = 0.1} . scale 0.8 . map (alignL . text')

-- a ||| b, a and b are aligned by vertical center, origin is origin of a

besidesL :: (Semigroup m, Backend b R2, Monoid m, Renderable (Path R2) b) =>
           QDiagram b R2 m -> QDiagram b R2 m -> QDiagram b R2 m
besidesL a b = a ||| strutX 0.5 ||| b'
    where b' = b # centerY # translate (dmid *^ unitY)
          dtop = magnitude $ envelopeV unitY a
          dbot = magnitude $ envelopeV ((-1) *^ unitY) a
          dmid = (dtop + dbot) / 2 - dbot

besidesR :: (Semigroup m, Backend b R2, Monoid m, Renderable (Path R2) b) =>
           QDiagram b R2 m -> QDiagram b R2 m -> QDiagram b R2 m
besidesR b a =  b' ||| strutX 0.5 ||| a
    where b' = b # centerY # translate (dmid *^ unitY)
          dtop = magnitude $ envelopeV unitY a
          dbot = magnitude $ envelopeV ((-1) *^ unitY) a
          dmid = (dtop + dbot) / 2 - dbot

drawMarkedWord (MW s e) = lw onepix . stroke $ expandTrail' with {_expandCap = LineCapRound} 0.4 t
    where t = fromVertices [p2i s, p2i e] # translate (r2 (1/2,1/2))

drawMarkedWords = mconcat . map drawMarkedWord

fit f a = scale (f / (max (magnitude (diameter unitX a)) (magnitude (diameter unitY a)))) a

drawCurve = lw onepix . fit 0.6 . centerXY . mconcat . map drawEdge

drawSlalomClue x = text' (show x) # scale 0.75 <> circle 0.4 # fc white # lw onepix

drawSlalomGrid g = drawClues drawSlalomClue (clues g) # translate (r2 (-1/2,-1/2))
                   <> gridpx (w-1) (h-1) id
                   <> phantom (frame w h) # translate (r2 (-1/2,-1/2))
    where (w, h) = size g

drawSlalomDiags g = drawClues diag (clues (fmap Just g))
    where diag '/' = stroke ur # lw edgewidth
          diag '\\' = stroke dr # lw edgewidth
