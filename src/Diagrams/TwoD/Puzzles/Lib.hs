{-# LANGUAGE FlexibleContexts, TypeFamilies #-}

module Diagrams.TwoD.Puzzles.Lib where

import Diagrams.Prelude

import Graphics.SVGFonts.ReadFont

import Control.Arrow ((***))

vline n = strokeLine . fromVertices . map p2 $ [(0, 0), (0, n)]
hline n = strokeLine . fromVertices . map p2 $ [(0, 0), (n, 0)]

hcatsep :: (Juxtaposable a, HasOrigin a, Monoid' a, V a ~ R2) => [a] -> a
hcatsep = hcat' with {_sep = 1}

vcatsep :: (Juxtaposable a, HasOrigin a, Monoid' a, V a ~ R2) => [a] -> a
vcatsep = cat' (r2 (0,1)) with {_sep = 1}

smash :: Backend b R2 => QDiagram b R2 Any -> QDiagram b R2 Any
smash = withEnvelope (vrule 0 :: D R2)

translatep (x, y) = translate . r2 $ (fromIntegral x, fromIntegral y)

p2i :: (Int, Int) -> P2
p2i = p2 . (fromIntegral *** fromIntegral)

interleave [] _ = []
interleave (x:xs) ys = x : interleave ys xs

spread v things = cat v . interleave (repeat (strut vgap)) $ things
    where ds = map (diameter v) things
          gap = (magnitude v - sum ds) / fromIntegral (length things + 1)
          vgap = (gap / magnitude v) *^ v

-- a ||| b, a and b are aligned by vertical center, origin is origin of a

dmid a = (dtop + dbot) / 2 - dbot
    where menv = magnitude . envelopeV
          dtop = menv unitY a
          dbot = menv ((-1) *^ unitY) a

besidesL :: (Semigroup m, Backend b R2, Monoid m, Renderable (Path R2) b) =>
           QDiagram b R2 m -> QDiagram b R2 m -> QDiagram b R2 m
besidesL a b = a ||| strutX 0.5 ||| b'
    where b' = b # centerY # translate (dmid a *^ unitY)

besidesR :: (Semigroup m, Backend b R2, Monoid m, Renderable (Path R2) b) =>
           QDiagram b R2 m -> QDiagram b R2 m -> QDiagram b R2 m
besidesR b a =  b' ||| strutX 0.5 ||| a
    where b' = b # centerY # translate (dmid a *^ unitY)

fit f a = scale (f / m) a
    where m = max (magnitude (diameter unitX a))
                  (magnitude (diameter unitY a))

text' t = stroke (textSVG' $ TextOpts t bit INSIDE_H KERN False 1 1)
          # lw 0 # fc black # scale 0.8
-- text' t = text t # fontSize 0.8 # font "Helvetica" # translate (r2 (0.04, -0.07))
          <> phantom (textrect t :: D R2)
textrect t = rect (fromIntegral (length t) * 0.4) 0.7 # lc red
text'' t = text' t `atop` textrect t

