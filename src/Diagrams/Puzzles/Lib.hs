{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Diagrams.Puzzles.Lib where

import Diagrams.Prelude

import Graphics.SVGFonts.ReadFont
import Control.Arrow ((***))

import Paths_puzzle_draw (getDataFileName)
import System.IO.Unsafe (unsafePerformIO)

type Backend' b = (Backend b R2, Renderable (Path R2) b)

-- | Vertical/horizontal stroked line of given length.
vline, hline :: Backend' b => Double -> Diagram b R2
vline n = strokeLine . fromVertices . map p2 $ [(0, 0), (0, n)]
hline n = strokeLine . fromVertices . map p2 $ [(0, 0), (n, 0)]

-- | Variant of 'hcat'' that spreads with distance @1@.
hcatsep :: (Juxtaposable a, HasOrigin a, Monoid' a, V a ~ R2) => [a] -> a
hcatsep = hcat' with {_sep = 1}

-- | Variant of 'vcat'' that spreads with distance @1@,
-- and stacks towards the top.
vcatsep :: (Juxtaposable a, HasOrigin a, Monoid' a, V a ~ R2) => [a] -> a
vcatsep = cat' (r2 (0,1)) with {_sep = 1}

-- | Collapse the envelope to a point.
smash :: Backend' b => QDiagram b R2 Any -> QDiagram b R2 Any
smash = withEnvelope (pointDiagram origin :: D R2)

-- | Helper to translate by a point given as @(Int, Int)@.
translatep :: (Transformable t, V t ~ R2) => (Int, Int) -> t -> t
translatep = translate . r2i

-- | Convert pair of @Int@ to vector.
r2i :: (Int, Int) -> R2
r2i = r2 . (fromIntegral *** fromIntegral)

-- | Convert pair of @Int@ to point.
p2i :: (Int, Int) -> P2
p2i = p2 . (fromIntegral *** fromIntegral)

mirror :: (Transformable t, V t ~ R2) => t -> t
mirror = reflectAbout (p2 (0, 0)) (r2 (1, 1))

-- | Interleave two lists.
interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave (x:xs) ys = x : interleave ys xs

-- | Spread diagrams evenly along the given vector.
spread :: Backend' b => R2 -> [Diagram b R2] -> Diagram b R2
spread v things = cat v . interleave (repeat (strut vgap)) $ things
    where ds = map (diameter v) things
          gap' = (magnitude v - sum ds) / fromIntegral (length things + 1)
          vgap = (gap' / magnitude v) *^ v

dmid ::  (Enveloped a, V a ~ R2) => R2 -> a -> Double
dmid u a = (dtop + dbot) / 2 - dbot
    where menv v = magnitude . envelopeV v
          dtop = menv u a
          dbot = menv ((-1) *^ u) a

-- | Place the second diagram to the right of the first, aligning both
-- vertically. The origin is the origin of the left diagram.
besidesL :: (Backend' b, Semigroup m, Monoid m) =>
            QDiagram b R2 m -> QDiagram b R2 m -> QDiagram b R2 m
besidesL a b = a ||| strutX 0.5 ||| b'
    where b' = b # centerY # translate (dmid unitY a *^ unitY)

-- | Variant of 'besidesL' where the origin is that of the right diagram.
besidesR :: (Backend' b, Semigroup m, Monoid m) =>
           QDiagram b R2 m -> QDiagram b R2 m -> QDiagram b R2 m
besidesR b a =  b' ||| strutX 0.5 ||| a
    where b' = b # centerY # translate (dmid unitY a *^ unitY)

aboveT :: (Backend' b, Semigroup m, Monoid m) =>
          QDiagram b R2 m -> QDiagram b R2 m -> QDiagram b R2 m
aboveT a b = a === strutY 0.5 === b'
    where b' = b # centerX # translate (dmid unitX a *^ unitX)

-- | @fit f a@ scales @a@ to fit into a square of size @f@.
fit :: (Transformable t, Enveloped t, V t ~ R2) =>
       Double -> t -> t
fit f a = scale (f / m) a
    where m = max (magnitude (diameter unitX a))
                  (magnitude (diameter unitY a))

-- | Write text that is centered both vertically and horizontally and that
-- has an envelope. Sized such that single capital characters fit nicely
-- into a square of size @1@.
text' :: Backend' b => String -> Diagram b R2
text' t = stroke (textSVG' $ TextOpts t fnt INSIDE_H KERN False 1 1)
          # lwG 0 # rfc black # scale 0.8
  where
    fnt = outlMap . unsafePerformIO . getDataFileName
        $ "data/fonts/gen-light.svg"
    rfc :: (HasStyle a, V a ~ R2) => Colour Double -> a -> a
    rfc = recommendFillColor

-- text' t = text t # fontSize 0.8 # font "Helvetica" # translate (r2 (0.04, -0.07))
--          <> phantom' (textrect t)
--textrect :: Backend' b => String -> Diagram b R2
--textrect t = rect (fromIntegral (length t) * 0.4) 0.7 # lc red
--text'' :: Backend' b => String -> Diagram b R2
--text'' t = text' t `atop` textrect t

-- | Variant of 'phantom' that forces the argument backend type.
phantom' :: Backend' b => Diagram b R2 -> Diagram b R2
phantom' = phantom
