{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Diagrams.Puzzles.Lib where

import Diagrams.Prelude

import Graphics.SVGFonts

import Control.Arrow ((***))

type Backend' b = (V b ~ V2, N b ~ Double,
                   Renderable (Path V2 Double) b, Backend b V2 Double)

-- | Vertical/horizontal stroked line of given length.
vline, hline :: Backend' b => Double -> Diagram b
vline n = strokeLine . fromVertices . map p2 $ [(0, 0), (0, n)]
hline n = strokeLine . fromVertices . map p2 $ [(0, 0), (n, 0)]

-- | Variant of 'hcat'' that spreads with distance @1@.
hcatsep :: (InSpace V2 Double a, Juxtaposable a, HasOrigin a, Monoid' a)
        => [a] -> a
hcatsep = hcat' with {_sep = 1}

-- | Variant of 'vcat'' that spreads with distance @1@,
-- and stacks towards the top.
vcatsep :: (InSpace V2 Double a, Juxtaposable a, HasOrigin a, Monoid' a)
        => [a] -> a
vcatsep = cat' (r2 (0,1)) with {_sep = 1}

-- | Collapse the envelope to a point.
smash :: Backend' b => Diagram b -> Diagram b
smash = withEnvelope (pointDiagram origin :: D R2)

-- | Helper to translate by a point given as @(Int, Int)@.
translatep :: (InSpace V2 Double t, Transformable t)
           => (Int, Int) -> t -> t
translatep = translate . r2i

-- | Convert pair of @Int@ to vector.
r2i :: (Int, Int) -> V2 Double
r2i = r2 . (fromIntegral *** fromIntegral)

-- | Convert pair of @Int@ to point.
p2i :: (Int, Int) -> P2 Double
p2i = p2 . (fromIntegral *** fromIntegral)

mirror :: (InSpace V2 Double t, Transformable t) => t -> t
mirror = reflectAbout (p2 (0, 0)) (r2 (1, -1))

-- | Interleave two lists.
interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave (x:xs) ys = x : interleave ys xs

magnitude :: V2 Double -> Double
magnitude = norm

-- | Spread diagrams evenly along the given vector.
spread :: Backend' b => V2 Double -> [Diagram b] -> Diagram b
spread v things = cat v . interleave (repeat (strut vgap)) $ things
    where ds = map (diameter v) things
          gap' = (magnitude v - sum ds) / fromIntegral (length things + 1)
          vgap = (gap' / magnitude v) *^ v

dmid ::  (InSpace V2 Double a, Enveloped a) => a -> Double
dmid a = (dtop + dbot) / 2 - dbot
    where menv v = magnitude . envelopeV v
          dtop = menv unitY a
          dbot = menv ((-1) *^ unitY) a

-- | Place the second diagram to the right of the first, aligning both
-- vertically. The origin is the origin of the left diagram.
besidesL :: (Backend' b, Semigroup m, Monoid m) =>
            QDiagram b V2 Double m -> QDiagram b V2 Double m -> QDiagram b V2 Double m
besidesL a b = a ||| strutX 0.5 ||| b'
    where b' = b # centerY # translate (dmid a *^ unitY)

-- | Variant of 'besidesL' where the origin is that of the right diagram.
besidesR :: (Backend' b, Semigroup m, Monoid m) =>
           QDiagram b V2 Double m -> QDiagram b V2 Double m -> QDiagram b V2 Double m
besidesR b a =  b' ||| strutX 0.5 ||| a
    where b' = b # centerY # translate (dmid a *^ unitY)

-- | @fit f a@ scales @a@ to fit into a square of size @f@.
fit :: (Transformable t, Enveloped t, InSpace V2 Double t) =>
       Double -> t -> t
fit f a = scale (f / m) a
    where m = max (diameter unitX a)
                  (diameter unitY a)

-- | Write text that is centered both vertically and horizontally and that
-- has an envelope. Sized such that single capital characters fit nicely
-- into a square of size @1@.
text' :: Backend' b => String -> Diagram b
text' t = stroke (textSVG' $ TextOpts t bit INSIDE_H KERN False 1 1)
          # lwG 0 # fc black # scale 0.8

-- text' t = text t # fontSize 0.8 # font "Helvetica" # translate (r2 (0.04, -0.07))
--          <> phantom' (textrect t)
--textrect :: Backend' b => String -> Diagram b R2
--textrect t = rect (fromIntegral (length t) * 0.4) 0.7 # lc red
--text'' :: Backend' b => String -> Diagram b R2
--text'' t = text' t `atop` textrect t

-- | Variant of 'phantom' that forces the argument backend type.
phantom' :: Backend' b => Diagram b -> Diagram b
phantom' = phantom
