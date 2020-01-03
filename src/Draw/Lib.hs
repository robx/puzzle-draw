{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Draw.Lib where

import Control.Arrow ((***))
import Diagrams.Prelude
import Draw.Font
import Graphics.SVGFonts.Text
  ( Mode (..),
    Spacing (..),
    TextOpts (..),
    textSVG',
  )

type Backend' b =
  ( V b ~ V2,
    N b ~ Double,
    Renderable (Path V2 Double) b,
    Backend b V2 Double
  )

-- | Vertical/horizontal stroked line of given length.
vline, hline :: Backend' b => Double -> Diagram b
vline n = strokeLine . fromVertices . map p2 $ [(0, 0), (0, n)]
hline n = strokeLine . fromVertices . map p2 $ [(0, 0), (n, 0)]

-- | Variant of 'hcat'' that spreads with distance @1@.
hcatSep ::
  (InSpace V2 Double a, Juxtaposable a, HasOrigin a, Monoid' a) =>
  Double ->
  [a] ->
  a
hcatSep s = hcat' with {_sep = s}

-- | Collapse the envelope to a point.
smash :: Backend' b => Diagram b -> Diagram b
smash = withEnvelope (pointDiagram origin :: D V2 Double)

-- | Helper to translate by a point given as @(Int, Int)@.
translatep :: (InSpace V2 Double t, Transformable t) => (Int, Int) -> t -> t
translatep = translate . r2i

-- | Convert pair of @Int@ to vector.
r2i :: (Int, Int) -> V2 Double
r2i = r2 . (fromIntegral *** fromIntegral)

-- | Convert pair of @Int@ to point.
p2i :: (Int, Int) -> P2 Double
p2i = p2 . (fromIntegral *** fromIntegral)

mirror :: (InSpace V2 Double t, Transformable t) => t -> t
mirror = reflectAbout (p2 (0, 0)) (direction $ r2 (1, -1))

-- | Interleave two lists.
interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave (x : xs) ys = x : interleave ys xs

magnitude :: V2 Double -> Double
magnitude = norm

-- | Spread diagrams evenly along the given vector.
spread :: Backend' b => V2 Double -> [Diagram b] -> Diagram b
spread v things = cat v . interleave (repeat (strut vgap)) $ things
  where
    ds = map (diameter v) things
    gap' = (magnitude v - sum ds) / fromIntegral (length things + 1)
    vgap = (gap' / magnitude v) *^ v

dmid :: (InSpace V2 Double a, Enveloped a) => V2 Double -> a -> Double
dmid u a = (dtop + dbot) / 2 - dbot
  where
    menv v = magnitude . envelopeV v
    dtop = menv u a
    dbot = menv ((-1) *^ u) a

-- | Place the second diagram to the right of the first, aligning both
-- vertically. The origin is the origin of the left diagram.
besidesL :: Backend' b => Diagram b -> Diagram b -> Diagram b
besidesL a b = a ||| strutX 0.5 ||| b'
  where
    b' = b # centerY # translate (dmid unitY a *^ unitY)

-- | Variant of 'besidesL' where the origin is that of the right diagram.
besidesR :: Backend' b => Diagram b -> Diagram b -> Diagram b
besidesR b a = b' ||| strutX 0.5 ||| a
  where
    b' = b # centerY # translate (dmid unitY a *^ unitY)

aboveT :: Backend' b => Diagram b -> Diagram b -> Diagram b
aboveT a b = a === strutY 0.5 === b'
  where
    b' = b # centerX # translate (dmid unitX a *^ unitX)

-- | @fit f a@ scales @a@ to fit into a square of size @f@.
fit :: (Transformable t, Enveloped t, InSpace V2 Double t) => Double -> t -> t
fit f a = scale (f / m) a where m = max (diameter unitX a) (diameter unitY a)

-- | @fitDown f a@ scales @a@ down to fit into a square of size $f$.
fitDown ::
  (Transformable t, Enveloped t, InSpace V2 Double t) => Double -> t -> t
fitDown f a = scale f' a
  where
    m = max (diameter unitX a) (diameter unitY a)
    f' = if m > f then f / m else 1

-- | Write text that is centered both vertically and horizontally and that
-- has an envelope. Sized such that single capital characters fit nicely
-- into a square of size @1@.
text'' ::
  Renderable (Path V2 Double) b => Font -> String -> QDiagram b V2 Double Any
text'' fnt t =
  stroke (textSVG' (TextOpts fnt INSIDE_H KERN False 1 1) t)
    # lwG 0
    # rfc black
    # scale 0.8
  where
    rfc :: (HasStyle a, InSpace V2 Double a) => Colour Double -> a -> a
    rfc = recommendFillColor

-- | Variant of 'phantom' that forces the argument backend type.
phantom' :: Backend' b => Diagram b -> Diagram b
phantom' = phantom
