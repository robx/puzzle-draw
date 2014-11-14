{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Diagrams.Puzzles.Draw (
    PuzzleSol,
    RenderPuzzle,
    OutputChoice(..),
    draw,
    Unit(..),
    diagramWidth,
    toOutputWidth,
  ) where

import Diagrams.Prelude
import Diagrams.BoundingBox

import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Widths
import Diagrams.Puzzles.Code

type RenderPuzzle b p s = (p -> Diagram b R2, (p, s) -> Diagram b R2)

type PuzzleSol b = (Diagram b R2, Maybe (Diagram b R2))

data OutputChoice = DrawPuzzle | DrawSolution | DrawExample
    deriving Show

-- | Optionally render the puzzle, its solution, or a side-by-side
--   example with puzzle and solution.
draw :: Backend' b
     => Maybe (CodeDiagrams (Diagram b R2))
     -> PuzzleSol b -> OutputChoice -> Maybe (Diagram b R2)
draw mc (p, ms) = fmap (bg white) . d
  where
    fixup = alignPixel . border borderwidth
    addCode x = case mc of
        Nothing                        -> x
        Just (CodeDiagrams cleft ctop) -> 
            (x =!= strutY 0.5 =!= ctop)
             |!| strutX 0.5 |!| cleft
    (=!=) = beside unitY
    (|!|) = beside (negateV unitX)
    d DrawPuzzle   = fixup . addCode <$> Just p
    d DrawSolution = fixup . addCode <$> ms
    d DrawExample  = sideBySide <$> d DrawPuzzle <*> d DrawSolution
    sideBySide x y = x ||| strutX 2.0 ||| y

data Unit = Pixels | Points

cmtopoint :: Double -> Double
cmtopoint = (* 28.3464567)

diagramWidth :: Diagram b R2 -> Double
diagramWidth = fst . unr2 . boxExtents . boundingBox

toOutputWidth :: Unit -> Double -> Double
toOutputWidth u w = case u of Pixels -> fromIntegral wpix
                              Points -> wpt
  where
    wpix = round (gridresd * w) :: Int  -- grid square size 40px
    wpt = cmtopoint (0.8 * w)     -- grid square size 0.8cm

alignPixel :: Backend' b => Diagram b R2 -> Diagram b R2
alignPixel = scale (1/gridresd) . align' . scale gridresd
  where
    align' d = maybe id grow (getCorners $ boundingBox d) d
    grow (bl, tr) = mappend $ phantoml (nudge bl False) (nudge tr True)
    nudge p dir = let (px, py) = unp2 p in p2 (nudge' px dir, nudge' py dir)
    nudge' x True  = fromIntegral (ceiling (x - 0.5) :: Int) + 0.5
    nudge' x False = fromIntegral (floor   (x + 0.5) :: Int) - 0.5
    phantoml p q = phantom' $ p ~~ q

-- | Add a phantom border of the given width around a diagram.
border :: Backend' b => Double -> Diagram b R2 -> Diagram b R2
border w = extrudeEnvelope (w *^ unitX) . extrudeEnvelope (-w *^ unitX)
         . extrudeEnvelope (w *^ unitY) . extrudeEnvelope (-w *^ unitY)
