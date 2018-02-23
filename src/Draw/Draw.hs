{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Draw.Draw (
    PuzzleSol,
    RenderPuzzle(..),
    OutputChoice(..),
    draw,
    Unit(..),
    diagramWidth,
    toOutputWidth,
  ) where

import Diagrams.Prelude

import Draw.Lib
import Draw.Widths
import Draw.Code

data RenderPuzzle b p s =
    Render
        { puzzle :: p -> Diagram b
        , solution :: (p, s) -> Diagram b
        }

type PuzzleSol b = (Diagram b, Maybe (Diagram b))

data OutputChoice = DrawPuzzle | DrawSolution | DrawExample
    deriving Show

-- | Optionally render the puzzle, its solution, or a side-by-side
--   example with puzzle and solution.
draw :: Backend' b
     => Maybe (CodeDiagrams (Diagram b))
     -> PuzzleSol b -> OutputChoice -> Maybe (Diagram b)
draw mc (p, ms) = fmap (bg white) . d
  where
    fixup = alignPixel . border borderwidth
    addCode x = case mc of
        Nothing                              -> x
        Just (CodeDiagrams cleft ctop cover) ->
            ((cover <> x) =!= top ctop) |!| lft cleft
    (=!=) = beside unitY
    (|!|) = beside (negated unitX)
    top c = if isEmpty c then mempty else strutY 0.5 =!= c
    lft c = if isEmpty c then mempty else strutX 0.5 |!| c
    isEmpty c = diameter unitX c == 0
    d DrawPuzzle   = fixup . addCode <$> Just p
    d DrawSolution = fixup . addCode <$> ms
    d DrawExample  = sideBySide <$> d DrawPuzzle <*> d DrawSolution
    sideBySide x y = x ||| strutX 2.0 ||| y

data Unit = Pixels | Points

cmtopoint :: Double -> Double
cmtopoint = (* 28.3464567)

diagramWidth :: Backend' b => Diagram b -> Double
diagramWidth = fst . unr2 . boxExtents . boundingBox

toOutputWidth :: Unit -> Double -> Double
toOutputWidth u w = case u of Pixels -> fromIntegral wpix
                              Points -> wpt
  where
    wpix = round (gridresd * w) :: Int  -- grid square size 40px
    wpt = cmtopoint w     -- grid square size 1.0cm

alignPixel :: Backend' b => Diagram b -> Diagram b
alignPixel = scale (1/gridresd) . align' . scale gridresd
  where
    align' d = maybe id grow (getCorners $ boundingBox d) d
    grow (bl, tr) = mappend $ phantoml (nudge bl False) (nudge tr True)
    nudge p dir = let (px, py) = unp2 p in p2 (nudge' px dir, nudge' py dir)
    nudge' x True  = fromIntegral (ceiling (x - 0.5) :: Int) + 0.5
    nudge' x False = fromIntegral (floor   (x + 0.5) :: Int) - 0.5
    phantoml p q = phantom' $ p ~~ q

-- | Add a phantom border of the given width around a diagram.
border :: Backend' b => Double -> Diagram b -> Diagram b
border w = extrudeEnvelope (w *^ unitX) . extrudeEnvelope (-w *^ unitX)
         . extrudeEnvelope (w *^ unitY) . extrudeEnvelope (-w *^ unitY)
