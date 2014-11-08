{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Module: Diagrams.TwoD.Puzzles.Elements
--
-- Tools to draw individual puzzle components. In particular
-- contents and decorations for individual cells.

module Diagrams.Puzzles.Elements where

import Diagrams.Prelude
import Diagrams.TwoD.Offset

import Data.Puzzles.Elements
import Data.Puzzles.GridShape

import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Widths
import Diagrams.Puzzles.Grid

pearl :: Backend' b =>
         MasyuPearl -> Diagram b
pearl m = circle 0.35 # lwG 0.05 # fc (c m)
  where
    c MWhite = white
    c MBlack = black

smallPearl :: Backend' b =>
              MasyuPearl -> Diagram b
smallPearl = scale 0.4 . pearl

-- | The up-right diagonal of a centered unit square.
ur :: Path V2 Double
ur = fromVertices [p2 (-1/2,-1/2), p2 (1/2,1/2)]

-- | The down-right diagonal of a centered unit square.
dr :: Path V2 Double
dr = fromVertices [p2 (1/2,-1/2), p2 (-1/2,1/2)]

-- | Both diagonals of a centered unit square.
cross :: Path V2 Double
cross = ur <> dr

-- | Draw a cross.
drawCross :: Backend' b => Diagram b
drawCross = stroke cross # scale 0.8 # lwG edgewidth

-- | Draw a Compass clue.
drawCompassClue :: Backend' b =>
                   CompassC -> Diagram b
drawCompassClue (CC n e s w) = texts <> stroke cross # lwG onepix
    where tx Nothing _ = mempty
          tx (Just x) v = text' (show x) # scale 0.5 # translate (r2 v)
          texts = mconcat . zipWith tx [n, e, s, w] $
                  [(0,f), (f,0), (0,-f), (-f,0)]
          f = 3/10

-- | Draw a thermometer, given by a list of bottom-left corners
-- of square cells.
thermo :: Backend' b => [P2 Double] -> Diagram b
thermo vs@(v:_) = (bulb `atop` line) # col # translate (r2 (0.5, 0.5))
    where bulb = circle 0.4 # moveTo v
          line = strokeLocLine (fromVertices vs)
                 # lwG 0.55 # lineCap LineCapSquare
          col = lc gr . fc gr
          gr = blend 0.6 white black
thermo [] = error "invalid empty thermometer"

-- | Draw a list of thermometers, given as lists of @(Int, Int)@ cell
-- coordinates.
drawThermos :: Backend' b => [Thermometer] -> Diagram b
drawThermos = mconcat . map (thermo . map p2i)

-- | @drawTight d t@ draws the tight-fit value @t@, using @d@ to
-- draw the components.
drawTight :: Backend' b =>
             (a -> Diagram b) -> Tightfit a -> Diagram b
drawTight d (Single x) = d x
drawTight d (UR x y) = stroke ur # lwG onepix
                       <> d x # scale s # translate (r2 (-t,t))
                       <> d y # scale s # translate (r2 (t,-t))
    where t = 1/5
          s = 2/3
drawTight d (DR x y) = stroke dr # lwG onepix
                       <> d x # scale s # translate (r2 (-t,-t))
                       <> d y # scale s # translate (r2 (t,t))
    where t = 1/5
          s = 2/3

-- | Stack the given words, left-justified.
stackWords :: Backend' b => [String] -> Diagram b
stackWords = vcat' with {_sep = 0.1} . scale 0.8 . map (alignL . text')

-- | Mark a word in a grid of letters.
drawMarkedWord :: Backend' b => MarkedWord -> Diagram b
drawMarkedWord (MW s e) = lwG onepix . stroke
                        $ expandTrail' with {_expandCap = LineCapRound} 0.4 t
    where t = fromVertices [p2i s, p2i e] # translate (r2 (1/2,1/2))

-- | Apply 'drawMarkedWord' to a list of words.
drawMarkedWords :: Backend' b => [MarkedWord] -> Diagram b
drawMarkedWords = mconcat . map drawMarkedWord

-- | Draw a slalom clue.
drawSlalomClue :: (Show a, Backend' b) =>
                  a -> Diagram b
drawSlalomClue x = text' (show x) # scale 0.75
                   <> circle 0.4 # fc white # lwG onepix

-- | Draw text. Shouldn't be more than two characters or so to fit a cell.
drawText :: Backend' b => String -> Diagram b
drawText = text'

-- | Draw an @Int@.
drawInt :: Backend' b =>
           Int -> Diagram b
drawInt s = drawText (show s)

-- | Draw a character.
drawChar :: Backend' b =>
            Char -> Diagram b
drawChar c = drawText [c]

-- | Stack a list of words into a unit square. Scaled such that at least
-- three words will fit.
drawWords :: Backend' b =>
             [String] -> Diagram b
drawWords ws = spread (-1.0 *^ unitY)
                      (map (centerXY . scale 0.4 . drawText) ws)
               # centerY

-- | Fit a line drawing into a unit square.
--   For example, a Curve Data clue.
drawCurve :: Backend' b => [Edge] -> Diagram b
drawCurve = lwG onepix . fit 0.6 . centerXY . mconcat . map (stroke . edge)

-- | Draw a shadow in the style of Afternoon Skyscrapers.
drawShade :: Backend' b => Shade -> Diagram b
drawShade (Shade s w) = (if s then south else mempty) <>
                        (if w then west else mempty)
  where
    shape = translate (r2 (-1/2, -1/2)) . fromVertices . map p2 $
        [ (0, 0), (1/4, 1/4), (1, 1/4), (1, 0), (0, 0) ]
    south = strokeLocLoop shape # lwG 0 # fc gray
    west = reflectAbout (p2 (0, 0)) (r2 (1, 1)) south

-- | Draws the digits of a tapa clue, ordered
--   left to right, top to bottom.
drawTapaClue :: Backend' b =>
                TapaClue -> Diagram b
drawTapaClue (TapaClue [x]) = drawInt x
drawTapaClue (TapaClue xs)  = fit 0.8
                            . atPoints (p (length xs))
                            . map drawInt
                            $ xs
  where
    p n = mconcat . pathVertices $ centerXY (p' n)
    p' 2 = p2 (-1/4, 1/4) ~~ p2 (1/4, -1/4)
    p' 3 = reflectX . rotateBy (1/6) $ triangle 0.8
    p' 4 = reflectX . rotateBy (3/8) $ square 0.7
    p' 1 = error "singleton clues handled separately"
    p' _ = error "invalid tapa clue"

drawPrimeDiag :: Backend' b =>
                 PrimeDiag -> Diagram b
drawPrimeDiag (PrimeDiag d) = stroke p # lwG (3 * onepix) # lc (blend 0.5 gray white)
  where
    p = case d of (False, False) -> mempty
                  (True,  False) -> ur
                  (False, True)  -> dr
                  (True,  True)  -> ur <> dr
