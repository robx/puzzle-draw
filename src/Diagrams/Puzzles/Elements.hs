{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}

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

pearl :: (Renderable (Path R2) b, Backend b R2) =>
         MasyuPearl -> Diagram b R2
pearl m = circle 0.35 # lw 0.05 # fc (c m)
  where
    c MWhite = white
    c MBlack = black

smallPearl :: (Renderable (Path R2) b, Backend b R2) =>
              MasyuPearl -> Diagram b R2
smallPearl = scale 0.4 . pearl

-- | The up-right diagonal of a centered unit square.
ur :: Path R2
ur = fromVertices [p2 (-1/2,-1/2), p2 (1/2,1/2)]

-- | The down-right diagonal of a centered unit square.
dr :: Path R2
dr = fromVertices [p2 (1/2,-1/2), p2 (-1/2,1/2)]

-- | Both diagonals of a centered unit square.
cross :: Path R2
cross = ur <> dr

-- | Draw a cross.
drawCross :: Renderable (Path R2) b => Diagram b R2
drawCross = stroke cross # scale 0.8 # lw edgewidth

-- | Draw a Compass clue.
drawCompassClue :: (Renderable (Path R2) b, Backend b R2) =>
                   CompassC -> Diagram b R2
drawCompassClue (CC n e s w) = texts <> stroke cross # lw onepix
    where tx Nothing _ = mempty
          tx (Just x) v = text' (show x) # scale 0.5 # translate (r2 v)
          texts = mconcat . zipWith tx [n, e, s, w] $
                  [(0,f), (f,0), (0,-f), (-f,0)]
          f = 3/10

-- | Draw a thermometer, given by a list of bottom-left corners
-- of square cells.
thermo :: Renderable (Path R2) b => [P2] -> QDiagram b R2 Any
thermo vs@(v:_) = (bulb `atop` line) # col # translate (r2 (0.5, 0.5))
    where bulb = circle 0.4 # moveTo v
          line = strokeLocLine (fromVertices vs)
                 # lw 0.55 # lineCap LineCapSquare
          col = lc gr . fc gr
          gr = blend 0.6 white black
thermo [] = error "invalid empty thermometer"

-- | Draw a list of thermometers, given as lists of @(Int, Int)@ cell
-- coordinates.
drawThermos :: Renderable (Path R2) b => [Thermometer] -> QDiagram b R2 Any
drawThermos = mconcat . map (thermo . map p2i)

-- | @drawTight d t@ draws the tight-fit value @t@, using @d@ to
-- draw the components.
drawTight :: (Renderable (Path R2) b, Backend b R2) =>
             (a -> Diagram b R2) -> Tightfit a -> Diagram b R2
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

-- | Stack the given words, left-justified.
stackWords :: (Backend b R2, Renderable (Path R2) b) => [String] -> QDiagram b R2 Any
stackWords = vcat' with {_sep = 0.1} . scale 0.8 . map (alignL . text')

-- | Mark a word in a grid of letters.
drawMarkedWord :: (Renderable (Path R2) b) => MarkedWord -> QDiagram b R2 Any
drawMarkedWord (MW s e) = lw onepix . stroke $ expandTrail' with {_expandCap = LineCapRound} 0.4 t
    where t = fromVertices [p2i s, p2i e] # translate (r2 (1/2,1/2))

-- | Apply 'drawMarkedWord' to a list of words.
drawMarkedWords :: (Renderable (Path R2) b) => [MarkedWord] -> QDiagram b R2 Any
drawMarkedWords = mconcat . map drawMarkedWord

-- | Draw a slalom clue.
drawSlalomClue :: (Show a, Renderable (Path R2) b, Backend b R2) =>
                  a -> Diagram b R2
drawSlalomClue x = text' (show x) # scale 0.75
                   <> circle 0.4 # fc white # lw onepix

-- | Draw text. Shouldn't be more than two characters or so to fit a cell.
drawText :: (Backend b R2, Renderable (Path R2) b) => String -> QDiagram b R2 Any
drawText = text'

-- | Draw an @Int@.
drawInt :: (Renderable (Path R2) b, Backend b R2) =>
           Int -> Diagram b R2
drawInt s = drawText (show s)

-- | Draw a character.
drawChar :: (Renderable (Path R2) b, Backend b R2) =>
            Char -> Diagram b R2
drawChar c = drawText [c]

-- | Stack a list of words into a unit square. Scaled such that at least
-- three words will fit.
drawWords :: (Renderable (Path R2) b, Backend b R2) =>
             [String] -> Diagram b R2
drawWords ws = spread (-1.0 *^ unitY)
                      (map (centerXY . scale 0.4 . drawText) ws)
               # centerY

-- | Fit a line drawing into a unit square.
--   For example, a Curve Data clue.
drawCurve :: Renderable (Path R2) b => [Edge] -> Diagram b R2
drawCurve = lw onepix . fit 0.6 . centerXY . mconcat . map (stroke . edge)

-- | Draw a shadow in the style of Afternoon Skyscrapers.
drawShade :: Renderable (Path R2) b => Shade -> Diagram b R2
drawShade (Shade s w) = (if s then south else mempty) <>
                        (if w then west else mempty)
  where
    shape = translate (r2 (-1/2, -1/2)) . fromVertices . map p2 $
        [ (0, 0), (1/4, 1/4), (1, 1/4), (1, 0), (0, 0) ]
    south = strokeLocLoop shape # lw 0 # fc gray
    west = reflectAbout (p2 (0, 0)) (r2 (1, 1)) south

-- | Draws the digits of a tapa clue, ordered
--   left to right, top to bottom.
drawTapaClue :: (Backend b R2, Renderable (Path R2) b) =>
                TapaClue -> Diagram b R2
drawTapaClue (TapaClue [x]) = drawInt x
drawTapaClue (TapaClue xs)  = fit 0.8
                            . decoratePath (p (length xs))
                            . map drawInt
                            $ xs
  where
    p n = centerXY (p' n)
    p' 2 = p2 (-1/4, 1/4) ~~ p2 (1/4, -1/4)
    p' 3 = reflectX . rotateBy (1/6) $ triangle 0.8
    p' 4 = reflectX . rotateBy (3/8) $ square 0.7
    p' 1 = error "singleton clues handled separately"
    p' _ = error "invalid tapa clue"

drawPrimeDiag :: (Backend b R2, Renderable (Path R2) b) =>
                 PrimeDiag -> Diagram b R2
drawPrimeDiag (PrimeDiag d) = stroke p # lw (3 * onepix) # lc (blend 0.5 gray white)
  where
    p = case d of (False, False) -> mempty
                  (True,  False) -> ur
                  (False, True)  -> dr
                  (True,  True)  -> ur <> dr
