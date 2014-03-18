{-# LANGUAGE FlexibleContexts #-}

module Diagrams.TwoD.Puzzles.Things where

import Diagrams.Prelude
import Diagrams.TwoD.Offset

import Data.Puzzles.Things
import Data.Puzzles.Grid

import Diagrams.TwoD.Puzzles.Lib
import Diagrams.TwoD.Puzzles.Widths
import Diagrams.TwoD.Puzzles.Grid

pearl :: (Renderable (Path R2) b, Backend b R2) =>
         MasyuPearl -> Diagram b R2
pearl MWhite = circle 0.35 # lw 0.05
pearl MBlack = pearl MWhite # fc black

ur :: Path R2
ur = fromVertices [p2 (-1/2,-1/2), p2 (1/2,1/2)]

dr :: Path R2
dr = fromVertices [p2 (1/2,-1/2), p2 (-1/2,1/2)]

cross :: Path R2
cross = ur <> dr

drawCross :: Renderable (Path R2) b => Diagram b R2
drawCross = stroke cross # scale 0.8 # lw edgewidth

drawCompassClue :: (Renderable (Path R2) b, Backend b R2) =>
                   CompassC -> Diagram b R2
drawCompassClue (CC n e s w) = texts <> stroke cross # lw onepix
    where tx Nothing _ = mempty
          tx (Just x) v = text' (show x) # scale 0.5 # translate (r2 v)
          texts = mconcat . zipWith tx [n, e, s, w] $
                  [(0,f), (f,0), (0,-f), (-f,0)]
          f = 3/10

thermo :: Renderable (Path R2) b => [P2] -> QDiagram b R2 Any
thermo vs@(v:_) = (bulb `atop` line) # col # translate (r2 (0.5, 0.5))
    where bulb = circle 0.4 # moveTo v
          line = strokeLocLine (fromVertices vs) # lw 0.55 # lineCap LineCapSquare
          col = lc gr . fc gr
          gr = blend 0.6 white black

drawThermos :: Renderable (Path R2) b => [Thermometer] -> QDiagram b R2 Any
drawThermos = mconcat . map (thermo . map p2i)

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

stackWords :: (Backend b R2, Renderable (Path R2) b) => [String] -> QDiagram b R2 Any
stackWords = vcat' with {_sep = 0.1} . scale 0.8 . map (alignL . text')

drawMarkedWord :: (Renderable (Path R2) b) => MarkedWord -> QDiagram b R2 Any
drawMarkedWord (MW s e) = lw onepix . stroke $ expandTrail' with {_expandCap = LineCapRound} 0.4 t
    where t = fromVertices [p2i s, p2i e] # translate (r2 (1/2,1/2))

drawMarkedWords :: (Renderable (Path R2) b) => [MarkedWord] -> QDiagram b R2 Any
drawMarkedWords = mconcat . map drawMarkedWord

drawSlalomClue :: (Show a, Renderable (Path R2) b, Backend b R2) =>
                  a -> Diagram b R2
drawSlalomClue x = text' (show x) # scale 0.75
                   <> circle 0.4 # fc white # lw onepix

drawText :: (Backend b R2, Renderable (Path R2) b) => String -> QDiagram b R2 Any
drawText = text'

drawInt :: (Renderable (Path R2) b, Backend b R2) =>
           Int -> Diagram b R2
drawInt s = drawText (show s)

drawChar :: (Renderable (Path R2) b, Backend b R2) =>
            Char -> Diagram b R2
drawChar c = drawText [c]

-- | Stack a list of words into a unit square.
drawWords :: (Renderable (Path R2) b, Backend b R2) =>
             [String] -> Diagram b R2
drawWords ws = spread (-1.0 *^ unitY)
                      (map (centerXY . scale 0.4 . drawText) ws)
               # centerY

-- | Fit a line drawing into a unit square.
--   For example, a Curve Data clue.
drawCurve :: Renderable (Path R2) b => [Edge] -> Diagram b R2
drawCurve = lw onepix . fit 0.6 . centerXY . mconcat . map (stroke . edge)
