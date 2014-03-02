module Diagrams.TwoD.Puzzles.Puzzle where

import Diagrams.Prelude

import Diagrams.TwoD.Puzzles.Draw
import Data.Puzzles.Puzzle

import Data.Puzzles.Grid

drawLITS (PP ag _) = drawAreaGridG ag
drawLITSsol p@(PP ag sg) = drawAreaGrid ag `atop` drawShadedGrid sg

solstyle :: HasStyle a => a -> a
solstyle = lc (blend 0.8 black white)

drawGeradeweg (PP ig _) = drawIntGrid ig
drawGeradewegsol p@(PP ig l) = drawIntClues ig `atop` drawDualEdges l # solstyle `atop` drawGrid ig

drawFillomino (PP ig _) = drawIntGrid ig
drawFillominosol (PP _ sg) = drawIntGrid sg

-- drawMasyu :: Masyu -> QDiagram b R2 Any
drawMasyu (PP mg _) = drawMasyuGrid mg
drawMasyusol p@(PP mg l) = drawDualEdges l # solstyle `atop` drawMasyu p

drawNurikabe (PP ig _) = drawIntGrid ig
drawNurikabesol p@(PP _ sg) = drawNurikabe p `atop` drawShadedGrid sg

data OutputChoice = DrawPuzzle | DrawSolution | DrawExample

type PuzzleSol b = (Diagram b R2, Diagram b R2)

--draw :: PuzzleSol -> OutputChoice -> Diagram B R2
draw (p, s) DrawPuzzle = p # bg white
draw (p, s) DrawSolution = s # bg white
draw (p, s) DrawExample = (p ||| strutX 1.0 ||| s) # bg white
