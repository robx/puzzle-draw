module Diagrams.TwoD.Puzzles.Puzzle where

import Diagrams.Prelude

import Diagrams.TwoD.Puzzles.Draw
import Data.Puzzles.Puzzle

import Data.Puzzles.Grid

drawLITS (PP ag _) = drawAreaGridG ag
drawLITSsol p@(PP ag sg) = drawAreaGrid ag `atop` drawShadedGrid sg

drawGeradeweg (PP ig _) = drawIntGrid ig
drawGeradewegsol p@(PP ig l) = drawIntClues ig `atop` drawDualEdges l # solstyle `atop` drawGrid ig
    where solstyle = lc (blend 0.8 black white)

drawFillomino (PP ig _) = drawIntGrid ig
drawFillominosol (PP _ sg) = drawIntGrid sg

drawExample p s = (p ||| strutX 1.0 ||| s) # bg white
