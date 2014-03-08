module Diagrams.TwoD.Puzzles.Puzzle where

import Diagrams.Prelude

import Diagrams.TwoD.Puzzles.Draw
import Diagrams.TwoD.Puzzles.Pyramid
import Diagrams.TwoD.Puzzles.Things
import Diagrams.TwoD.Puzzles.Lib

import Data.Puzzles.ReadPuzzle

import Data.Puzzles.Grid
import Data.Puzzles.Pyramid

drawLITS (PP ag _) = drawAreaGridG ag
drawLITSsol p@(PP ag sg) = drawAreaGrid ag `atop` drawShadedGrid sg

solstyle :: HasStyle a => a -> a
solstyle = lc (blend 0.8 black white)

drawGeradeweg (PP ig _) = drawIntGrid ig
drawGeradewegsol p@(PP ig l) = drawIntClues ig `atop` drawDualEdges l # solstyle `atop` drawGrid ig

drawFillomino (PP ig _) = drawFillo ig
drawFillominosol (PP _ sg) = drawFillo sg

-- drawMasyu :: Masyu -> QDiagram b R2 Any
drawMasyu (PP mg _) = drawMasyuGrid mg
drawMasyusol p@(PP mg l) = drawDualEdges l # solstyle `atop` drawMasyu p

drawNurikabe (PP ig _) = drawIntGrid ig
drawNurikabesol p@(PP _ sg) = drawNurikabe p `atop` drawShadedGrid sg

drawLatinTapa (PP cg _) = drawGrid cg <> drawWordsClues cg
drawLatinTapasol p@(PP _ sg) = drawLatinTapa p <> atCentres drawChar (clues sg)

drawSudoku (PP ig _) = drawIntClues ig <> sudokugrid ig
drawSudokusol (PP _ sg) = drawIntClues sg <> sudokugrid sg 

drawThermoSudoku (PP (ig, ts) _) = drawIntClues ig <> sudokugrid ig <> drawThermos ts
drawThermoSudokusol (PP (_, ts) sg) = drawIntClues sg <> sudokugrid sg <> drawThermos ts

drawPyramid (PP p _) = pyramid p
drawPyramidsol (PP p q) = pyramid (mergepyramids p q)

drawKropkiPyramid (PP p _) = kpyramid p
drawKropkiPyramidsol (PP p q) = kpyramid (mergekpyramids p q)

drawSlither (PP ig _) = drawSlitherGrid ig
drawSlithersol p@(PP _ l) = drawSlither p <> drawedges l # solstyle

drawLiarSlither (PP ig _) = drawSlitherGrid ig
drawLiarSlithersol p@(PP _ (l, cs)) = drawCrosses cs # solstyle 
                                      <> drawSlither p
                                      <> drawedges l # solstyle

drawTightfitSkyscraper (PP (o, g) _) = atCentres drawInt (clueso o)
                                       <> drawTightGrid (const mempty) g
drawTightfitSkyscrapersol (PP (o, _) s) = atCentres drawInt (clueso o)
                                          <> drawTightGrid drawInt s

drawWordloop (PP (g, ws) _) = stackWords ws `besidesR` drawClueGrid g
drawWordloopsol (PP _ s) = drawClueGrid s

drawWordsearch (PP (g, ws) _) = stackWords ws `besidesR` drawClueGrid g
drawWordsearchsol (PP _ (g, mw)) = drawMarkedWords mw # solstyle <> drawClueGrid g

drawCurveData (PP g _) = atCentres drawCurve (clues g) <> drawGrid g
drawCurveDatasol p@(PP _ es) = drawedges es # solstyle # translate (r2 (1/2,1/2)) <> drawCurveData p

drawDoubleBack (PP g _) = drawAreaGridG g
drawDoubleBacksol p@(PP _ l) = drawDualEdges l # solstyle <> drawDoubleBack p

drawSlalom (PP g _) = drawSlalomGrid g
drawSlalomsol p@(PP _ s) = drawSlalom p <> drawSlalomDiags s # solstyle

drawCompass (PP g _) = drawCompassGrid g
drawCompasssol (PP g s) = drawCompassClues g <> drawAreaGridG s

data OutputChoice = DrawPuzzle | DrawSolution | DrawExample

type PuzzleSol b = (Diagram b R2, Diagram b R2)

--draw :: PuzzleSol -> OutputChoice -> Diagram B R2
draw (p, s) DrawPuzzle = p # bg white
draw (p, s) DrawSolution = s # bg white
draw (p, s) DrawExample = (p ||| strutX 2.0 ||| s) # bg white
