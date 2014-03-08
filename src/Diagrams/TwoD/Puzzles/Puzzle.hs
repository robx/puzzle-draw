{-# LANGUAGE FlexibleContexts #-}

module Diagrams.TwoD.Puzzles.Puzzle where

import Diagrams.Prelude

import Diagrams.TwoD.Puzzles.Draw
import Diagrams.TwoD.Puzzles.Grid
import qualified Diagrams.TwoD.Puzzles.Pyramid as Pyr
import Diagrams.TwoD.Puzzles.Things
import Diagrams.TwoD.Puzzles.Lib

import Data.Puzzles.ReadPuzzle hiding (puzzle, solution)

import Data.Puzzles.Grid
import Data.Puzzles.Pyramid

data RenderPuzzle b a = RenderPuzzle
                        { puzzle   :: a -> Diagram b R2
                        , solution :: a -> Diagram b R2
                        }

lits :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b LITS
lits = RenderPuzzle
       (drawAreaGridG . pzl)
       (drawAreaGrid . pzl <> drawShadedGrid . sol)

solstyle :: HasStyle a => a -> a
solstyle = lc (blend 0.8 black white)

geradeweg :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Geradeweg
geradeweg = RenderPuzzle
     (drawIntGrid . pzl)
     (drawIntClues . pzl
      <> solstyle . drawDualEdges . sol
      <> drawGrid . pzl)

fillomino :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Fillomino
fillomino = RenderPuzzle (drawFillo . pzl) (drawFillo . sol)

masyu :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Masyu
masyu = RenderPuzzle (drawMasyuGrid . pzl)
                     (solstyle . drawDualEdges . sol <> drawMasyuGrid . pzl)

nurikabe :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Nurikabe
nurikabe = RenderPuzzle (drawIntGrid . pzl)
                        (drawIntGrid . pzl <> drawShadedGrid . sol)

latintapa :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b LatinTapa
latintapa = RenderPuzzle
            l
            (l <> atCentres drawChar . clues . sol)
    where l = (drawGrid <> drawWordsClues) . pzl

sudoku :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Sudoku
sudoku = RenderPuzzle
         ((drawIntClues <> sudokugrid) . pzl)
         ((drawIntClues <> sudokugrid) . sol)

thermosudoku :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b ThermoSudoku
thermosudoku = RenderPuzzle
               ((drawIntClues . fst <> sudokugrid . fst <> drawThermos . snd) . pzl)
               (drawIntClues . sol <> sudokugrid . sol <> drawThermos . snd . pzl)

pyramid :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b PPyramid
pyramid = RenderPuzzle
          (Pyr.pyramid . pzl)
          (Pyr.pyramid . merge)
    where merge (PP p q) = mergepyramids p q

kpyramid :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b PKropkiPyramid
kpyramid = RenderPuzzle
          (Pyr.kpyramid . pzl)
          (Pyr.kpyramid . merge)
    where merge (PP p q) = mergekpyramids p q

slither :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b SlitherLink
slither = RenderPuzzle
          (drawSlitherGrid . pzl)
          (drawSlitherGrid . pzl <> solstyle . drawedges . sol)

liarslither :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b LiarSlitherLink
liarslither = RenderPuzzle
              (drawSlitherGrid . pzl)
              (solstyle . drawCrosses . snd . sol
               <> drawSlitherGrid . pzl
               <> solstyle . drawedges . fst . sol)

tightfitskyscrapers :: (Backend b R2, Renderable (Path R2) b) =>
                       RenderPuzzle b TightfitSkyscrapers
tightfitskyscrapers = RenderPuzzle
    (atCentres drawInt . clueso . fst . pzl
     <> drawTightGrid (const mempty) . snd . pzl)
    (atCentres drawInt . clueso . fst . pzl
     <> drawTightGrid drawInt . sol)

wordgrid :: (Backend b R2, Renderable (Path R2) b) =>
            Grid (Maybe Char) -> [String] -> Diagram b R2
wordgrid g ws = stackWords ws `besidesR` drawClueGrid g

wordloop :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Wordloop
wordloop = RenderPuzzle (uncurry wordgrid . pzl) (drawClueGrid . sol)

wordsearch :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Wordsearch
wordsearch = RenderPuzzle
             (uncurry wordgrid . pzl) 
             (solstyle . drawMarkedWords . snd . sol
              <> drawClueGrid . fst . sol)

curvedata :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b CurveData
curvedata = RenderPuzzle
            cd
            ((solstyle . drawDualEdges . sol) <> cd)
     where cd = (atCentres drawCurve . clues <> drawGrid) . pzl

doubleback :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b DoubleBack
doubleback = RenderPuzzle
             (drawAreaGridG . pzl)
             (solstyle . drawDualEdges . sol <> drawAreaGridG . pzl)

slalom :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Slalom
slalom = RenderPuzzle
         (drawSlalomGrid . pzl)
         (drawSlalomGrid . pzl <> solstyle . drawSlalomDiags . sol)

compass :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Compass
compass = RenderPuzzle
          (drawCompassGrid . pzl)
          (drawCompassClues . pzl <> drawAreaGridG . sol)

data OutputChoice = DrawPuzzle | DrawSolution | DrawExample

draw :: (Backend b R2, Renderable (Path R2) b) =>
        RenderPuzzle b a -> a -> OutputChoice -> Diagram b R2
draw rp p DrawPuzzle   = puzzle rp p # bg white
draw rp p DrawSolution = solution rp p # bg white
draw rp p DrawExample  = (puzzle rp p
                          ||| strutX 2.0
                          ||| solution rp p) # bg white
