{-# LANGUAGE FlexibleContexts #-}

module Diagrams.TwoD.Puzzles.Puzzle (
    PuzzleSol,
    RenderPuzzle,
    OutputChoice(..),
    draw,
    
    lits, litsplus, geradeweg, fillomino, masyu, nurikabe, latintapa,
    sudoku, thermosudoku, pyramid, kpyramid, slither,
    liarslither, tightfitskyscrapers, wordloop, wordsearch,
    curvedata, doubleback, slalom, compass
    ) where

import Diagrams.Prelude

import Diagrams.TwoD.Puzzles.Draw
import Diagrams.TwoD.Puzzles.Grid
import qualified Diagrams.TwoD.Puzzles.Pyramid as DPyr
import Diagrams.TwoD.Puzzles.Things
import Diagrams.TwoD.Puzzles.Lib

import Data.Puzzles.PuzzleTypes

import Data.Puzzles.Grid
import qualified Data.Puzzles.Pyramid as Pyr

import Control.Applicative (liftA2)

type PuzzleSol b = (Diagram b R2, Diagram b R2)
type RenderPuzzle b a = a -> PuzzleSol b

lits :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b LITS
lits = liftA2 (,)
    (drawAreaGridGray . pzl)
    (drawAreaGrid . pzl <> drawShadedGrid . sol)

litsplus :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b LITS
litsplus = lits

solstyle :: HasStyle a => a -> a
solstyle = lc (blend 0.8 black white)

geradeweg :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Geradeweg
geradeweg = liftA2 (,)
    (drawIntGrid . pzl)
    (drawIntClues . pzl
     <> solstyle . drawDualEdges . sol
     <> grid . size . pzl)

fillomino :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Fillomino
fillomino = liftA2 (,)
    (drawFillo . pzl)
    (drawFillo . sol)

masyu :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Masyu
masyu = liftA2 (,)
    (drawMasyuGrid . pzl)
    (solstyle . drawDualEdges . sol <> drawMasyuGrid . pzl)

nurikabe :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Nurikabe
nurikabe = liftA2 (,)
    (drawIntGrid . pzl)
    (drawIntGrid . pzl <> drawShadedGrid . sol)

latintapa :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b LatinTapa
latintapa = liftA2 (,)
    l
    (l <> atCentres drawChar . clues . sol)
    where l = (grid . size <> drawWordsClues) . pzl

sudoku :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Sudoku
sudoku = liftA2 (,)
    ((drawIntClues <> sudokugrid) . pzl)
    ((drawIntClues <> sudokugrid) . sol)

thermosudoku :: (Backend b R2, Renderable (Path R2) b) =>
    RenderPuzzle b ThermoSudoku
thermosudoku = liftA2 (,)
    ((drawIntClues . fst <> sudokugrid . fst <> drawThermos . snd) . pzl)
    (drawIntClues . sol <> sudokugrid . sol <> drawThermos . snd . pzl)

pyramid :: (Backend b R2, Renderable (Path R2) b) =>
    RenderPuzzle b Pyramid
pyramid = liftA2 (,)
    (DPyr.pyramid . pzl)
    (DPyr.pyramid . merge)
    where merge (PD p q) = Pyr.mergepyramids p q

kpyramid :: (Backend b R2, Renderable (Path R2) b) =>
    RenderPuzzle b RowKropkiPyramid
kpyramid = liftA2 (,)
    (DPyr.kpyramid . pzl)
    (DPyr.kpyramid . merge)
    where merge (PD p q) = Pyr.mergekpyramids p q

slither :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b SlitherLink
slither = liftA2 (,)
    (drawSlitherGrid . pzl)
    (drawSlitherGrid . pzl <> solstyle . drawEdges . sol)

liarslither :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b LiarSlitherLink
liarslither = liftA2 (,)
    (drawSlitherGrid . pzl)
    (solstyle . drawCrosses . snd . sol
     <> drawSlitherGrid . pzl
     <> solstyle . drawEdges . fst . sol)

tightfitskyscrapers :: (Backend b R2, Renderable (Path R2) b) =>
                       RenderPuzzle b TightfitSkyscrapers
tightfitskyscrapers = liftA2 (,)
    (atCentres drawInt . outsideclues . fst . pzl
     <> drawTightGrid (const mempty) . snd . pzl)
    (atCentres drawInt . outsideclues . fst . pzl
     <> drawTightGrid drawInt . sol)

wordgrid :: (Backend b R2, Renderable (Path R2) b) =>
            SGrid (Maybe Char) -> [String] -> Diagram b R2
wordgrid g ws = stackWords ws `besidesR` drawClueGrid g

wordloop :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Wordloop
wordloop = liftA2 (,)
    (uncurry wordgrid . pzl)
    (drawClueGrid . sol)

wordsearch :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Wordsearch
wordsearch = liftA2 (,)
    (uncurry wordgrid . pzl) 
    (solstyle . drawMarkedWords . snd . sol
     <> drawClueGrid . fst . sol)

curvedata :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b CurveData
curvedata = liftA2 (,)
    cd
    ((solstyle . drawDualEdges . sol) <> cd)
    where cd = (atCentres drawCurve . clues <> grid . size) . pzl

doubleback :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b DoubleBack
doubleback = liftA2 (,)
    (drawAreaGridGray . pzl)
    (solstyle . drawDualEdges . sol <> drawAreaGridGray . pzl)

slalom :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Slalom
slalom = liftA2 (,)
    (drawSlalomGrid . pzl)
    (drawSlalomGrid . pzl <> solstyle . drawSlalomDiags . sol)

compass :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b Compass
compass = liftA2 (,)
    (drawCompassGrid . pzl)
    (drawCompassClues . pzl <> drawAreaGridGray . sol)

data OutputChoice = DrawPuzzle | DrawSolution | DrawExample

-- | Combine a puzzle renderer with its input to optionally
--   render the puzzle, its solution, or a side-by-side
--   example with puzzle and solution.
draw :: (Backend b R2, Renderable (Path R2) b) =>
        PuzzleSol b -> OutputChoice -> Diagram b R2
draw (p, s) o = d o # bg white
    where d DrawPuzzle   = p
          d DrawSolution = s
          d DrawExample  = p ||| strutX 2.0 ||| s
