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

import Diagrams.Prelude hiding (Loop)

import Diagrams.TwoD.Puzzles.Draw
import Diagrams.TwoD.Puzzles.Grid
import qualified Diagrams.TwoD.Puzzles.Pyramid as DPyr
import Diagrams.TwoD.Puzzles.Things
import Diagrams.TwoD.Puzzles.Lib

import Data.Puzzles.Grid
import Data.Puzzles.GridShape (Edge)
import Data.Puzzles.Things
import qualified Data.Puzzles.Pyramid as Pyr

type PuzzleSol b = (Diagram b R2, Diagram b R2)
type RenderPuzzle b a = a -> PuzzleSol b

lits :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b (AreaGrid, ShadedGrid)
lits = liftA2 (,)
    (drawAreaGridGray . fst)
    (drawAreaGrid . fst <> drawShadedGrid . snd)

litsplus :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b (AreaGrid, ShadedGrid)
litsplus = lits

solstyle :: HasStyle a => a -> a
solstyle = lc (blend 0.8 black white)

geradeweg :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b (IntGrid, Loop)
geradeweg = liftA2 (,)
    (drawIntGrid . fst)
    (drawIntClues . fst
     <> solstyle . drawDualEdges . snd
     <> grid . size . fst)

fillomino :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b (IntGrid, IntGrid)
fillomino = liftA2 (,)
    (drawFillo . fst)
    (drawFillo . snd)

masyu :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b (SGrid (Clue MasyuPearl), Loop)
masyu = liftA2 (,)
    (drawMasyuGrid . fst)
    (solstyle . drawDualEdges . snd <> drawMasyuGrid . fst)

nurikabe :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b (IntGrid, ShadedGrid)
nurikabe = liftA2 (,)
    (drawIntGrid . fst)
    (drawIntGrid . fst <> drawShadedGrid . snd)

latintapa :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b (SGrid (Clue [String]), CharClueGrid)
latintapa = liftA2 (,)
    l
    (l <> atCentres drawChar . clues . snd)
    where l = (grid . size <> drawWordsClues) . fst

sudoku :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b (IntGrid, IntGrid)
sudoku = liftA2 (,)
    ((drawIntClues <> sudokugrid) . fst)
    ((drawIntClues <> sudokugrid) . snd)

thermosudoku :: (Backend b R2, Renderable (Path R2) b) =>
                RenderPuzzle b ((SGrid Int, [Thermometer]), IntGrid)
thermosudoku = liftA2 (,)
    ((drawInts . fst <> sudokugrid . fst <> drawThermos . snd) . fst)
    (drawIntClues . snd <> sudokugrid . snd <> drawThermos . snd . fst)

pyramid :: (Backend b R2, Renderable (Path R2) b) =>
    RenderPuzzle b (Pyr.Pyramid, Pyr.Pyramid)
pyramid = liftA2 (,)
    (DPyr.pyramid . fst)
    (DPyr.pyramid . merge)
    where merge (p, q) = Pyr.mergepyramids p q

kpyramid :: (Backend b R2, Renderable (Path R2) b) =>
    RenderPuzzle b (Pyr.RowKropkiPyramid, Pyr.Pyramid)
kpyramid = liftA2 (,)
    (DPyr.kpyramid . fst)
    (DPyr.kpyramid . merge)
    where merge (p, q) = Pyr.mergekpyramids p q

slither :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b (IntGrid, Loop)
slither = liftA2 (,)
    (drawSlitherGrid . fst)
    (drawSlitherGrid . fst <> solstyle . drawEdges . snd)

liarslither :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b (IntGrid, (Loop, SGrid Bool))
liarslither = liftA2 (,)
    (drawSlitherGrid . fst)
    (solstyle . drawCrosses . snd . snd
     <> drawSlitherGrid . fst
     <> solstyle . drawEdges . fst . snd)

tightfitskyscrapers :: (Backend b R2, Renderable (Path R2) b) =>
                       RenderPuzzle b ((OutsideClues (Maybe Int), SGrid (Tightfit ())),
                                       SGrid (Tightfit Int))
tightfitskyscrapers = liftA2 (,)
    (atCentres drawInt . outsideclues . fst . fst
     <> drawTightGrid (const mempty) . snd . fst)
    (atCentres drawInt . outsideclues . fst . fst
     <> drawTightGrid drawInt . snd)

wordgrid :: (Backend b R2, Renderable (Path R2) b) =>
            SGrid (Maybe Char) -> [String] -> Diagram b R2
wordgrid g ws = stackWords ws `besidesR` drawClueGrid g

wordloop :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b ((CharClueGrid, [String]), CharClueGrid)
wordloop = liftA2 (,)
    (uncurry wordgrid . fst)
    (drawClueGrid . snd)

wordsearch :: (Backend b R2, Renderable (Path R2) b) =>
              RenderPuzzle b ((CharClueGrid, [String]), (CharClueGrid, [MarkedWord]))
wordsearch = liftA2 (,)
    (uncurry wordgrid . fst) 
    (solstyle . drawMarkedWords . snd . snd
     <> drawClueGrid . fst . snd)

curvedata :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b (SGrid (Clue [Edge]), [Edge])
curvedata = liftA2 (,)
    cd
    ((solstyle . drawDualEdges . snd) <> cd)
    where cd = (atCentres drawCurve . clues <> grid . size) . fst

doubleback :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b (AreaGrid, Loop)
doubleback = liftA2 (,)
    (drawAreaGridGray . fst)
    (solstyle . drawDualEdges . snd <> drawAreaGridGray . fst)

slalom :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b (IntGrid, SGrid SlalomDiag)
slalom = liftA2 (,)
    (drawSlalomGrid . fst)
    (drawSlalomGrid . fst <> solstyle . drawSlalomDiags . snd)

compass :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b (SGrid (Clue CompassC), AreaGrid)
compass = liftA2 (,)
    (drawCompassGrid . fst)
    (drawCompassClues . fst <> drawAreaGridGray . snd)

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
