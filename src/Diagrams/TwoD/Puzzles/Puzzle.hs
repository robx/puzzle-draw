{-# LANGUAGE FlexibleContexts #-}

module Diagrams.TwoD.Puzzles.Puzzle (
    PuzzleSol,
    RenderPuzzle,
    OutputChoice(..),
    draw,
    
    lits, litsplus, geradeweg, fillomino, masyu, nurikabe, latintapa,
    sudoku, thermosudoku, pyramid, kpyramid, slither,
    liarslither, tightfitskyscrapers, wordloop, wordsearch,
    curvedata, doubleback, slalom, compass, boxof2or3,
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
type RenderPuzzle b p s = (p -> Diagram b R2, (p, s) -> Diagram b R2)

lits :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b AreaGrid ShadedGrid
lits = (,)
    drawAreaGridGray
    (drawAreaGrid . fst <> drawShadedGrid . snd)

litsplus :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b AreaGrid ShadedGrid
litsplus = lits

solstyle :: HasStyle a => a -> a
solstyle = lc (blend 0.8 black white)

geradeweg :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b IntGrid Loop
geradeweg = (,)
    drawIntGrid
    (drawIntClues . fst
     <> solstyle . drawDualEdges . snd
     <> grid . size . fst)

fillomino :: (Backend b R2, Renderable (Path R2) b) => RenderPuzzle b IntGrid IntGrid
fillomino = (,)
    drawFillo
    (drawFillo . snd)

masyu :: (Backend b R2, Renderable (Path R2) b) =>
         RenderPuzzle b (SGrid (Clue MasyuPearl)) Loop
masyu = (,)
    drawMasyuGrid
    (solstyle . drawDualEdges . snd <> drawMasyuGrid . fst)

nurikabe :: (Backend b R2, Renderable (Path R2) b) =>
            RenderPuzzle b IntGrid ShadedGrid
nurikabe = (,)
    drawIntGrid
    (drawIntGrid . fst <> drawShadedGrid . snd)

latintapa :: (Backend b R2, Renderable (Path R2) b) =>
             RenderPuzzle b (SGrid (Clue [String])) CharClueGrid
latintapa = (,)
    l
    (l . fst <> atCentres drawChar . clues . snd)
  where
    l = grid . size <> drawWordsClues

sudoku :: (Backend b R2, Renderable (Path R2) b) =>
          RenderPuzzle b IntGrid IntGrid
sudoku = (,)
    (drawIntClues <> sudokugrid)
    ((drawIntClues <> sudokugrid) . snd)

thermosudoku :: (Backend b R2, Renderable (Path R2) b) =>
                RenderPuzzle b (SGrid Int, [Thermometer]) IntGrid
thermosudoku = (,)
    (drawInts . fst <> sudokugrid . fst <> drawThermos . snd)
    (drawIntClues . snd <> sudokugrid . snd <> drawThermos . snd . fst)

pyramid :: (Backend b R2, Renderable (Path R2) b) =>
    RenderPuzzle b Pyr.Pyramid Pyr.Pyramid
pyramid = (,)
    DPyr.pyramid
    (DPyr.pyramid . merge)
  where
    merge (p, q) = Pyr.mergepyramids p q

kpyramid :: (Backend b R2, Renderable (Path R2) b) =>
    RenderPuzzle b Pyr.RowKropkiPyramid Pyr.Pyramid
kpyramid = (,)
    DPyr.kpyramid
    (DPyr.kpyramid . merge)
  where
    merge (p, q) = Pyr.mergekpyramids p q

slither :: (Backend b R2, Renderable (Path R2) b) =>
           RenderPuzzle b IntGrid Loop
slither = (,)
    drawSlitherGrid
    (drawSlitherGrid . fst <> solstyle . drawEdges . snd)

liarslither :: (Backend b R2, Renderable (Path R2) b) =>
               RenderPuzzle b IntGrid (Loop, SGrid Bool)
liarslither = (,)
    drawSlitherGrid
    (solstyle . drawCrosses . snd . snd
     <> drawSlitherGrid . fst
     <> solstyle . drawEdges . fst . snd)

tightfitskyscrapers :: (Backend b R2, Renderable (Path R2) b) =>
                       RenderPuzzle b (OutsideClues (Maybe Int), SGrid (Tightfit ()))
                                      (SGrid (Tightfit Int))
tightfitskyscrapers = (,)
    (atCentres drawInt . outsideclues . fst
     <> drawTightGrid (const mempty) . snd)
    (atCentres drawInt . outsideclues . fst . fst
     <> drawTightGrid drawInt . snd)

wordgrid :: (Backend b R2, Renderable (Path R2) b) =>
            SGrid (Maybe Char) -> [String] -> Diagram b R2
wordgrid g ws = stackWords ws `besidesR` drawClueGrid g

wordloop :: (Backend b R2, Renderable (Path R2) b) =>
            RenderPuzzle b (CharClueGrid, [String]) CharClueGrid
wordloop = (,)
    (uncurry wordgrid)
    (drawClueGrid . snd)

wordsearch :: (Backend b R2, Renderable (Path R2) b) =>
              RenderPuzzle b (CharClueGrid, [String]) (CharClueGrid, [MarkedWord])
wordsearch = (,)
    (uncurry wordgrid) 
    (solstyle . drawMarkedWords . snd . snd
     <> drawClueGrid . fst . snd)

curvedata :: (Backend b R2, Renderable (Path R2) b) =>
             RenderPuzzle b (SGrid (Clue [Edge])) [Edge]
curvedata = (,)
    cd
    ((solstyle . drawDualEdges . snd) <> cd . fst)
    where cd = (atCentres drawCurve . clues <> grid . size)

doubleback :: (Backend b R2, Renderable (Path R2) b) =>
              RenderPuzzle b AreaGrid Loop
doubleback = (,)
    drawAreaGridGray
    (solstyle . drawDualEdges . snd <> drawAreaGridGray . fst)

slalom :: (Backend b R2, Renderable (Path R2) b) =>
          RenderPuzzle b IntGrid (SGrid SlalomDiag)
slalom = (,)
    drawSlalomGrid
    (drawSlalomGrid . fst <> solstyle . drawSlalomDiags . snd)

compass :: (Backend b R2, Renderable (Path R2) b) =>
           RenderPuzzle b (SGrid (Clue CompassC)) AreaGrid
compass = (,)
    drawCompassGrid
    (drawCompassClues . fst <> drawAreaGridGray . snd)

boxof2or3 :: (Backend b R2, Renderable (Path R2) b) =>
             RenderPuzzle b (SGrid MasyuPearl, [Edge]) ()
boxof2or3 = (,)
    (atCentres pearl . values . fst <> drawDualEdges . snd)
    (error "boxof2or3 solution not implemented")

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
