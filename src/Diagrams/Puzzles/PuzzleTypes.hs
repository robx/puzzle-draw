{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Diagrams.Puzzles.PuzzleTypes (
    lits, litsplus, geradeweg, fillomino, masyu, nurikabe, latintapa,
    sudoku, thermosudoku, pyramid, kpyramid, slither,
    liarslither, tightfitskyscrapers, wordloop, wordsearch,
    curvedata, doubleback, slalom, compass, boxof2or3,
    afternoonskyscrapers, countnumbers, tapa, japanesesums,
    coral, maximallengths, primeplace, labyrinth, bahnhof,
    cave
  ) where

import Diagrams.Prelude hiding (Loop, coral)

import Diagrams.Puzzles.PuzzleGrids
import Diagrams.Puzzles.Draw
import Diagrams.Puzzles.Grid
import qualified Diagrams.Puzzles.Pyramid as DPyr
import Diagrams.Puzzles.Elements
import Diagrams.Puzzles.Lib
import Diagrams.Puzzles.Widths

import Data.Puzzles.Grid
import Data.Puzzles.GridShape (Edge)
import Data.Puzzles.Elements
import qualified Data.Puzzles.Pyramid as Pyr

lits :: Backend' b => RenderPuzzle b AreaGrid ShadedGrid
lits = (,)
    drawAreaGridGray
    (drawAreaGrid . fst <> drawShadedGrid . snd)

litsplus :: Backend' b => RenderPuzzle b AreaGrid ShadedGrid
litsplus = lits

solstyle :: (HasStyle a, V a ~ R2) => a -> a
solstyle = lc (blend 0.8 black white) . lwG (3 * onepix)

geradeweg :: Backend' b => RenderPuzzle b IntGrid Loop
geradeweg = (,)
    drawIntGrid
    (drawIntClues . fst
     <> solstyle . drawDualEdges . snd
     <> grid . size . fst)

fillomino :: Backend' b => RenderPuzzle b IntGrid (SGrid Int)
fillomino = (,)
    (atCentres drawInt . clues <> dashedgrid . size)
    ((atCentres drawInt . values <> drawEdges . borders <> dashedgrid . size) . snd)

masyu :: Backend' b =>
         RenderPuzzle b (SGrid (Clue MasyuPearl)) Loop
masyu = (,)
    drawMasyuGrid
    (solstyle . drawDualEdges . snd <> drawMasyuGrid . fst)

nurikabe :: Backend' b =>
            RenderPuzzle b IntGrid ShadedGrid
nurikabe = (,)
    drawIntGrid
    (drawIntGrid . fst <> drawShadedGrid . snd)

latintapa :: Backend' b =>
             RenderPuzzle b (SGrid (Clue [String])) CharClueGrid
latintapa = (,)
    l
    (l . fst <> atCentres drawChar . clues . snd)
  where
    l = grid . size <> drawWordsClues

sudoku :: Backend' b =>
          RenderPuzzle b IntGrid IntGrid
sudoku = (,)
    (drawIntClues <> sudokugrid)
    ((drawIntClues <> sudokugrid) . snd)

thermosudoku :: Backend' b =>
                RenderPuzzle b (SGrid Int, [Thermometer]) IntGrid
thermosudoku = (,)
    (drawInts . fst <> sudokugrid . fst <> drawThermos . snd)
    (drawIntClues . snd <> sudokugrid . snd <> drawThermos . snd . fst)

pyramid :: Backend' b =>
    RenderPuzzle b Pyr.Pyramid Pyr.PyramidSol
pyramid = (,)
    DPyr.pyramid
    (DPyr.pyramid . merge)
  where
    merge (p, q) = Pyr.mergepyramidsol p q

kpyramid :: Backend' b =>
    RenderPuzzle b Pyr.RowKropkiPyramid Pyr.PyramidSol
kpyramid = (,)
    DPyr.kpyramid
    (DPyr.kpyramid . merge)
  where
    merge (p, q) = Pyr.mergekpyramidsol p q

slither :: Backend' b =>
           RenderPuzzle b IntGrid Loop
slither = (,)
    drawSlitherGrid
    (drawSlitherGrid . fst <> solstyle . drawEdges . snd)

liarslither :: Backend' b =>
               RenderPuzzle b IntGrid (Loop, SGrid Bool)
liarslither = (,)
    drawSlitherGrid
    (solstyle . drawCrosses . snd . snd
     <> drawSlitherGrid . fst
     <> solstyle . drawEdges . fst . snd)

tightfitskyscrapers :: Backend' b =>
                       RenderPuzzle b (OutsideClues (Maybe Int), SGrid (Tightfit ()))
                                      (SGrid (Tightfit Int))
tightfitskyscrapers = (,)
    (atCentres drawInt . outsideClues . fst
     <> drawTightGrid (const mempty) . snd)
    (atCentres drawInt . outsideClues . fst . fst
     <> drawTightGrid drawInt . snd)

wordgrid :: Backend' b =>
            SGrid (Maybe Char) -> [String] -> Diagram b R2
wordgrid g ws = stackWords ws `besidesR` drawClueGrid g

wordloop :: Backend' b =>
            RenderPuzzle b (CharClueGrid, [String]) CharClueGrid
wordloop = (,)
    (uncurry wordgrid)
    (drawClueGrid . snd)

wordsearch :: Backend' b =>
              RenderPuzzle b (CharClueGrid, [String]) (CharClueGrid, [MarkedWord])
wordsearch = (,)
    (uncurry wordgrid) 
    (solstyle . drawMarkedWords . snd . snd
     <> drawClueGrid . fst . snd)

curvedata :: Backend' b =>
             RenderPuzzle b (SGrid (Clue [Edge])) [Edge]
curvedata = (,)
    cd
    ((solstyle . drawDualEdges . snd) <> cd . fst)
  where
    cd = atCentres drawCurve . clues <> grid . size

doubleback :: Backend' b =>
              RenderPuzzle b AreaGrid Loop
doubleback = (,)
    drawAreaGridGray
    (solstyle . drawDualEdges . snd <> drawAreaGridGray . fst)

slalom :: Backend' b =>
          RenderPuzzle b IntGrid (SGrid SlalomDiag)
slalom = (,)
    drawSlalomGrid
    (drawSlalomGrid . fst <> solstyle . drawSlalomDiags . snd)

compass :: Backend' b =>
           RenderPuzzle b (SGrid (Clue CompassC)) AreaGrid
compass = (,)
    drawCompassGrid
    (drawCompassClues . fst <> drawAreaGridGray . snd)

boxof2or3 :: Backend' b =>
             RenderPuzzle b (SGrid MasyuPearl, [Edge]) ()
boxof2or3 = (,)
    (atCentres smallPearl . values . fst
     <> phantom' . grid . size . fst
     <> drawThinDualEdges . snd)
    (error "boxof2or3 solution not implemented")

afternoonskyscrapers :: Backend' b =>
                        RenderPuzzle b (SGrid Shade) IntGrid
afternoonskyscrapers = (,)
    (grid . size <> atCentres drawShade . values)
    (drawIntGrid . snd <> atCentres drawShade . values . fst)

countnumbers :: Backend' b =>
                        RenderPuzzle b AreaGrid IntGrid
countnumbers = (,)
    drawAreaGrid
    (drawIntGrid . snd <> drawAreaGrid . fst)

tapa :: Backend' b =>
        RenderPuzzle b (SGrid TapaClue) ShadedGrid
tapa = (,)
    tapaGrid
    (tapaGrid . fst <> drawShadedGrid . snd)
  where
    tapaGrid = atCentres drawTapaClue . values <> grid . size

japanesesums :: Backend' b =>
                RenderPuzzle b (OutsideClues [Int]) (SGrid (Either Black Int))
japanesesums = (,)
    outsideIntGrid
    (outsideIntGrid . fst <> japcells . snd)
  where
    japcells = atCentres japcell . values
    japcell (Left Black) = fillBG gray
    japcell (Right x) = drawInt x

coral :: Backend' b =>
          RenderPuzzle b (OutsideClues [String]) ShadedGrid
coral = (,)
    outsideGrid
    (outsideGrid . fst <> drawShadedGrid . snd)

maximallengths :: Backend' b =>
                  RenderPuzzle b (OutsideClues (Maybe Int)) Loop
maximallengths = (,)
    g
    (solstyle . drawDualEdges . snd <> g . fst)
  where
    g = atCentres drawInt . outsideClues
        <> grid . outsideSize

primeplace :: Backend' b =>
              RenderPuzzle b (SGrid PrimeDiag) (SGrid Int)
primeplace = (,)
    g
    (atCentres drawInt . values . snd <> g . fst)
  where
    g = irregularGrid <> atCentres drawPrimeDiag . values

labyrinth :: Backend' b =>
             RenderPuzzle b (SGrid (Clue Int), [Edge]) (SGrid (Clue Int))
labyrinth = (,)
    (atCentres drawInt . clues . fst <> g)
    (atCentres drawInt . clues . snd <> g . fst)
  where
    g = drawEdges . snd <> plaingrid . size . fst

bahnhof :: Backend' b =>
            RenderPuzzle b (SGrid (Maybe BahnhofClue)) [Edge]
bahnhof = (,)
    (atCentres drawBahnhofClue . clues <> grid . size)
    (atCentres drawBahnhofStation . clues . fst
     <> solstyle . drawDualEdges . snd
     <> grid . size . fst)
  where
    drawBahnhofStation = either drawInt (const mempty)

cave ::
    (Backend b R2, Renderable (Path R2) b) =>
    RenderPuzzle b (SGrid (Clue Int)) ShadedGrid
cave = (,)
    g
    (drawEdges . edgesGen (/=) not . snd
     <> drawShadedGrid . snd <> fr . fst
     <> g . fst)
  where
    g = gridDashing . plaingrid . size <> atCentres drawInt . clues
    fr gr = outframe' 8 (size gr) # lc gray
