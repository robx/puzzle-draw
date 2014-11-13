
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Diagrams.Puzzles.PuzzleTypes (
    lits, litsplus, geradeweg, fillomino, masyu, nurikabe, latintapa,
    sudoku, thermosudoku, pyramid, kpyramid, slither,
    liarslither, tightfitskyscrapers, wordloop, wordsearch,
    curvedata, doubleback, slalom, compass, boxof2or3,
    afternoonskyscrapers, meanderingnumbers, tapa, japanesesums,
    coral, maximallengths, primeplace, labyrinth, bahnhof,
    cave
  ) where

import Diagrams.Prelude hiding (Loop, coral)

import Diagrams.Puzzles.Style
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
    (drawAreas <> grid gDefault)
    ((drawAreas <> grid gDefault) . fst <> drawShade . snd)

litsplus :: Backend' b => RenderPuzzle b AreaGrid ShadedGrid
litsplus = lits

solstyle :: (HasStyle a, V a ~ R2) => a -> a
solstyle = lc (blend 0.8 black white) . lwG (3 * onepix)

geradeweg :: Backend' b => RenderPuzzle b IntGrid Loop
geradeweg = (,)
    drawIntGrid
    (drawIntClues . fst
     <> solstyle . drawDualEdges . snd
     <> grid gDefault . fst)

fillomino :: Backend' b => RenderPuzzle b IntGrid (SGrid Int)
fillomino = (,)
    (atCentres drawInt . clues <> grid gDashed)
    ((atCentres drawInt . values <> drawEdges . borders <> grid gDashed) . snd)

masyu :: Backend' b =>
         RenderPuzzle b (SGrid (Clue MasyuPearl)) Loop
masyu = (,)
    drawMasyuGrid
    (solstyle . drawDualEdges . snd <> drawMasyuGrid . fst)

nurikabe :: Backend' b =>
            RenderPuzzle b IntGrid ShadedGrid
nurikabe = (,)
    drawIntGrid
    (drawIntGrid . fst <> drawShade . snd)

latintapa :: Backend' b =>
             RenderPuzzle b (SGrid (Clue [String])) CharClueGrid
latintapa = (,)
    l
    (l . fst <> atCentres drawChar . clues . snd)
  where
    l = grid gDefault <> drawWordsClues

sudoku :: Backend' b =>
          RenderPuzzle b IntGrid IntGrid
sudoku = (,)
    (drawIntClues <> sudokugrid)
    ((drawIntClues <> sudokugrid) . snd)

thermosudoku :: Backend' b =>
                RenderPuzzle b (SGrid (Maybe Int), [Thermometer]) (SGrid (Maybe Int))
thermosudoku = (,)
    (atCentres drawInt . clues . fst <> sudokugrid . fst <> drawThermos . snd)
    (atCentres drawInt . clues . snd <> sudokugrid . snd <> drawThermos . snd . fst)

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
    cd = atCentres drawCurve . clues <> grid gDefault

doubleback :: Backend' b =>
              RenderPuzzle b AreaGrid Loop
doubleback = (,)
    p
    (solstyle . drawDualEdges . snd <> p . fst)
  where
    p = grid gDefault <> drawAreasGray

slalom :: Backend' b =>
          RenderPuzzle b IntGrid (SGrid SlalomDiag)
slalom = (,)
    drawSlalomGrid
    (drawSlalomGrid . fst <> solstyle . drawSlalomDiags . snd)

compass :: Backend' b =>
           RenderPuzzle b (SGrid (Clue CompassC)) AreaGrid
compass = (,)
    drawCompassGrid
    (drawCompassClues . fst <> (grid gDefault <> drawAreasGray) . snd)

boxof2or3 :: Backend' b =>
             RenderPuzzle b (SGrid MasyuPearl, [Edge]) ()
boxof2or3 = (,)
    (atCentres smallPearl . values . fst
     <> phantom' . grid gDefault . fst
     <> drawThinDualEdges . snd)
    (error "boxof2or3 solution not implemented")

afternoonskyscrapers :: Backend' b =>
                        RenderPuzzle b (SGrid Shade) IntGrid
afternoonskyscrapers = (,)
    (grid gDefault <> atCentres drawShadow . values)
    (drawIntGrid . snd <> atCentres drawShadow . values . fst)

meanderingnumbers :: Backend' b =>
                        RenderPuzzle b AreaGrid IntGrid
meanderingnumbers = (,)
    (grid gDefault <> drawAreas)
    (drawIntGrid . snd <> drawAreas . fst)

tapa :: Backend' b =>
        RenderPuzzle b (SGrid (Maybe TapaClue)) ShadedGrid
tapa = (,)
    tapaGrid
    (tapaGrid . fst <> drawShade . snd)
  where
    tapaGrid = atCentres drawTapaClue . clues <> grid gDefault

japanesesums :: Backend' b =>
                RenderPuzzle b (OutsideClues [Int], String)
                               (SGrid (Either Black Int))
japanesesums = (,)
    (outsideIntGrid . fst <> n)
    (outsideIntGrid . fst . fst <> japcells . snd)
  where
    n (ocs, ds) = placeNote (outsideSize ocs) (drawText ds # scale 0.8)
    japcells = atCentres japcell . values
    japcell (Left Black) = fillBG gray
    japcell (Right x) = drawInt x

coral :: Backend' b =>
          RenderPuzzle b (OutsideClues [String]) ShadedGrid
coral = (,)
    drawOutsideGrid
    (drawOutsideGrid . fst <> drawShade . snd)

maximallengths :: Backend' b =>
                  RenderPuzzle b (OutsideClues (Maybe Int)) Loop
maximallengths = (,)
    g
    (solstyle . drawDualEdges . snd <> g . fst)
  where
    g = atCentres drawInt . outsideClues
        <> grid gDefault . outsideGrid

primeplace :: Backend' b =>
              RenderPuzzle b (SGrid PrimeDiag) (SGrid Int)
primeplace = (,)
    g
    (atCentres drawInt . values . snd <> g . fst)
  where
    g = grid gStyle
        <> atCentres drawPrimeDiag . values
    gStyle = GridStyle LineThin LineThick Nothing VertexNone

labyrinth :: Backend' b =>
             RenderPuzzle b (SGrid (Clue Int), [Edge]) (SGrid (Clue Int))
labyrinth = (,)
    (atCentres drawInt . clues . fst <> g)
    (atCentres drawInt . clues . snd <> g . fst)
  where
    g = drawEdges . snd <> grid gPlain . fst

bahnhof :: Backend' b =>
            RenderPuzzle b (SGrid (Maybe BahnhofClue)) [Edge]
bahnhof = (,)
    (atCentres drawBahnhofClue . clues <> grid gDefault)
    (atCentres drawBahnhofStation . clues . fst
     <> solstyle . drawDualEdges . snd
     <> grid gDefault . fst)
  where
    drawBahnhofStation = either drawInt (const mempty)

cave ::
    (Backend b R2, Renderable (Path R2) b) =>
    RenderPuzzle b (SGrid (Clue Int)) ShadedGrid
cave = (,)
    (grid gDashDash <> atCentres drawInt . clues)
    (drawEdges . edgesGen (/=) not . snd
     <> atCentres drawInt . clues . fst
     <> drawShade . snd
     <> grid gStyle . fst)
  where
    gDashDash = GridStyle LineDashed LineDashed Nothing VertexNone
    gStyle = GridStyle LineDashed LineNone (Just $ FrameStyle 8 gray)
                       VertexNone
