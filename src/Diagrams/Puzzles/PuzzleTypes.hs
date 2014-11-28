
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
    cave, angleLoop
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
import Data.Puzzles.GridShape
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

geradeweg :: Backend' b => RenderPuzzle b (Grid C (Maybe Int)) (Loop C)
geradeweg = (,)
    drawIntGrid
    (placeGrid . fmap drawInt . clues . fst
     <> solstyle . drawEdges . snd
     <> grid gDefault . fst)

fillomino :: Backend' b => RenderPuzzle b (Grid C (Maybe Int)) (Grid C Int)
fillomino = (,)
    (placeGrid . fmap drawInt . clues <> grid gDashed)
    ((placeGrid . fmap drawInt <> drawEdges . borders <> grid gDashed) . snd)

masyu :: Backend' b =>
         RenderPuzzle b (Grid C (Maybe MasyuPearl)) (Loop C)
masyu = (,)
    p
    (solstyle . drawEdges . snd <> p . fst)
  where
    p = placeGrid . fmap pearl . clues <> grid gDefault

nurikabe :: Backend' b =>
            RenderPuzzle b (Grid C (Maybe Int)) ShadedGrid
nurikabe = (,)
    drawIntGrid
    (drawIntGrid . fst <> drawShade . snd)

latintapa :: Backend' b =>
             RenderPuzzle b (Grid C (Maybe [String])) (Grid C (Maybe Char))
latintapa = (,)
    l
    (l . fst <> placeGrid . fmap drawChar . clues . snd)
  where
    l = grid gDefault <> drawWordsClues

sudoku :: Backend' b =>
          RenderPuzzle b (Grid C (Maybe Int)) (Grid C (Maybe Int))
sudoku = (,)
    (placeGrid . fmap drawInt . clues <> sudokugrid)
    ((placeGrid . fmap drawInt . clues <> sudokugrid) . snd)

thermosudoku :: Backend' b =>
                RenderPuzzle b (Grid C (Maybe Int), [Thermometer]) (Grid C (Maybe Int))
thermosudoku = (,)
    (placeGrid . fmap drawInt . clues . fst <> sudokugrid . fst <> drawThermos . snd)
    (placeGrid . fmap drawInt . clues . snd <> sudokugrid . snd <> drawThermos . snd . fst)

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
           RenderPuzzle b (Grid C (Maybe Int)) (Loop N)
slither = (,)
    drawSlitherGrid
    (drawSlitherGrid . fst <> solstyle . drawEdges . snd)

liarslither :: Backend' b =>
               RenderPuzzle b (Grid C (Maybe Int)) (Loop N, Grid C Bool)
liarslither = (,)
    drawSlitherGrid
    (placeGrid . fmap (solstyle . drawCross) . snd . snd
     <> drawSlitherGrid . fst
     <> solstyle . drawEdges . fst . snd)

tightfitskyscrapers :: Backend' b =>
                       RenderPuzzle b (OutsideClues C (Maybe Int), Grid C (Tightfit ()))
                                      (Grid C (Tightfit Int))
tightfitskyscrapers = (,)
    (placeGrid . fmap drawInt . clues . outsideClues . fst
     <> drawTightGrid (const mempty) . snd)
    (placeGrid . fmap drawInt . clues . outsideClues . fst . fst
     <> drawTightGrid drawInt . snd)

wordgrid :: Backend' b =>
            Grid C (Maybe Char) -> [String] -> Diagram b R2
wordgrid g ws = stackWords ws `besidesR` drawCharGrid g

wordloop :: Backend' b =>
            RenderPuzzle b (Grid C (Maybe Char), [String]) (Grid C (Maybe Char))
wordloop = (,)
    (uncurry wordgrid)
    (drawCharGrid . snd)

wordsearch :: Backend' b =>
              RenderPuzzle b (Grid C (Maybe Char), [String])
                             (Grid C (Maybe Char), [MarkedWord])
wordsearch = (,)
    (uncurry wordgrid) 
    (solstyle . drawMarkedWords . snd . snd
     <> drawCharGrid . fst . snd)

curvedata :: Backend' b =>
             RenderPuzzle b (Grid C (Maybe [Edge N])) [Edge C]
curvedata = (,)
    cd
    ((solstyle . drawEdges . snd) <> cd . fst)
  where
    cd = placeGrid . fmap drawCurve . clues <> grid gDefault

doubleback :: Backend' b =>
              RenderPuzzle b AreaGrid (Loop C)
doubleback = (,)
    p
    (solstyle . drawEdges . snd <> p . fst)
  where
    p = grid gDefault <> drawAreasGray

slalom :: Backend' b =>
          RenderPuzzle b (Grid N (Maybe Int)) (Grid C SlalomDiag)
slalom = (,)
    p
    (p . fst <> placeGrid . fmap (solstyle . drawSlalomDiag) . snd)
  where
    p = placeGrid . fmap drawSlalomClue . clues
        <> grid gDefault . cellGrid

compass :: Backend' b =>
           RenderPuzzle b (Grid C (Maybe CompassC)) AreaGrid
compass = (,)
    (placeGrid . fmap drawCompassClue . clues <> grid gDefault)
    (placeGrid . fmap drawCompassClue . clues . fst
     <> (grid gDefault <> drawAreasGray) . snd)

boxof2or3 :: Backend' b =>
             RenderPuzzle b (Grid N MasyuPearl, [Edge N]) ()
boxof2or3 = (,)
    (placeGrid . fmap smallPearl . fst
     <> drawThinEdges . snd)
    (error "boxof2or3 solution not implemented")

afternoonskyscrapers :: Backend' b =>
                        RenderPuzzle b (Grid C Shade) (Grid C (Maybe Int))
afternoonskyscrapers = (,)
    (grid gDefault <> placeGrid . fmap drawShadow)
    (drawIntGrid . snd <> placeGrid . fmap drawShadow . fst)

meanderingnumbers :: Backend' b =>
                        RenderPuzzle b AreaGrid (Grid C (Maybe Int))
meanderingnumbers = (,)
    (grid gDefault <> drawAreas)
    (drawIntGrid . snd <> drawAreas . fst)

tapa :: Backend' b =>
        RenderPuzzle b (Grid C (Maybe TapaClue)) ShadedGrid
tapa = (,)
    tapaGrid
    (tapaGrid . fst <> drawShade . snd)
  where
    tapaGrid = placeGrid . fmap drawTapaClue . clues <> grid gDefault

japanesesums :: Backend' b =>
                RenderPuzzle b (OutsideClues C [Int], String)
                               (Grid C (Either Black Int))
japanesesums = (,)
    (outsideIntGrid . fst <> n)
    (outsideIntGrid . fst . fst <> japcells . snd)
  where
    n (ocs, ds) = placeNote (outsideSize ocs) (drawText ds # scale 0.8)
    japcells = placeGrid . fmap japcell
    japcell (Left Black) = fillBG gray
    japcell (Right x) = drawInt x

coral :: Backend' b =>
          RenderPuzzle b (OutsideClues C [String]) ShadedGrid
coral = (,)
    drawMultiOutsideGrid
    (drawMultiOutsideGrid . fst <> drawShade . snd)

maximallengths :: Backend' b =>
                  RenderPuzzle b (OutsideClues C (Maybe Int)) (Loop C)
maximallengths = (,)
    g
    (solstyle . drawEdges . snd <> g . fst)
  where
    g = placeGrid . fmap drawInt . clues . outsideClues
        <> grid gDefault . outsideGrid

primeplace :: Backend' b =>
              RenderPuzzle b (Grid C PrimeDiag) (Grid C Int)
primeplace = (,)
    g
    (placeGrid . fmap drawInt . snd <> g . fst)
  where
    g = grid gStyle
        <> placeGrid . fmap drawPrimeDiag
    gStyle = GridStyle LineThin LineThick Nothing VertexNone

labyrinth :: Backend' b =>
             RenderPuzzle b (Grid C (Maybe Int), [Edge N]) (Grid C (Maybe Int))
labyrinth = (,)
    (placeGrid . fmap drawInt . clues . fst <> g)
    (placeGrid . fmap drawInt . clues . snd <> g . fst)
  where
    g = drawEdges . snd <> grid gPlain . fst

bahnhof :: Backend' b =>
            RenderPuzzle b (Grid C (Maybe BahnhofClue)) [Edge C]
bahnhof = (,)
    (placeGrid . fmap drawBahnhofClue . clues <> grid gDefault)
    (placeGrid . fmap drawBahnhofStation . clues . fst
     <> solstyle . drawEdges . snd
     <> grid gDefault . fst)
  where
    drawBahnhofStation = either drawInt (const mempty)

cave ::
    (Backend b R2, Renderable (Path R2) b) =>
    RenderPuzzle b (Grid C (Maybe Int)) ShadedGrid
cave = (,)
    (grid gDashDash <> placeGrid . fmap drawInt . clues)
    (drawEdges . edgesGen (/=) not . snd
     <> placeGrid . fmap drawInt . clues . fst
     <> drawShade . snd
     <> grid gStyle . fst)
  where
    gDashDash = GridStyle LineDashed LineDashed Nothing VertexNone
    gStyle = GridStyle LineDashed LineNone (Just $ FrameStyle 8 gray)
                       VertexNone

angleLoop ::
    Backend' b =>
    RenderPuzzle b (Grid N (Clue Int)) VertexLoop
angleLoop = (,)
    (cs <> gr)
    (cs . fst
     <> lineJoin LineJoinBevel . solstyle . strokeLocLoop . vertexLoop . snd
     <> gr . fst)
  where
    cs = placeGrid . fmap drawAnglePoly . clues
    gr = grid gPlain . cellGrid

