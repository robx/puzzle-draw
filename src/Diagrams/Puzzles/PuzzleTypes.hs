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
    blackoutDominos, angleLoop, anglers, cave, skyscrapers,
    summon, baca, buchstabensalat, doppelblock, sudokuDoppelblock,
    dominos, skyscrapersStars, fillominoCheckered, numberlink,
    slithermulti, dominoPills, fillominoLoop
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

fillominoCheckered :: Backend' b => RenderPuzzle b IntGrid (SGrid Int)
fillominoCheckered = (,)
    (atCentres drawInt . clues <> dashedgrid . size)
    ((atCentres drawInt . values
      <> drawEdges . borders
      <> outframe . size
      <> shadeGrid . checker) . snd)
  where
    checker = fmap pickColour . colour
    pickColour 1 = Nothing
    pickColour 2 = Just gray
    pickColour _ = Just red

fillominoLoop :: Backend' b => RenderPuzzle b IntGrid (SGrid Int, Loop)
fillominoLoop = (,)
    (fst fillomino)
    ((atCentres drawInt . values . fst
      <> solstyle . drawDualEdges . snd
      <> drawEdges . borders . fst
      <> dashedgrid . size . fst) . snd)

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

slithermulti :: Backend' b =>
                RenderPuzzle b (IntGrid, Int) [Edge]
slithermulti = (,)
    (drawSlitherGrid . fst <> n)
    (drawSlitherGrid . fst . fst <> solstyle . drawEdges . snd)
  where
    n (g, l) = placeNote (size g) (drawInt l ||| strutX 0.2 ||| miniloop)

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
    outsideGrid
    (drawShadedGrid . snd <> outsideGrid . fst)

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
            RenderPuzzle b (SGrid (Clue Char)) ()
bahnhof = (,)
    drawClueGrid
    undefined

blackoutDominos :: Backend' b =>
                   RenderPuzzle b (SGrid (Clue Int), DigitRange)
                                  (SGrid (Clue Int), AreaGrid)
blackoutDominos = (,)
    p
    (atCentres drawInt . clues . fst . snd
     <> irregAreaGridX gridDashing . snd . snd)
  where
    p (g, ds) = (atCentres drawInt . clues <> irregularGrid' gridDashing $ g)
                `aboveT`
                drawDominos ds

angleLoop ::
    Backend' b =>
    RenderPuzzle b (SGrid (Clue Int)) VertexLoop
angleLoop = (,)
    (cs <> gr)
    (cs . fst
     <> lineJoin LineJoinBevel . solstyle . strokeLocLoop . vertexLoop . snd
     <> gr . fst)
  where
    cs = atVertices drawAnglePoly . clues
    gr = plaingrid . size'
    size' g = let (w, h) = size g in (w-1, h-1)

anglers ::
    Backend' b =>
    RenderPuzzle b (OutsideClues (Clue Int), SGrid (Maybe Fish)) [Edge]
anglers = (,)
    (p <> g)
    (p . fst <> solstyle . drawDualEdges . snd <> g . fst)
  where
    p = atCentres drawInt' . outsideClues . fst <>
        atCentres drawFish' . clues . snd
    g = grid . size . snd
    drawInt' x = drawInt x <> (square 0.6 # lc white # fc white)
    drawFish' x = drawFish x <> (square 0.6 # lc white # fc white)

cave ::
    Backend' b =>
    RenderPuzzle b (SGrid (Clue Int)) ShadedGrid
cave = (,)
    g
    (drawEdges . edgesGen (/=) not . snd
     <> drawShadedGrid . snd <> fr . fst
     <> g . fst)
  where
    g = gridDashing . plaingrid . size <> atCentres drawInt . clues
    fr gr = outframe (size gr) # lc gray

skyscrapers ::
    Backend' b =>
    RenderPuzzle b (OutsideClues (Maybe Int)) IntGrid
skyscrapers = (,)
    g
    (g . fst <> atCentres drawInt . clues . snd)
  where
    g = atCentres drawInt . outsideClues
        <> grid . outsideSize

skyscrapersStars ::
    Backend' b =>
    RenderPuzzle b (OutsideClues (Maybe Int), Int)
                   (SGrid (Either Int Star))
skyscrapersStars = (,)
    (g <> n)
    (g . fst <> atCentres (either drawInt drawStar) . values . snd)
  where
    g = (atCentres drawInt . outsideClues <> grid . outsideSize) . fst
    n (oc, s) = placeNote (outsideSize oc)
                          (drawInt s ||| strutX 0.2 ||| drawStar Star)

summon ::
    Backend' b =>
    RenderPuzzle b (AreaGrid, OutsideClues (Maybe Int)) IntGrid
summon = (,)
    p
    (p . fst <> atCentres drawInt . clues . snd)
  where
    p (g, oc) = drawAreaGridGray g
                <> atCentres (scale 0.7 . drawInt) (outsideClues oc)

baca ::
    Backend' b =>
    RenderPuzzle b (SGrid (Maybe Char),
                    OutsideClues [Int],
                    OutsideClues (Maybe Char))
                   (SGrid (Either Black Char))
baca = (,)
    (inside <> outside)
    (outside . fst <> atCentres drawVal . values . snd <> inside . fst)
  where
    inside (g,_,_) = atCentres drawChar (clues g)
    outside (g,tl,br) =
              grid (size g)
              <> atCentres (scale 0.8 . drawInt) (multiOutsideClues tl)
              <> atCentres (scale 0.8 . drawChar) (outsideClues br)
    drawVal (Right c) = drawChar c
    drawVal (Left _) = fillBG gray

buchstabensalat ::
    Backend' b =>
    RenderPuzzle b (OutsideClues (Maybe Char), String) (SGrid (Maybe Char))
buchstabensalat = (p <> n, p . fst <> atCentres drawChar . clues . snd)
  where
    p = (atCentres (scale 0.8 . drawChar) . outsideClues
         <> grid . outsideSize) . fst
    n (ocs, ls) = placeNote (outsideSize ocs) (drawText ls # scale 0.8)

doppelblock ::
    Backend' b =>
    RenderPuzzle b (OutsideClues (Maybe Int))
                   (SGrid (Either Black Int))
doppelblock = (,)
    p
    (p . fst <> atCentres drawVal . values . snd)
  where
    p = atCentres (scale 0.8 . drawInt) . outsideClues
        <> grid . outsideSize
    drawVal (Right c) = drawInt c
    drawVal (Left _) = fillBG gray

sudokuDoppelblock ::
    Backend' b =>
    RenderPuzzle b (AreaGrid, OutsideClues (Maybe Int)) (SGrid (Either Black Int))
sudokuDoppelblock = (,)
    p
    (p . fst <> atCentres drawVal . values . snd)
  where
    p = atCentres (scale 0.8 . drawInt) . outsideClues . snd
        <> drawAreaGrid . fst
    drawVal (Right c) = drawInt c
    drawVal (Left _) = fillBG gray

dominos ::
    Backend' b =>
    RenderPuzzle b (SGrid (Clue Int), DigitRange) AreaGrid
dominos = (,)
    p
    (atCentres drawInt . clues . fst . fst <> drawAreaGridGray' gridDashing . snd)
  where
    p (g, r) =
        ((atCentres drawInt . clues <> dashedgrid . size) $ g)
        `aboveT`
        drawDominos r

dominoPills ::
    Backend' b =>
    RenderPuzzle b (SGrid (Clue Int), DigitRange, DigitRange) AreaGrid
dominoPills = (,)
    p
    (atCentres drawInt . clues . fst3 . fst <> drawAreaGridGray' gridDashing . snd)
  where
    fst3 (a,_,_) = a
    p (g, ds, ps) =
        ((atCentres drawInt . clues <> dashedgrid . size) $ g)
        `aboveT`
        (drawDominos ds ||| strutX 0.5 ||| drawPills ps)

numberlink ::
    Backend' b =>
    RenderPuzzle b (SGrid (Maybe Int)) [Edge]
numberlink = (,)
    drawIntGrid
    (atCentres drawInt' . clues . fst
     <> solstyle . drawDualEdges . snd
     <> grid . size . fst)
  where
    drawInt' x = drawInt x <> (square 0.7 # lc white # fc white)
