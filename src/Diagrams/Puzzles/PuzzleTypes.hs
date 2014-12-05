
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
    blackoutDominos, angleLoop, anglers, cave, skyscrapers,
    summon, baca, buchstabensalat, doppelblock, sudokuDoppelblock,
    dominos, skyscrapersStars, fillominoCheckered, numberlink,
    slithermulti, dominoPills, fillominoLoop
  ) where

import Diagrams.Prelude hiding (Loop, coral)

import qualified Data.Map as Map

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

fillominoCheckered :: Backend' b => RenderPuzzle b (Grid C (Maybe Int)) (Grid C Int)
fillominoCheckered = (,)
    (placeGrid . fmap  drawInt . clues <> grid gDashed)
    ((placeGrid . fmap drawInt
      <> drawEdges . borders
      <> grid gDashed
      <> shadeGrid . checker) . snd)
  where
    checker = fmap pickColour . colour
    pickColour 1 = Nothing
    pickColour 2 = Just gray
    pickColour _ = Just red

fillominoLoop :: Backend' b => RenderPuzzle b (Grid C (Maybe Int))
                                              (Grid C Int, Loop C)
fillominoLoop = (,)
    (fst fillomino)
    ((placeGrid . fmap drawInt . fst
      <> solstyle . drawEdges . snd
      <> drawEdges . borders . fst
      <> grid gDashed . fst) . snd)

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

slithermulti :: Backend' b =>
                RenderPuzzle b (Grid C (Maybe Int), Int) [Edge N]
slithermulti = (,)
    (drawSlitherGrid . fst <> n)
    (drawSlitherGrid . fst . fst <> solstyle . drawEdges . snd)
  where
    n (g, l) = placeNote (size' g) (drawInt l ||| strutX 0.2 ||| miniloop)
    size' = size . Map.mapKeys toCoord

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

blackoutDominos :: Backend' b =>
                   RenderPuzzle b (Grid C (Clue Int), DigitRange)
                                  (Grid C (Clue Int), AreaGrid)
blackoutDominos = (,)
    p
    ((placeGrid . fmap drawInt . clues . fst
      <> grid gDashedThick . fst 
      <> drawAreas . snd
      <> shadeGrid . fmap cols . snd) . snd)
  where
    p (g, ds) = (placeGrid . fmap drawInt . clues <> grid gDashedThick $ g)
                `aboveT`
                drawDominos ds
    cols 'X' = Just gray
    cols _   = Nothing

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

anglers ::
    Backend' b =>
    RenderPuzzle b (OutsideClues C (Clue Int), Grid C (Maybe Fish)) [Edge C]
anglers = (,)
    (p <> g)
    (p . fst <> solstyle . drawEdges . snd <> g . fst)
  where
    p = placeGrid . fmap drawInt' . clues . outsideClues . fst <>
        placeGrid . fmap drawFish' . clues . snd
    g = grid gDefault . snd
    drawInt' x = drawInt x <> (square 0.6 # lc white # fc white)
    drawFish' x = drawFish x <> (square 0.6 # lc white # fc white)

cave ::
    Backend' b =>
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

skyscrapers ::
    Backend' b =>
    RenderPuzzle b (OutsideClues C (Maybe Int)) (Grid C (Maybe Int))
skyscrapers = (,)
    g
    (g . fst <> placeGrid . fmap drawInt . clues . snd)
  where
    g = placeGrid . fmap drawInt . clues . outsideClues
        <> grid gDefault . outsideGrid

skyscrapersStars ::
    Backend' b =>
    RenderPuzzle b (OutsideClues C (Maybe Int), Int)
                   (Grid C (Either Int Star))
skyscrapersStars = (,)
    (g <> n)
    (g . fst <> placeGrid . fmap (either drawInt drawStar) . snd)
  where
    g = (placeGrid . fmap drawInt . clues . outsideClues
         <> grid gDefault . outsideGrid) . fst
    n (oc, s) = placeNote (outsideSize oc)
                          (drawInt s ||| strutX 0.2 ||| drawStar Star)

summon ::
    Backend' b =>
    RenderPuzzle b (AreaGrid, OutsideClues C (Maybe Int)) (Grid C (Maybe Int))
summon = (,)
    p
    (p . fst <> placeGrid . fmap drawInt . clues . snd)
  where
    p (g, oc) = grid gDefault g <> drawAreasGray g
                <> (placeGrid . fmap (scale 0.7 . drawInt)
                    . clues . outsideClues $ oc)

baca ::
    Backend' b =>
    RenderPuzzle b (Grid C (Maybe Char),
                    OutsideClues C [Int],
                    OutsideClues C (Maybe Char))
                   (Grid C (Either Black Char))
baca = (,)
    (inside <> outside)
    (outside . fst <> placeGrid . fmap drawVal . snd <> inside . fst)
  where
    inside (g,_,_) = placeGrid . fmap (fc gray . drawChar) . clues $ g
    outside (g,tl,br) =
              grid gDefault g
              <> (placeGrid . fmap (scale 0.8 . drawInt)
                  . multiOutsideClues $ tl)
              <> (placeGrid . fmap (scale 0.8 . drawChar) . clues
                  . outsideClues $ br)
    drawVal (Right c) = drawChar c
    drawVal (Left _) = fillBG gray

buchstabensalat ::
    Backend' b =>
    RenderPuzzle b (OutsideClues C (Maybe Char), String) (Grid C (Maybe Char))
buchstabensalat = (p <> n, p . fst <> placeGrid . fmap drawChar . clues . snd)
  where
    p = (placeGrid . fmap (scale 0.8 . drawChar) . clues . outsideClues
         <> grid gDefault . outsideGrid) . fst
    n (ocs, ls) = placeNote (outsideSize ocs) (drawText ls # scale 0.8)

doppelblock ::
    Backend' b =>
    RenderPuzzle b (OutsideClues C (Maybe Int))
                   (Grid C (Either Black Int))
doppelblock = (,)
    p
    (p . fst <> placeGrid . fmap drawVal . snd)
  where
    p = placeGrid . fmap (scale 0.8 . drawInt) . clues . outsideClues
        <> grid gDefault . outsideGrid
    drawVal (Right c) = drawInt c
    drawVal (Left _) = fillBG gray

sudokuDoppelblock ::
    Backend' b =>
    RenderPuzzle b (AreaGrid, OutsideClues C (Maybe Int))
                   (Grid C (Either Black Int))
sudokuDoppelblock = (,)
    p
    (p . fst <> placeGrid . fmap drawVal . snd)
  where
    p = placeGrid . fmap (scale 0.8 . drawInt) . clues . outsideClues . snd
        <> (grid gDefault <> drawAreas) . fst
    drawVal (Right c) = drawInt c
    drawVal (Left _) = fillBG gray

dominos ::
    Backend' b =>
    RenderPuzzle b (Grid C (Clue Int), DigitRange) AreaGrid
dominos = (,)
    p
    (placeGrid . fmap drawInt . clues . fst . fst
     <> (grid gDashed <> drawAreasGray) . snd)
  where
    p (g, r) =
        ((placeGrid . fmap drawInt . clues <> grid gDashed) $ g)
        `aboveT`
        drawDominos r

dominoPills ::
    Backend' b =>
    RenderPuzzle b (Grid C (Clue Int), DigitRange, DigitRange) AreaGrid
dominoPills = (,)
    p
    (placeGrid . fmap drawInt . clues . fst3 . fst
     <> (grid gDashed <> drawAreasGray) . snd)
  where
    fst3 (a,_,_) = a
    p (g, ds, ps) =
        ((placeGrid . fmap drawInt . clues <> grid gDashed) $ g)
        `aboveT`
        (drawDominos ds ||| strutX 0.5 ||| drawPills ps)

numberlink ::
    Backend' b =>
    RenderPuzzle b (Grid C (Maybe Int)) [Edge C]
numberlink = (,)
    drawIntGrid
    (placeGrid . fmap drawInt' . clues . fst
     <> solstyle . drawEdges . snd
     <> grid gDefault . fst)
  where
    drawInt' x = drawInt x <> (square 0.7 # lc white # fc white)
