
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
    cave, angleLoop, shikaku, slovaksums,
    blackoutDominos, anglers, skyscrapers,
    summon, baca, buchstabensalat, doppelblock, sudokuDoppelblock,
    dominos, skyscrapersStars, fillominoCheckered, numberlink,
    slithermulti, dominoPills, fillominoLoop, loopki, litssym,
    scrabble, neighbors, starwars, heyawake, wormhole, pentominous,
    starbattle, colorakari, persistenceOfMemory, abctje, kropki,
    statuepark, pentominousBorders, nanroSignpost, tomTom,
    horseSnake, illumination, pentopia,
    pentominoPipes, greaterWall, galaxies, mines, tents,
    pentominoSums, coralLits, coralLitso, snake, countryRoad
  ) where

import Diagrams.Prelude hiding (Loop, N, coral, size)

import Data.Char (isUpper)
import Data.List (nub, sort, sortOn)
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

unimplemented :: String -> a
unimplemented x = error (x ++ " unimplemented")

lits :: Backend' b => RenderPuzzle b AreaGrid ShadedGrid
lits = (,)
    (grid gDefault <> drawAreasGray)
    ((drawAreas <> grid gDefault) . fst <> drawShade . snd)

litsplus :: Backend' b => RenderPuzzle b AreaGrid ShadedGrid
litsplus = lits

litssym :: Backend' b => RenderPuzzle b AreaGrid ShadedGrid
litssym = (,)
    p
    (p . fst <> drawShade . snd)
  where
    p g = drawAreas g <> grid gDefault g <> translate (c g) (scale 0.5 $ smallPearl MBlack)
    c g = let (rs, cs) = size . Map.mapKeys toCoord $ g
          in r2 ((fromIntegral rs) / 2, (fromIntegral cs) / 2)

solstyle :: (HasStyle a, InSpace V2 Double a) => a -> a
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
            Grid C (Maybe Char) -> [String] -> Diagram b
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
    (placeGrid . fmap drawCompassClue . clues <> grid gDashed)
    (placeGrid . fmap drawCompassClue . clues . fst
     <> (grid gDashed <> drawAreasGray) . snd)

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
    n (ocs, ds) = placeNoteTL (0, h ocs) (drawText ds # scale 0.8)
    japcells = placeGrid . fmap japcell
    japcell (Left Black) = fillBG gray
    japcell (Right x) = drawInt x
    h = snd . outsideSize

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
    gr = grid gPlainDashed . cellGrid

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
    RenderPuzzle b (OutsideClues C (Maybe Int), String) (Grid C (Maybe Int))
skyscrapers = (,)
    (g . fst <> n)
    (g . fst . fst <> placeGrid . fmap drawInt . clues . snd)
  where
    g = placeGrid . fmap drawInt . clues . outsideClues
        <> grid gDefault . outsideGrid
    n (oc, s) = placeNote (outsideSize oc) (drawText s)

shikaku :: Backend' b => RenderPuzzle b (Grid C (Maybe Int)) AreaGrid
shikaku = (,)
    p
    (drawAreas . snd <> p . fst)
  where
    p = placeGrid . fmap drawInt . clues <> grid gDashed

slovaksums :: Backend' b => RenderPuzzle b (Grid C (Maybe SlovakClue), String) (Grid C (Maybe Int))
slovaksums = (,)
    (p . fst <> n)
    (placeGrid . fmap drawInt . clues . snd <> p . fst . fst)
  where
    n (g, ds) = placeNote (size' g) (drawText ds # scale 0.8)
    p = grid gDefault <> placeGrid . fmap drawSlovakClue . clues
    size' = size . Map.mapKeys toCoord

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
    RenderPuzzle b (AreaGrid, OutsideClues C (Maybe Int), String) (Grid C (Maybe Int))
summon = (,)
    (p <> n)
    (placeGrid . fmap drawInt . clues . snd <> p . fst)
  where
    p (g, oc, _) = grid gDefault g <> drawAreasGray g
                <> (placeGrid . clues . outsideClues
                    . al . fmap (fmap (scale 0.7 . drawInt)) $ oc)
    al :: Backend' b => OutsideClues k (Maybe (Diagram b)) -> OutsideClues k (Maybe (Diagram b))
    al (OC l r b t) = OC l (map (fmap alignL) r) b t

    n (g, _, ds) = placeNoteBR (size' g) (drawText ds # scale 0.7)
    size' = size . Map.mapKeys toCoord

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
              <> (placeGrid . fmap drawInt
                  . multiOutsideClues $ tl)
              <> (placeGrid . fmap drawChar . clues
                  . outsideClues $ br)
    drawVal (Right c) = drawChar c
    drawVal (Left _) = fillBG gray

buchstabensalat ::
    Backend' b =>
    RenderPuzzle b (OutsideClues C (Maybe Char), String) (Grid C (Maybe Char))
buchstabensalat = (p <> n, p . fst <> placeGrid . fmap drawChar . clues . snd)
  where
    p = (placeGrid . fmap drawChar . clues . outsideClues
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

loopki :: Backend' b =>
          RenderPuzzle b (Grid C (Maybe MasyuPearl)) (Loop N)
loopki = (,)
    p
    (solstyle . drawEdges . snd <> p . fst)
  where
    p = placeGrid . fmap (scale 0.5 . pearl) . clues <> grid gSlither

scrabble :: Backend' b =>
            RenderPuzzle b (Grid C Bool, [String]) (Grid C (Maybe Char))
scrabble = (,)
    p
    (placeGrid . fmap drawCharFixed . clues . snd <> gr . fst . fst)
  where
    p (g, ws) = stackWords ws `besidesR` gr g
    gr = grid gDefault <> drawShade

neighbors :: Backend' b =>
             RenderPuzzle b (Grid C Bool, Grid C (Maybe Int)) (Grid C Int)
neighbors = (,)
    (placeGrid . fmap drawInt . clues . snd <> (grid gDefault <> drawShade) . fst)
    (placeGrid . fmap drawInt . snd <> (grid gDefault <> drawShade) . fst . fst)

starwars :: Backend' b =>
            RenderPuzzle b (AreaGrid, [MarkedLine C]) (Grid C (Maybe Star))
starwars = (,)
    p
    (p . fst <> placeGrid . fmap drawStar . clues . snd)
  where
    p = ((drawAreas <> grid gDefault) . fst <> drawMarkedLines . snd)

starbattle :: Backend' b =>
              RenderPuzzle b (AreaGrid, Int) (Grid C (Maybe Star))
starbattle = (,)
    (p <> n)
    ((p <> n) . fst <> placeGrid . fmap drawStar . clues . snd)
  where
    p = (drawAreas <> grid gDefault) . fst
    n (g, k) = placeNote (size' g)
                         (drawInt k ||| strutX 0.2 ||| drawStar Star)
    size' = size . Map.mapKeys toCoord

heyawake :: Backend' b =>
            RenderPuzzle b (AreaGrid, Grid C (Maybe Int)) (Grid C Bool)
heyawake = (,)
    (as <> cs)
    (as . fst <> drawShade . snd <> cs . fst)
  where
    as = (drawAreas <> grid gDefault) . fst
    cs = placeGrid . fmap drawInt . clues . snd

wormhole :: Backend' b =>
            RenderPuzzle b (Grid C (Maybe (Either Int Char))) ()
wormhole = (,)
    (placeGrid . fmap (either drawInt drawChar) . clues <> grid gDashed)
    mempty

pentominous ::
    Backend' b =>
    RenderPuzzle b (Grid C (Maybe Char)) (Grid C Char)
pentominous = (,)
    (placeGrid . fmap drawChar . clues <> grid gDashed)
    (placeGrid . fmap drawChar . clues . fst <>
     (drawAreas <> grid gDashed) . snd)

colorakari ::
    Backend' b =>
    RenderPuzzle b (Grid C (Maybe Char)) (Grid C (Maybe Char))
colorakari = (,)
    (placeGrid . fmap drawColorClue . clues <> grid gDefault)
    (error "color akari solution not implemented")
  where
    drawColorClue 'X' = fillBG black
    drawColorClue c = case col c of Nothing -> error "invalid color"
                                    Just c' -> drawText [c] # scale 0.5
                                               <> circle (1/3) # fc c'
                                               <> fillBG black
    col c = case c of 'R' -> Just red
                      'G' -> Just green
                      'B' -> Just blue
                      'Y' -> Just yellow
                      'C' -> Just cyan
                      'M' -> Just magenta
                      'W' -> Just white
                      _   -> Nothing

persistenceOfMemory ::
    Backend' b =>
    RenderPuzzle b (AreaGrid, (Grid C (Maybe MEnd))) (Loop C)
persistenceOfMemory = (,)
    (ends_ <> areas)
    (ends_ . fst <> solstyle . drawEdges . snd <> areas . fst)
  where
    ends_ = placeGrid . fmap drawEnd . clues . snd
    areas = (drawAreas <> grid gDashed <> shadeGrid . fmap cols) . fst
    cols c | isUpper c  = Just (blend 0.25 black white)
           | otherwise  = Nothing

abctje ::
    Backend' b =>
    RenderPuzzle b (DigitRange, [(String, Int)]) [(Int, Char)]
abctje = (,)
    p
    ((b . g . h  ||| const (strutX 1.0) ||| b . g . h') . snd)
  where
    p (ds, cs) = (digNote ds `aboveT` (stackWordsLeft ws ||| strutX 1.0 ||| stackWordsRight ns))
                 `besidesR` (strutX 2.0 ||| (b . g $ ps) ||| strutX 1.0 ||| (b . g $ ps'))
      where
        ws = map fst cs
        ns = map (show . snd) cs
        ls = nub . sort . concatMap fst $ cs
        ps = [ (x:[], "") | x <- ls ]
        ps' = [ (show x, "") | x <- digitList ds ]
    digNote (DigitRange x y) = note . drawText $ show x ++ "-" ++ show y
    b = placeGrid . fmap drawText <> grid gPlain
    h = sortOn fst . map (\(x, y) -> (y:[], show x))
    h' = map (\(x, y) -> (show x, y:[]))
    g ps = Map.fromList $
               [ (C 0 (l-i-1), x) | (i, x) <- zip [0..] c1 ] ++
               [ (C 1 (l-i-1), x) | (i, x) <- zip [0..] c2 ]
      where
        l = length ps
        c1 = map fst ps
        c2 = map snd ps

kropki ::
    Backend' b =>
    RenderPuzzle b (Map.Map (Edge N) KropkiDot) (Grid C Int)
kropki = (,)
    p
    (placeGrid . fmap drawInt . snd <> p . fst)
  where
    p = placeGrid' . Map.mapKeys midPoint . fmap kropkiDot <> grid gDefault . sizeGrid . sz
    sz m = edgeSize (Map.keys m)

statuepark ::
    Backend' b =>
    RenderPuzzle b (Grid C (Maybe MasyuPearl)) (Grid C Bool)
statuepark = (,)
    p
    (p . fst <> drawShade . snd)
  where
    p = placeGrid . fmap pearl . clues <> grid gDashed

pentominousBorders ::
    Backend' b =>
    RenderPuzzle b (Grid C (), [Edge N]) (Grid C Char)
pentominousBorders = (,)
    (drawEdges . snd <> grid gDashed . fst)
    ((drawAreas <> grid gDashed) . snd)

nanroSignpost ::
    Backend' b =>
    RenderPuzzle b (AreaGrid, Grid C (Maybe Int)) (Grid C Int)
nanroSignpost = (,)
    p
    (placeGrid . fmap drawInt . snd <> p . fst)
  where
    p = ((drawAreas <> grid gDashed) . fst <> placeGrid . fmap hintTL . fmap show . clues . snd)

tomTom ::
    Backend' b =>
    RenderPuzzle b (AreaGrid, Grid C (Maybe String)) (Grid C Int)
tomTom = (,)
    p
    (placeGrid . fmap drawInt . snd <> p . fst)
  where
    p = ((drawAreas <> grid gDashed) . fst <> placeGrid . fmap hintTL . clues . snd)

horseSnake ::
    Backend' b =>
    RenderPuzzle b (Grid C (Maybe (Either MEnd Int))) [Edge C]
horseSnake = (,)
    p
    (solstyle . drawEdges . snd <> p . fst)
  where
    p = (placeGrid . fmap (either drawBigEnd drawInt) . clues <> grid gDashed)

illumination ::
    Backend' b =>
    RenderPuzzle b (OutsideClues C (Maybe Fraction)) (Grid N (Maybe PlainNode), [Edge N])
illumination = (,)
    p
    ((placeGrid . fmap (const (smallPearl MWhite)) . clues . fst <> drawEdges . snd) . snd <> p . fst)
  where
    p = placeGrid . fmap drawFraction . clues . outsideClues
        <> grid gDashed . outsideGrid

pentopia ::
    Backend' b =>
    RenderPuzzle b (Grid C (Maybe Myopia)) (Grid C Bool)
pentopia = (,)
    p
    (p . fst <> drawShade . snd)
  where
    p = placeGrid . fmap drawMyopia . clues <> grid gDefault

pentominoPipes ::
    Backend' b =>
    RenderPuzzle b (Grid N Char) (Grid N KropkiDot, [Edge N])
pentominoPipes = (,)
    (placeGrid . fmap drawCharOpaque <> grid gSlither . cellGrid)
    ((placeGrid . fmap kropkiDot . fst
      <> drawEdges . snd) . snd
     <> grid gSlither . cellGrid . fst)

greaterWall ::
    Backend' b =>
    RenderPuzzle b ([GreaterClue], [GreaterClue]) (Grid C Bool)
greaterWall = (,)
    ((plc <> grid gDefault . outsideGrid) . munge)
    undefined
  where
    munge (rs,cs) = OC (map (reverse . greaterClue) (reverse rs)) [] []
                       (map (map (rotateBy (-1/4))) . map (reverse . greaterClue) $ cs)
    plc ocs = placeGrid' . Map.mapKeys toPt . multiOutsideClues $ ocs
      where
        OC l _ _ _ = ocs
        h = length l
        h' = fromIntegral h
        -- toPoint c = p2 (1/2, 1/2) .+^ r2i (c .--. C 0 0)
        -- terrible hack
        toPt c@(C x y) | x < 0  = let p = toPoint c in scaleX 0.7 p .+^ r2 (-1/2, 0)
                       | y >= h = let p = toPoint c in scaleY 0.7 (p .-^ r2 (0,h')) .+^ r2 (0, 1/2 + h')
        toPt c = toPoint c

galaxies ::
    Backend' b =>
    RenderPuzzle b (Grid C (), Grid N (), Grid C (), Map.Map (Edge N) ()) AreaGrid
galaxies = (,)
    p
    (p . fst <> drawAreas . snd)
  where
    p = (gals <> grid gDashed . fst4)
    gal = const (kropkiDot KWhite)
    gals (_, a,b,c) = (placeGrid . fmap gal $ a)
                   <> (placeGrid . fmap gal $ b)
                   <> (placeGrid' . fmap gal . Map.mapKeys midPoint $ c)
    fst4 (a,_,_,_) = a

mines ::
    Backend' b =>
    RenderPuzzle b (Grid C (Maybe Int)) ShadedGrid
mines = (,)
    p
    (p . fst <> placeGrid . fmap (const (pearl MBlack)) . Map.filter id . snd)
  where
    p = grid gDefault <> placeGrid . fmap (\i -> drawInt i <> fillBG lightgray) . clues

tents ::
    Backend' b =>
    RenderPuzzle b (OutsideClues C (Maybe Int), Grid C (Maybe Tree)) (Grid C (Maybe PlacedTent))
tents = (,)
    p
    (p . fst <> placeGrid . fmap drawTent . clues . snd)
  where
    p = placeGrid . fmap drawInt . clues . outsideClues . fst
        <> placeGrid . fmap drawTree . clues . snd
        <> grid gDashed . snd

pentominoSums :: Backend' b => RenderPuzzle b (OutsideClues C [String], String) ()
pentominoSums =
    (p, unimplemented "pentomino sums solution")
  where
    p = fst coral . fst <> n
    n (ocs, ds) = placeNoteTL (0, h ocs) (drawText ds # scale 0.8)
    h = snd . outsideSize

coralLits :: Backend' b => RenderPuzzle b (OutsideClues C [String]) ()
coralLits = (fst coral, unimplemented "coral lits solution")

coralLitso :: Backend' b => RenderPuzzle b (OutsideClues C [String]) ()
coralLitso = (fst coral, unimplemented "coral litso solution")

snake ::
    Backend' b =>
    RenderPuzzle b (OutsideClues C (Maybe Int), Grid C (Maybe MEnd))
                   (Grid C (Maybe (Either MEnd Black)))
snake = (p,s)
  where
    cs = placeGrid . fmap drawInt . clues . outsideClues . fst
    p = cs
        <> placeGrid . fmap drawBigEnd . clues . snd
        <> grid gDefault . snd
    s = cs . fst
        <> grid gDefault . snd
        <> placeGrid . fmap (either (drawBigEnd <> gr) gr) . clues . snd
    gr = const (fillBG gray)

countryRoad ::
    Backend' b =>
    RenderPuzzle b (AreaGrid, Grid C (Maybe Int)) ()
countryRoad =
    (fst nanroSignpost, unimplemented "country road solution")
