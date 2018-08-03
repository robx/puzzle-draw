
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Draw.PuzzleTypes (
    lits, geradeweg, fillomino, masyu, nurikabe, latintapa,
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
    pentominoSums, coralLits, coralLitso, snake, countryRoad,
    killersudoku, friendlysudoku, japsummasyu
  ) where

import Diagrams.Prelude hiding (Loop, N, coral, size)

import Data.Char (isUpper)
import Data.List (nub, sort, sortOn)
import qualified Data.Map.Strict as Map

import Draw.Style
import Draw.PuzzleGrids
import Draw.Draw
import Draw.Grid
import qualified Draw.Pyramid as DPyr
import Draw.Elements
import Draw.Lib
import Draw.Widths

import Data.Grid
import Data.GridShape
import Data.Elements
import qualified Data.Pyramid as Pyr

unimplemented :: String -> a
unimplemented x = error (x ++ " unimplemented")

lits :: Backend' b => Drawers b AreaGrid ShadedGrid
lits = Drawers
    (grid gDefault <> drawAreasGray)
    ((drawAreas <> grid gDefault) . fst <> drawShade . snd)

litssym :: Backend' b => Drawers b AreaGrid ShadedGrid
litssym = Drawers
    p
    (p . fst <> drawShade . snd)
  where
    p g = drawAreas g <> grid gDefault g <> translate (c g) (scale 0.5 $ smallPearl MBlack)
    c g = let (rs, cs) = size . Map.mapKeys toCoord $ g
          in r2 ((fromIntegral rs) / 2, (fromIntegral cs) / 2)

solstyle :: (HasStyle a, InSpace V2 Double a) => a -> a
solstyle = lc (blend 0.8 black white) . lwG (3 * onepix)

geradeweg :: Backend' b => Drawers b (Grid C (Maybe Int)) (Loop C)
geradeweg = Drawers
    drawIntGrid
    (placeGrid . fmap drawInt . clues . fst
     <> solstyle . drawEdges . snd
     <> grid gDefault . fst)

fillomino :: Backend' b => Drawers b (Grid C (Maybe Int)) (Grid C Int)
fillomino = Drawers
    (placeGrid . fmap drawInt . clues <> grid gDashed)
    ((placeGrid . fmap drawInt <> drawEdges . borders <> grid gDashed) . snd)

fillominoCheckered :: Backend' b => Drawers b (Grid C (Maybe Int)) (Grid C Int)
fillominoCheckered = Drawers
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

fillominoLoop :: Backend' b => Drawers b (Grid C (Maybe Int))
                                              (Grid C Int, Loop C)
fillominoLoop = Drawers
    (placeGrid . fmap drawInt . clues <> grid gDashed)
    ((placeGrid . fmap drawInt . fst
      <> solstyle . drawEdges . snd
      <> drawEdges . borders . fst
      <> grid gDashed . fst) . snd)

masyu :: Backend' b =>
         Drawers b (Grid C (Maybe MasyuPearl)) (Loop C)
masyu = Drawers
    p
    (solstyle . drawEdges . snd <> p . fst)
  where
    p = placeGrid . fmap pearl . clues <> grid gDefault

nurikabe :: Backend' b =>
            Drawers b (Grid C (Maybe Int)) ShadedGrid
nurikabe = Drawers
    drawIntGrid
    (drawIntGrid . fst <> drawShade . snd)

latintapa :: Backend' b =>
             Drawers b (Grid C (Maybe [String])) (Grid C (Maybe Char))
latintapa = Drawers
    l
    (l . fst <> placeGrid . fmap drawChar . clues . snd)
  where
    l = grid gDefault <> drawWordsClues

sudoku :: Backend' b =>
          Drawers b (Grid C (Maybe Int)) (Grid C (Maybe Int))
sudoku = Drawers
    (placeGrid . fmap drawInt . clues <> sudokugrid)
    ((placeGrid . fmap drawInt . clues <> sudokugrid) . snd)

thermosudoku :: Backend' b =>
                Drawers b (Grid C (Maybe Int), [Thermometer]) (Grid C (Maybe Int))
thermosudoku = Drawers
    (placeGrid . fmap drawInt . clues . fst <> sudokugrid . fst <> drawThermos . snd)
    (placeGrid . fmap drawInt . clues . snd <> sudokugrid . snd <> drawThermos . snd . fst)

killersudoku :: Backend' b =>
                Drawers b (AreaGrid, Map.Map Char Int, Grid C (Maybe Int)) (Grid C Int)
killersudoku = Drawers
    (p <> placeGrid . fmap drawInt . clues . trd3)
    (placeGrid . fmap drawInt . snd <> p . fst)
  where
    fst3 (x,_,_) = x
    trd3 (_,_,z) = z
    p = sudokugrid . fst3 <> cages
    cages (g, m, _) = drawCages (Map.filter (/= '.') g) (Map.map drawInt m)

pyramid :: Backend' b =>
    Drawers b Pyr.Pyramid Pyr.PyramidSol
pyramid = Drawers
    DPyr.pyramid
    (DPyr.pyramid . merge)
  where
    merge (p, q) = Pyr.mergepyramidsol p q

kpyramid :: Backend' b =>
    Drawers b Pyr.RowKropkiPyramid Pyr.PyramidSol
kpyramid = Drawers
    DPyr.kpyramid
    (DPyr.kpyramid . merge)
  where
    merge (p, q) = Pyr.mergekpyramidsol p q

slither :: Backend' b =>
           Drawers b (Grid C (Maybe Int)) (Loop N)
slither = Drawers
    drawSlitherGrid
    (drawSlitherGrid . fst <> solstyle . drawEdges . snd)

liarslither :: Backend' b =>
               Drawers b (Grid C (Maybe Int)) (Loop N, Grid C Bool)
liarslither = Drawers
    drawSlitherGrid
    (placeGrid . fmap (solstyle . drawCross) . snd . snd
     <> drawSlitherGrid . fst
     <> solstyle . drawEdges . fst . snd)

slithermulti :: Backend' b =>
                Drawers b (Grid C (Maybe Int), Int) [Edge N]
slithermulti = Drawers
    (drawSlitherGrid . fst <> n)
    (drawSlitherGrid . fst . fst <> solstyle . drawEdges . snd)
  where
    n (g, l) = placeNoteTR (size' g) (drawInt l ||| strutX' 0.2 ||| miniloop)
    size' = size . Map.mapKeys toCoord

tightfitskyscrapers :: Backend' b =>
                       Drawers b (OutsideClues C (Maybe Int), Grid C (Tightfit ()))
                                      (Grid C (Tightfit Int))
tightfitskyscrapers = Drawers
    (placeOutside . fmap (fmap drawInt) . fst
     <> drawTightGrid (const mempty) . snd)
    (placeOutside . fmap (fmap drawInt) . fst . fst
     <> drawTightGrid drawInt . snd)

wordgrid :: Backend' b =>
            Grid C (Maybe Char) -> [String] -> Drawing b
wordgrid g ws = stackWords ws `besidesR'` drawCharGrid g

wordloop :: Backend' b =>
            Drawers b (Grid C (Maybe Char), [String]) (Grid C (Maybe Char))
wordloop = Drawers
    (uncurry wordgrid)
    (drawCharGrid . snd)

wordsearch :: Backend' b =>
              Drawers b (Grid C (Maybe Char), [String])
                             (Grid C (Maybe Char), [MarkedWord])
wordsearch = Drawers
    (uncurry wordgrid) 
    (solstyle . drawMarkedWords . snd . snd
     <> drawCharGrid . fst . snd)

curvedata :: Backend' b =>
             Drawers b (Grid C (Maybe [Edge N])) [Edge C]
curvedata = Drawers
    (placeGrid . fmap drawCurve . clues
     <> grid gDefault)
    (placeGrid . fmap drawCurve . clues . fst
     <> solstyle . drawEdges . snd
     <> grid gDefault . fst)

doubleback :: Backend' b =>
              Drawers b AreaGrid (Loop C)
doubleback = Drawers
    p
    (solstyle . drawEdges . snd <> p . fst)
  where
    p = grid gDefault <> drawAreasGray

slalom :: Backend' b =>
          Drawers b (Grid N (Maybe Int)) (Grid C SlalomDiag)
slalom = Drawers
    p
    (p . fst <> placeGrid . fmap (solstyle . drawSlalomDiag) . snd)
  where
    p = placeGrid . fmap drawSlalomClue . clues
        <> grid gDefault . cellGrid

compass :: Backend' b =>
           Drawers b (Grid C (Maybe CompassC)) AreaGrid
compass = Drawers
    (placeGrid . fmap drawCompassClue . clues <> grid gDashed)
    (placeGrid . fmap drawCompassClue . clues . fst
     <> (grid gDashed <> drawAreasGray) . snd)

boxof2or3 :: Backend' b =>
             Drawers b (Grid N MasyuPearl, [Edge N]) ()
boxof2or3 = Drawers
    (placeGrid . fmap smallPearl . fst
     <> drawThinEdges . snd)
    (unimplemented "boxof2or3 solution")

afternoonskyscrapers :: Backend' b =>
                        Drawers b (Grid C Shade) (Grid C (Maybe Int))
afternoonskyscrapers = Drawers
    (grid gDefault <> placeGrid . fmap drawShadow)
    (drawIntGrid . snd <> placeGrid . fmap drawShadow . fst)

meanderingnumbers :: Backend' b =>
                        Drawers b AreaGrid (Grid C (Maybe Int))
meanderingnumbers = Drawers
    (grid gDefault <> drawAreas)
    (drawIntGrid . snd <> drawAreas . fst)

tapa :: Backend' b =>
        Drawers b (Grid C (Maybe TapaClue)) ShadedGrid
tapa = Drawers
    tapaGrid
    (tapaGrid . fst <> drawShade . snd)
  where
    tapaGrid = placeGrid . fmap drawTapaClue . clues <> grid gDefault

japanesesums :: Backend' b =>
                Drawers b (OutsideClues C [Int], String)
                               (Grid C (Either Black Int))
japanesesums = Drawers
    (outsideIntGrid . fst <> n)
    (outsideIntGrid . fst . fst <> japcells . snd)
  where
    n (ocs, ds) = placeNoteTL (0, h ocs) (text' ds # scale 0.8)
    japcells = placeGrid . fmap japcell
    japcell (Left Black) = fillBG gray
    japcell (Right x) = drawInt x
    h = snd . outsideSize

coral :: Backend' b =>
          Drawers b (OutsideClues C [String]) ShadedGrid
coral = Drawers
    drawMultiOutsideGrid
    (drawMultiOutsideGrid . fst <> drawShade . snd)

maximallengths :: Backend' b =>
                  Drawers b (OutsideClues C (Maybe Int)) (Loop C)
maximallengths = Drawers
    g
    (solstyle . drawEdges . snd <> g . fst)
  where
    g = placeOutside . fmap (fmap drawInt)
        <> grid gDefault . outsideGrid

primeplace :: Backend' b =>
              Drawers b (Grid C PrimeDiag) (Grid C Int)
primeplace = Drawers
    g
    (placeGrid . fmap drawInt . snd <> g . fst)
  where
    g = grid gStyle
        <> placeGrid . fmap drawPrimeDiag
    gStyle = GridStyle LineThin LineThick Nothing VertexNone

labyrinth :: Backend' b =>
             Drawers b (Grid C (Maybe Int), [Edge N], String) (Grid C (Maybe Int))
labyrinth = Drawers
    (placeGrid . fmap drawInt . clues . fst3 <> p <> n)
    (placeGrid . fmap drawInt . clues . snd <> p . fst)
  where
    p (g, e, _) = drawEdges e <> grid gPlain g
    n (g, _, ds) = placeNoteTR (size' g) (text' ds # scale 0.8)
    size' = size . Map.mapKeys toCoord
    fst3 (x,_,_) = x

bahnhof :: Backend' b =>
            Drawers b (Grid C (Maybe BahnhofClue)) [Edge C]
bahnhof = Drawers
    (placeGrid . fmap drawBahnhofClue . clues <> grid gDefault)
    (placeGrid . fmap drawBahnhofStation . clues . fst
     <> solstyle . drawEdges . snd
     <> grid gDefault . fst)
  where
    drawBahnhofStation = either drawInt (const mempty)

blackoutDominos :: Backend' b =>
                   Drawers b (Grid C (Clue Int), DigitRange)
                                  (Grid C (Clue Int), AreaGrid)
blackoutDominos = Drawers
    p
    ((placeGrid . fmap drawInt . clues . fst
      <> grid gDashedThick . fst 
      <> drawAreas . snd
      <> shadeGrid . fmap cols . snd) . snd)
  where
    p (g, ds) = (placeGrid . fmap drawInt . clues <> grid gDashedThick $ g)
                `aboveT'`
                drawDominos ds
    cols 'X' = Just gray
    cols _   = Nothing

angleLoop ::
    Backend' b =>
    Drawers b (Grid N (Clue Int)) VertexLoop
angleLoop = Drawers
    (cs <> gr)
    (cs . fst
     <> draw . lineJoin LineJoinBevel . solstyle . strokeLocLoop . vertexLoop . snd
     <> gr . fst)
  where
    cs = placeGrid . fmap drawAnglePoly . clues
    gr = grid gPlainDashed . cellGrid

anglers ::
    Backend' b =>
    Drawers b (OutsideClues C (Clue Int), Grid C (Maybe Fish)) [Edge C]
anglers = Drawers
    (p <> g)
    (p . fst <> solstyle . drawEdges . snd <> g . fst)
  where
    p = placeOutside . fmap (fmap drawInt') . fst <>
        placeGrid . fmap drawFish' . clues . snd
    g = grid gDefault . snd
    drawInt' x = drawInt x <> draw (square 0.6 # lc white # fc white)
    drawFish' x = drawFish x <> draw (square 0.6 # lc white # fc white)

cave ::
    Backend' b =>
    Drawers b (Grid C (Maybe Int)) ShadedGrid
cave = Drawers
    (grid gDashDash <> placeGrid . fmap drawInt . clues)
    (drawEdges . edgesGen (/=) not . snd
     <> placeGrid . fmap drawInt . clues . fst
     <> drawShade . snd
     <> grid gStyle . fst)
  where
    gDashDash = GridStyle LineDashed LineDashed Nothing VertexNone
    gStyle = GridStyle LineDashed LineNone (Just $ FrameStyle (8/3) gray)
                       VertexNone

skyscrapers ::
    Backend' b =>
    Drawers b (OutsideClues C (Maybe Int), String) (Grid C (Maybe Int))
skyscrapers = Drawers
    (g . fst <> n)
    (g . fst . fst <> placeGrid . fmap drawInt . clues . snd)
  where
    g = placeOutside . fmap (fmap drawInt)
        <> grid gDefault . outsideGrid
    n (oc, s) = placeNoteTR (outsideSize oc) (text' s)

shikaku :: Backend' b => Drawers b (Grid C (Maybe Int)) AreaGrid
shikaku = Drawers
    p
    (drawAreas . snd <> p . fst)
  where
    p = placeGrid . fmap drawInt . clues <> grid gDashed

slovaksums :: Backend' b => Drawers b (Grid C (Maybe SlovakClue), String) (Grid C (Maybe Int))
slovaksums = Drawers
    (p . fst <> n)
    (placeGrid . fmap drawInt . clues . snd <> p . fst . fst)
  where
    n (g, ds) = placeNoteTR (size' g) (text' ds # scale 0.8)
    p = grid gDefault <> placeGrid . fmap drawSlovakClue . clues
    size' = size . Map.mapKeys toCoord

skyscrapersStars ::
    Backend' b =>
    Drawers b (OutsideClues C (Maybe Int), Int)
                   (Grid C (Either Int Star))
skyscrapersStars = Drawers
    (g <> n)
    (g . fst <> placeGrid . fmap (either drawInt drawStar) . snd)
  where
    g = (placeOutside . fmap (fmap drawInt)
         <> grid gDefault . outsideGrid) . fst
    n (oc, s) = placeNoteTR (outsideSize oc)
                          (drawInt s ||| strutX' 0.2 ||| drawStar Star)

summon ::
    Backend' b =>
    Drawers b (AreaGrid, OutsideClues C (Maybe Int), String) (Grid C (Maybe Int))
summon = Drawers
    (p <> n)
    (placeGrid . fmap drawInt . clues . snd <> p . fst)
  where
    p (g, oc, _) = grid gDefault g <> drawAreasGray g
                <> (placeOutside
                    . al . fmap (fmap (scale 0.7 . drawInt)) $ oc)
    al :: Backend' b => OutsideClues k (Maybe (Drawing b)) -> OutsideClues k (Maybe (Drawing b))
    al (OC l r b t) = OC l (map (fmap alignL') r) b t

    n (g, _, ds) = placeNoteBR (size' g) (text' ds # scale 0.7)
    size' = size . Map.mapKeys toCoord

baca ::
    Backend' b =>
    Drawers b (Grid C (Maybe Char),
               OutsideClues C [Int],
               OutsideClues C (Maybe Char))
              (Grid C (Either Black Char))
baca = Drawers
    (inside <> outside)
    (outside . fst <> placeGrid . fmap drawVal . snd <> inside . fst)
  where
    inside (g,_,_) = placeGrid . fmap (fc gray . drawChar) . clues $ g
    outside (g,tl,br) =
              grid gDefault g
              <> (placeMultiOutside . fmap (fmap drawInt) $ tl)
              <> (placeOutside . fmap (fmap drawChar) $ br)
    drawVal (Right c) = drawChar c
    drawVal (Left _) = fillBG gray

buchstabensalat ::
    Backend' b =>
    Drawers b (OutsideClues C (Maybe Char), String) (Grid C (Maybe Char))
buchstabensalat = Drawers
    (p <> n)
    (p . fst <> placeGrid . fmap drawChar . clues . snd)
  where
    p = (placeOutside . fmap (fmap drawChar)
         <> grid gDefault . outsideGrid) . fst
    n (ocs, ls) = placeNoteTR (outsideSize ocs) (text' ls # scale 0.8)

doppelblock ::
    Backend' b =>
    Drawers b (OutsideClues C (Maybe Int))
              (Grid C (Either Black Int))
doppelblock = Drawers
    (p <> n)
    (p . fst <> placeGrid . fmap drawVal . snd)
  where
    p = placeOutside . fmap (fmap (scale 0.8 . drawInt))
        <> grid gDefault . outsideGrid
    n ocs = placeNoteTL (0, h) (text' ds # scale 0.8)
      where
        h = snd (outsideSize ocs)
        ds = "1-" ++ show (h - 2)
    drawVal (Right c) = drawInt c
    drawVal (Left _) = fillBG gray

sudokuDoppelblock ::
    Backend' b =>
    Drawers b (AreaGrid, OutsideClues C (Maybe Int))
                   (Grid C (Either Black Int))
sudokuDoppelblock = Drawers
    p
    (p . fst <> placeGrid . fmap drawVal . snd)
  where
    p = placeOutside . fmap (fmap (scale 0.8 . drawInt)) . snd
        <> (grid gDefault <> drawAreas) . fst
    drawVal (Right c) = drawInt c
    drawVal (Left _) = fillBG gray

dominos ::
    Backend' b =>
    Drawers b (Grid C (Clue Int), DigitRange) AreaGrid
dominos = Drawers
    p
    (placeGrid . fmap drawInt . clues . fst . fst
     <> (grid gDashed <> drawAreasGray) . snd)
  where
    p (g, r) =
        ((placeGrid . fmap drawInt . clues <> grid gDashed) $ g)
        `aboveT'`
        drawDominos r

dominoPills ::
    Backend' b =>
    Drawers b (Grid C (Clue Int), DigitRange, DigitRange) AreaGrid
dominoPills = Drawers
    p
    (placeGrid . fmap drawInt . clues . fst3 . fst
     <> (grid gDashed <> drawAreasGray) . snd)
  where
    fst3 (a,_,_) = a
    p (g, ds, ps) =
        ((placeGrid . fmap drawInt . clues <> grid gDashed) $ g)
        `aboveT'`
        (drawDominos ds ||| strutX' 0.5 ||| drawPills ps)

numberlink ::
    Backend' b =>
    Drawers b (Grid C (Maybe Int)) [Edge C]
numberlink = Drawers
    drawIntGrid
    (placeGrid . fmap drawInt' . clues . fst
     <> solstyle . drawEdges . snd
     <> grid gDefault . fst)
  where
    drawInt' x = drawInt x <> draw (square 0.7 # lc white # fc white)

loopki :: Backend' b =>
          Drawers b (Grid C (Maybe MasyuPearl)) (Loop N)
loopki = Drawers
    p
    (solstyle . drawEdges . snd <> p . fst)
  where
    p = placeGrid . fmap (scale 0.5 . pearl) . clues <> grid gSlither

scrabble :: Backend' b =>
            Drawers b (Grid C Bool, [String]) (Grid C (Maybe Char))
scrabble = Drawers
    p
    (placeGrid . fmap drawCharFixed . clues . snd <> gr . fst . fst)
  where
    p (g, ws) = stackWords ws `besidesR'` gr g
    gr = grid gDefault <> drawShade

neighbors :: Backend' b =>
             Drawers b (Grid C Bool, Grid C (Maybe Int)) (Grid C Int)
neighbors = Drawers
    (placeGrid . fmap drawInt . clues . snd <> (grid gDefault <> drawShade) . fst)
    (placeGrid . fmap drawInt . snd <> (grid gDefault <> drawShade) . fst . fst)

starwars :: Backend' b =>
            Drawers b (AreaGrid, [MarkedLine C]) (Grid C (Maybe Star))
starwars = Drawers
    p
    (p . fst <> placeGrid . fmap drawStar . clues . snd)
  where
    p = ((drawAreas <> grid gDefault) . fst <> drawMarkedLines . snd)

starbattle :: Backend' b =>
              Drawers b (AreaGrid, Int) (Grid C (Maybe Star))
starbattle = Drawers
    (p <> n)
    ((p <> n) . fst <> placeGrid . fmap drawStar . clues . snd)
  where
    p = (drawAreas <> grid gDefault) . fst
    n (g, k) = placeNoteTR (size' g)
                         (drawInt k ||| strutX' 0.2 ||| drawStar Star)
    size' = size . Map.mapKeys toCoord

heyawake :: Backend' b =>
            Drawers b (AreaGrid, Grid C (Maybe Int)) (Grid C Bool)
heyawake = Drawers
    (as <> cs)
    (as . fst <> drawShade . snd <> cs . fst)
  where
    as = (drawAreas <> grid gDefault) . fst
    cs = placeGrid . fmap drawInt . clues . snd

wormhole :: Backend' b =>
            Drawers b (Grid C (Maybe (Either Int Char))) ()
wormhole = Drawers
    (placeGrid . fmap (either drawInt drawChar) . clues <> grid gDashed)
    mempty

pentominous ::
    Backend' b =>
    Drawers b (Grid C (Maybe Char)) (Grid C Char)
pentominous = Drawers
    (placeGrid . fmap drawChar . clues <> grid gDashed)
    (placeGrid . fmap drawChar . clues . fst <>
     (drawAreas <> grid gDashed) . snd)

colorakari ::
    Backend' b =>
    Drawers b (Grid C (Maybe Char)) (Grid C (Maybe Char))
colorakari = Drawers
    (placeGrid . fmap drawColorClue . clues <> grid gDefault)
    (unimplemented "color akari solution")
  where
    drawColorClue 'X' = fillBG black
    drawColorClue c = case col c of Nothing -> error "invalid color"
                                    Just c' -> text' [c] # scale 0.5
                                               <> circle (1/3) # fc c' # draw
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
    Drawers b (AreaGrid, (Grid C (Maybe MEnd))) (Loop C)
persistenceOfMemory = Drawers
    (ends_ <> areas)
    (ends_ . fst <> solstyle . drawEdges . snd <> areas . fst)
  where
    ends_ = placeGrid . fmap drawEnd . clues . snd
    areas = (drawAreas <> grid gDashed <> shadeGrid . fmap cols) . fst
    cols c | isUpper c  = Just (blend 0.25 black white)
           | otherwise  = Nothing

mappingTable :: Backend' b => [(String, String)] -> Drawing b
mappingTable = b . g
  where
    b = placeGrid . fmap text' <> grid gPlain
    g ps = Map.fromList $
               [ (C 0 (l-i-1), x) | (i, x) <- zip [0..] c1 ] ++
               [ (C 1 (l-i-1), x) | (i, x) <- zip [0..] c2 ]
      where
        l = length ps
        c1 = map fst ps
        c2 = map snd ps

abctje ::
    Backend' b =>
    Drawers b (DigitRange, [(String, Int)]) [(Int, Char)]
abctje = Drawers
    p
    ((mappingTable . h  ||| const (strutX' 1.0) ||| mappingTable . h') . snd)
  where
    p (ds, cs) = (digNote ds `aboveT'` (stackWordsLeft ws ||| strutX' 1.0 ||| stackWordsRight ns))
                 `besidesR'` (strutX' 2.0 ||| mappingTable ps ||| strutX' 1.0 ||| mappingTable ps')
      where
        ws = map fst cs
        ns = map (show . snd) cs
        ls = nub . sort . concatMap fst $ cs
        ps = [ (x:[], "") | x <- ls ]
        ps' = [ (show x, "") | x <- digitList ds ]
    digNote (DigitRange x y) = note . text' $ show x ++ "-" ++ show y
    h = sortOn fst . map (\(x, y) -> (y:[], show x))
    h' = map (\(x, y) -> (show x, y:[]))

kropki ::
    Backend' b =>
    Drawers b (Map.Map (Edge N) KropkiDot) (Grid C Int)
kropki = Drawers
    (p <> n)
    (placeGrid . fmap drawInt . snd <> p . fst)
  where
    p = placeGrid' . Map.mapKeys midPoint . fmap kropkiDot <> grid gDefault . sizeGrid . sz
    n g = placeNoteTR (w, h) (text' ds # scale 0.8)
      where
        (w, h) = sz g
        ds = "1-" ++ show h
    sz m = edgeSize (Map.keys m)

statuepark ::
    Backend' b =>
    Drawers b (Grid C (Maybe MasyuPearl)) (Grid C Bool)
statuepark = Drawers
    p
    (p . fst <> drawShade . snd)
  where
    p = placeGrid . fmap pearl . clues <> grid gDashed

pentominousBorders ::
    Backend' b =>
    Drawers b (Grid C (), [Edge N]) (Grid C Char)
pentominousBorders = Drawers
    (drawEdges . snd <> grid gDashed . fst)
    ((drawAreas <> grid gDashed) . snd)

smallHintRooms ::
    Backend' b =>
    (AreaGrid, Grid C (Maybe Int)) -> Drawing b
smallHintRooms = ((drawAreas <> grid gDashed) . fst <> placeGrid . fmap hintTL . fmap show . clues . snd)

nanroSignpost ::
    Backend' b =>
    Drawers b (AreaGrid, Grid C (Maybe Int)) (Grid C Int)
nanroSignpost = Drawers
    smallHintRooms
    (placeGrid . fmap drawInt . snd <> smallHintRooms . fst)

tomTom ::
    Backend' b =>
    Drawers b (AreaGrid, Grid C (Maybe String)) (Grid C Int)
tomTom = Drawers
    p
    (placeGrid . fmap drawInt . snd <> p . fst)
  where
    p = ((drawAreas <> grid gDashed) . fst <> placeGrid . fmap hintTL . clues . snd)

horseSnake ::
    Backend' b =>
    Drawers b (Grid C (Maybe (Either MEnd Int))) [Edge C]
horseSnake = Drawers
    p
    (solstyle . drawEdges . snd <> p . fst)
  where
    p = (placeGrid . fmap (either drawBigEnd drawInt) . clues <> grid gDashed)

illumination ::
    Backend' b =>
    Drawers b (OutsideClues C (Maybe Fraction)) (Grid N (Maybe PlainNode), [Edge N])
illumination = Drawers
    p
    ((placeGrid . fmap (const (smallPearl MWhite)) . clues . fst <> drawEdges . snd) . snd <> p . fst)
  where
    p = placeOutside . fmap (fmap drawFraction)
        <> grid gDashed . outsideGrid

pentopia ::
    Backend' b =>
    Drawers b (Grid C (Maybe Myopia)) (Grid C Bool)
pentopia = Drawers
    p
    (p . fst <> drawShade . snd)
  where
    p = placeGrid . fmap drawMyopia . clues <> grid gDefault

pentominoPipes ::
    Backend' b =>
    Drawers b (Grid N Char) (Grid N KropkiDot, [Edge N])
pentominoPipes = Drawers
    (placeGrid . fmap drawCharOpaque <> grid gSlither . cellGrid)
    ((placeGrid . fmap kropkiDot . fst
      <> drawEdges . snd) . snd
     <> grid gSlither . cellGrid . fst)

greaterWall ::
    Backend' b =>
    Drawers b ([GreaterClue], [GreaterClue]) (Grid C Bool)
greaterWall = Drawers
    ((placeMultiOutside <> grid gDefault . outsideGrid) . munge)
    undefined
  where
    munge (rs,cs) = OC (map (reverse . greaterClue) (reverse rs)) [] []
                       (map (map (rotateBy (-1/4))) . map (reverse . greaterClue) $ cs)

galaxies ::
    Backend' b =>
    Drawers b (Grid C (), Grid N (), Grid C (), Map.Map (Edge N) ()) AreaGrid
galaxies = Drawers
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
    Drawers b (Grid C (Maybe Int)) ShadedGrid
mines = Drawers
    p
    (p . fst <> placeGrid . fmap (const (pearl MBlack)) . Map.filter id . snd)
  where
    p = grid gDefault <> placeGrid . fmap (\i -> drawInt i <> fillBG lightgray) . clues

tents ::
    Backend' b =>
    Drawers b (OutsideClues C (Maybe Int), Grid C (Maybe Tree)) (Grid C (Maybe PlacedTent))
tents = Drawers
    p
    (p . fst <> placeGrid . fmap drawTent . clues . snd)
  where
    p = placeOutside . fmap (fmap drawInt) . fst
        <> placeGrid . fmap drawTree . clues . snd
        <> grid gDashed . snd

pentominoSums :: Backend' b => Drawers b (OutsideClues C [String], String)
                               (Grid C (Either Pentomino Int), [(Char, Int)], OutsideClues C [String])
pentominoSums = Drawers
    p
    (solgrid ||| const (strutX' 1.0) ||| table)
  where
    p (ocs, ds) =
        (((drawMultiOutsideGrid ocs <> n (ocs, ds)) ||| strutX' 1.0 ||| emptyTable ocs)
        `aboveT'` drawPentominos)
    n (ocs, ds) = placeNoteTL (0, h ocs) (text' ds # scale 0.8)
    h = snd . outsideSize
    emptyTable = mappingTable . emptys
    emptys = map (\k -> (k, "")) . nub . sort . concat . outsideValues
    solgrid =
        skel . fst3 . snd
        <> drawMultiOutsideGrid . trd3 . snd
        <> cells . fst3 . snd
    fst3 (x,_,_) = x
    trd3 (_,_,z) = z
    skel = skeletonStyle . drawEdges . skeletons . lefts
    skeletonStyle = lc white . lwG (3 * onepix)
    lefts = clues . fmap (either Just (const Nothing))
    cells = placeGrid . fmap (\v -> case v of
        Left _  -> fillBG gray
        Right x -> drawInt x)
    table ((cs, _), (_, m, _)) = mappingTable m'
      where
        m' = Map.toList (Map.union (Map.fromList a) (Map.fromList (emptys cs)))
        a = map (\(k, v) -> ([k], show v)) m

coralLits ::
    Backend' b =>
    Drawers b (OutsideClues C [String]) (Grid C (Maybe Char))
coralLits = Drawers
    (\ocs -> drawMultiOutsideGrid ocs `aboveT'` drawLITS)
    (skeletonStyle . drawEdges . skeletons . clues . snd
     <> drawMultiOutsideGrid . fst
     <> placeGrid . fmap (const (fillBG gray)) . clues . snd)
  where
    skeletonStyle = lc white . lwG (3 * onepix)

coralLitso ::
    Backend' b =>
    Drawers b (OutsideClues C [String]) (Grid C (Either Black Char))
coralLitso = Drawers
    (\ocs -> drawMultiOutsideGrid ocs `aboveT'` drawLITSO)
    (drawMultiOutsideGrid . fst
     <> skeletonStyle . drawEdges . skeletons . rights . snd
     <> placeGrid . fmap (const (fillBG gray)) . lefts . snd)
  where
    skeletonStyle = solstyle
    lefts = clues . fmap (either Just (const Nothing))
    rights = clues . fmap (either (const Nothing) Just)

snake ::
    Backend' b =>
    Drawers b (OutsideClues C (Maybe Int), Grid C (Maybe MEnd))
                   (Grid C (Maybe (Either MEnd Black)))
snake = Drawers p s
  where
    cs = placeOutside . fmap (fmap drawInt) . fst
    p = cs
        <> placeGrid . fmap drawBigEnd . clues . snd
        <> grid gDefault . snd
    s = cs . fst
        <> grid gDefault . snd
        <> placeGrid . fmap (either (drawBigEnd <> gr) gr) . clues . snd
    gr = const (fillBG gray)

countryRoad ::
    Backend' b =>
    Drawers b (AreaGrid, Grid C (Maybe Int)) (Loop C)
countryRoad = Drawers
    smallHintRooms
    (solstyle . drawEdges . snd <> smallHintRooms . fst)

friendlysudoku ::
    Backend' b =>
    Drawers b (Map.Map (Edge N) KropkiDot, Grid C (Maybe Int)) (Grid C Int)
friendlysudoku = Drawers
    p
    (placeGrid . fmap drawInt . snd <> p . fst)
  where
    p = placeGrid' . Map.mapKeys midPoint . fmap kropkiDot . fst
      <> placeGrid . fmap drawInt . clues . snd
      <> sudokugrid . snd

japsummasyu :: Backend' b =>
          Drawers b (OutsideClues C [String]) ()
japsummasyu = Drawers
    (placeMultiOutside . fmap (fmap (scale 0.8 . text'))
                     <> grid gDashDash . outsideGrid)
    (error "japsummasyu solution not implemented")
  where
    gDashDash = GridStyle LineDashed LineDashed Nothing VertexNone
