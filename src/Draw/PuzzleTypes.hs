
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Draw.PuzzleTypes (
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
lits = drawers
    (grid gDefault <> drawAreasGray)
    ((drawAreas <> grid gDefault) . fst <> drawShade . snd)

litsplus :: Backend' b => Drawers b AreaGrid ShadedGrid
litsplus = lits

litssym :: Backend' b => Drawers b AreaGrid ShadedGrid
litssym = drawers
    p
    (p . fst <> drawShade . snd)
  where
    p g = drawAreas g <> grid gDefault g <> translate (c g) (scale 0.5 $ smallPearl MBlack)
    c g = let (rs, cs) = size . Map.mapKeys toCoord $ g
          in r2 ((fromIntegral rs) / 2, (fromIntegral cs) / 2)

solstyle :: (HasStyle a, InSpace V2 Double a) => a -> a
solstyle = lc (blend 0.8 black white) . lwG (3 * onepix)

geradeweg :: Backend' b => Drawers b (Grid C (Maybe Int)) (Loop C)
geradeweg = drawers
    drawIntGrid
    (placeGrid . fmap drawInt . clues . fst
     <> solstyle . drawEdges . snd
     <> grid gDefault . fst)

fillomino :: Backend' b => Drawers b (Grid C (Maybe Int)) (Grid C Int)
fillomino = drawers
    (placeGrid . fmap drawInt . clues <> grid gDashed)
    ((placeGrid . fmap drawInt <> drawEdges . borders <> grid gDashed) . snd)

fillominoCheckered :: Backend' b => Drawers b (Grid C (Maybe Int)) (Grid C Int)
fillominoCheckered = drawers
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
fillominoLoop = drawers
    (placeGrid . fmap drawInt . clues <> grid gDashed)
    ((placeGrid . fmap drawInt . fst
      <> solstyle . drawEdges . snd
      <> drawEdges . borders . fst
      <> grid gDashed . fst) . snd)

masyu :: Backend' b =>
         Drawers b (Grid C (Maybe MasyuPearl)) (Loop C)
masyu = drawers
    p
    (solstyle . drawEdges . snd <> p . fst)
  where
    p = placeGrid . fmap pearl . clues <> grid gDefault

nurikabe :: Backend' b =>
            Drawers b (Grid C (Maybe Int)) ShadedGrid
nurikabe = drawers
    drawIntGrid
    (drawIntGrid . fst <> drawShade . snd)

latintapa :: Backend' b =>
             Drawers b (Grid C (Maybe [String])) (Grid C (Maybe Char))
latintapa = drawers
    l
    (l . fst <> placeGrid . fmap drawChar . clues . snd)
  where
    l = grid gDefault <> drawWordsClues

sudoku :: Backend' b =>
          Drawers b (Grid C (Maybe Int)) (Grid C (Maybe Int))
sudoku = drawers
    (placeGrid . fmap drawInt . clues <> sudokugrid)
    ((placeGrid . fmap drawInt . clues <> sudokugrid) . snd)

thermosudoku :: Backend' b =>
                Drawers b (Grid C (Maybe Int), [Thermometer]) (Grid C (Maybe Int))
thermosudoku = drawers
    (placeGrid . fmap drawInt . clues . fst <> sudokugrid . fst <> drawThermos . snd)
    (placeGrid . fmap drawInt . clues . snd <> sudokugrid . snd <> drawThermos . snd . fst)

pyramid :: Backend' b =>
    Drawers b Pyr.Pyramid Pyr.PyramidSol
pyramid = drawers
    DPyr.pyramid
    (DPyr.pyramid . merge)
  where
    merge (p, q) = Pyr.mergepyramidsol p q

kpyramid :: Backend' b =>
    Drawers b Pyr.RowKropkiPyramid Pyr.PyramidSol
kpyramid = drawers
    DPyr.kpyramid
    (DPyr.kpyramid . merge)
  where
    merge (p, q) = Pyr.mergekpyramidsol p q

slither :: Backend' b =>
           Drawers b (Grid C (Maybe Int)) (Loop N)
slither = drawers
    drawSlitherGrid
    (drawSlitherGrid . fst <> solstyle . drawEdges . snd)

liarslither :: Backend' b =>
               Drawers b (Grid C (Maybe Int)) (Loop N, Grid C Bool)
liarslither = drawers
    drawSlitherGrid
    (placeGrid . fmap (solstyle . drawCross) . snd . snd
     <> drawSlitherGrid . fst
     <> solstyle . drawEdges . fst . snd)

slithermulti :: Backend' b =>
                Drawers b (Grid C (Maybe Int), Int) [Edge N]
slithermulti = drawers
    (drawSlitherGrid . fst <> n)
    (drawSlitherGrid . fst . fst <> solstyle . drawEdges . snd)
  where
    n (g, l) = placeNote (size' g) (drawInt l ||| strutX 0.2 ||| miniloop)
    size' = size . Map.mapKeys toCoord

tightfitskyscrapers :: Backend' b =>
                       Drawers b (OutsideClues C (Maybe Int), Grid C (Tightfit ()))
                                      (Grid C (Tightfit Int))
tightfitskyscrapers = drawers
    (placeGrid . fmap drawInt . clues . outsideClues . fst
     <> drawTightGrid (const mempty) . snd)
    (placeGrid . fmap drawInt . clues . outsideClues . fst . fst
     <> drawTightGrid drawInt . snd)

wordgrid :: Backend' b =>
            Grid C (Maybe Char) -> [String] -> Diagram b
wordgrid g ws = stackWords ws `besidesR` drawCharGrid g

wordloop :: Backend' b =>
            Drawers b (Grid C (Maybe Char), [String]) (Grid C (Maybe Char))
wordloop = drawers
    (uncurry wordgrid)
    (drawCharGrid . snd)

wordsearch :: Backend' b =>
              Drawers b (Grid C (Maybe Char), [String])
                             (Grid C (Maybe Char), [MarkedWord])
wordsearch = drawers
    (uncurry wordgrid) 
    (solstyle . drawMarkedWords . snd . snd
     <> drawCharGrid . fst . snd)

curvedata :: Backend' b =>
             Drawers b (Grid C (Maybe [Edge N])) [Edge C]
curvedata = drawers
    cd
    ((solstyle . drawEdges . snd) <> cd . fst)
  where
    cd = placeGrid . fmap drawCurve . clues <> grid gDefault

doubleback :: Backend' b =>
              Drawers b AreaGrid (Loop C)
doubleback = drawers
    p
    (solstyle . drawEdges . snd <> p . fst)
  where
    p = grid gDefault <> drawAreasGray

slalom :: Backend' b =>
          Drawers b (Grid N (Maybe Int)) (Grid C SlalomDiag)
slalom = drawers
    p
    (p . fst <> placeGrid . fmap (solstyle . drawSlalomDiag) . snd)
  where
    p = placeGrid . fmap drawSlalomClue . clues
        <> grid gDefault . cellGrid

compass :: Backend' b =>
           Drawers b (Grid C (Maybe CompassC)) AreaGrid
compass = drawers
    (placeGrid . fmap drawCompassClue . clues <> grid gDashed)
    (placeGrid . fmap drawCompassClue . clues . fst
     <> (grid gDashed <> drawAreasGray) . snd)

boxof2or3 :: Backend' b =>
             Drawers b (Grid N MasyuPearl, [Edge N]) ()
boxof2or3 = drawers
    (placeGrid . fmap smallPearl . fst
     <> drawThinEdges . snd)
    (unimplemented "boxof2or3 solution")

afternoonskyscrapers :: Backend' b =>
                        Drawers b (Grid C Shade) (Grid C (Maybe Int))
afternoonskyscrapers = drawers
    (grid gDefault <> placeGrid . fmap drawShadow)
    (drawIntGrid . snd <> placeGrid . fmap drawShadow . fst)

meanderingnumbers :: Backend' b =>
                        Drawers b AreaGrid (Grid C (Maybe Int))
meanderingnumbers = drawers
    (grid gDefault <> drawAreas)
    (drawIntGrid . snd <> drawAreas . fst)

tapa :: Backend' b =>
        Drawers b (Grid C (Maybe TapaClue)) ShadedGrid
tapa = drawers
    tapaGrid
    (tapaGrid . fst <> drawShade . snd)
  where
    tapaGrid = placeGrid . fmap drawTapaClue . clues <> grid gDefault

japanesesums :: Backend' b =>
                Drawers b (OutsideClues C [Int], String)
                               (Grid C (Either Black Int))
japanesesums = drawers
    (outsideIntGrid . fst <> n)
    (outsideIntGrid . fst . fst <> japcells . snd)
  where
    n (ocs, ds) = placeNoteTL (0, h ocs) (drawText ds # scale 0.8)
    japcells = placeGrid . fmap japcell
    japcell (Left Black) = fillBG gray
    japcell (Right x) = drawInt x
    h = snd . outsideSize

coral :: Backend' b =>
          Drawers b (OutsideClues C [String]) ShadedGrid
coral = drawers
    drawMultiOutsideGrid
    (drawMultiOutsideGrid . fst <> drawShade . snd)

maximallengths :: Backend' b =>
                  Drawers b (OutsideClues C (Maybe Int)) (Loop C)
maximallengths = drawers
    g
    (solstyle . drawEdges . snd <> g . fst)
  where
    g = placeGrid . fmap drawInt . clues . outsideClues
        <> grid gDefault . outsideGrid

primeplace :: Backend' b =>
              Drawers b (Grid C PrimeDiag) (Grid C Int)
primeplace = drawers
    g
    (placeGrid . fmap drawInt . snd <> g . fst)
  where
    g = grid gStyle
        <> placeGrid . fmap drawPrimeDiag
    gStyle = GridStyle LineThin LineThick Nothing VertexNone

labyrinth :: Backend' b =>
             Drawers b (Grid C (Maybe Int), [Edge N]) (Grid C (Maybe Int))
labyrinth = drawers
    (placeGrid . fmap drawInt . clues . fst <> g)
    (placeGrid . fmap drawInt . clues . snd <> g . fst)
  where
    g = drawEdges . snd <> grid gPlain . fst

bahnhof :: Backend' b =>
            Drawers b (Grid C (Maybe BahnhofClue)) [Edge C]
bahnhof = drawers
    (placeGrid . fmap drawBahnhofClue . clues <> grid gDefault)
    (placeGrid . fmap drawBahnhofStation . clues . fst
     <> solstyle . drawEdges . snd
     <> grid gDefault . fst)
  where
    drawBahnhofStation = either drawInt (const mempty)

blackoutDominos :: Backend' b =>
                   Drawers b (Grid C (Clue Int), DigitRange)
                                  (Grid C (Clue Int), AreaGrid)
blackoutDominos = drawers
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
    Drawers b (Grid N (Clue Int)) VertexLoop
angleLoop = drawers
    (cs <> gr)
    (cs . fst
     <> lineJoin LineJoinBevel . solstyle . strokeLocLoop . vertexLoop . snd
     <> gr . fst)
  where
    cs = placeGrid . fmap drawAnglePoly . clues
    gr = grid gPlainDashed . cellGrid

anglers ::
    Backend' b =>
    Drawers b (OutsideClues C (Clue Int), Grid C (Maybe Fish)) [Edge C]
anglers = drawers
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
    Drawers b (Grid C (Maybe Int)) ShadedGrid
cave = drawers
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
    Drawers b (OutsideClues C (Maybe Int), String) (Grid C (Maybe Int))
skyscrapers = drawers
    (g . fst <> n)
    (g . fst . fst <> placeGrid . fmap drawInt . clues . snd)
  where
    g = placeGrid . fmap drawInt . clues . outsideClues
        <> grid gDefault . outsideGrid
    n (oc, s) = placeNote (outsideSize oc) (drawText s)

shikaku :: Backend' b => Drawers b (Grid C (Maybe Int)) AreaGrid
shikaku = drawers
    p
    (drawAreas . snd <> p . fst)
  where
    p = placeGrid . fmap drawInt . clues <> grid gDashed

slovaksums :: Backend' b => Drawers b (Grid C (Maybe SlovakClue), String) (Grid C (Maybe Int))
slovaksums = drawers
    (p . fst <> n)
    (placeGrid . fmap drawInt . clues . snd <> p . fst . fst)
  where
    n (g, ds) = placeNote (size' g) (drawText ds # scale 0.8)
    p = grid gDefault <> placeGrid . fmap drawSlovakClue . clues
    size' = size . Map.mapKeys toCoord

skyscrapersStars ::
    Backend' b =>
    Drawers b (OutsideClues C (Maybe Int), Int)
                   (Grid C (Either Int Star))
skyscrapersStars = drawers
    (g <> n)
    (g . fst <> placeGrid . fmap (either drawInt drawStar) . snd)
  where
    g = (placeGrid . fmap drawInt . clues . outsideClues
         <> grid gDefault . outsideGrid) . fst
    n (oc, s) = placeNote (outsideSize oc)
                          (drawInt s ||| strutX 0.2 ||| drawStar Star)

summon ::
    Backend' b =>
    Drawers b (AreaGrid, OutsideClues C (Maybe Int), String) (Grid C (Maybe Int))
summon = drawers
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
    Drawers b (Grid C (Maybe Char),
                    OutsideClues C [Int],
                    OutsideClues C (Maybe Char))
                   (Grid C (Either Black Char))
baca = drawers
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
    Drawers b (OutsideClues C (Maybe Char), String) (Grid C (Maybe Char))
buchstabensalat = drawers
    (p <> n)
    (p . fst <> placeGrid . fmap drawChar . clues . snd)
  where
    p = (placeGrid . fmap drawChar . clues . outsideClues
         <> grid gDefault . outsideGrid) . fst
    n (ocs, ls) = placeNote (outsideSize ocs) (drawText ls # scale 0.8)

doppelblock ::
    Backend' b =>
    Drawers b (OutsideClues C (Maybe Int))
                   (Grid C (Either Black Int))
doppelblock = drawers
    p
    (p . fst <> placeGrid . fmap drawVal . snd)
  where
    p = placeGrid . fmap (scale 0.8 . drawInt) . clues . outsideClues
        <> grid gDefault . outsideGrid
    drawVal (Right c) = drawInt c
    drawVal (Left _) = fillBG gray

sudokuDoppelblock ::
    Backend' b =>
    Drawers b (AreaGrid, OutsideClues C (Maybe Int))
                   (Grid C (Either Black Int))
sudokuDoppelblock = drawers
    p
    (p . fst <> placeGrid . fmap drawVal . snd)
  where
    p = placeGrid . fmap (scale 0.8 . drawInt) . clues . outsideClues . snd
        <> (grid gDefault <> drawAreas) . fst
    drawVal (Right c) = drawInt c
    drawVal (Left _) = fillBG gray

dominos ::
    Backend' b =>
    Drawers b (Grid C (Clue Int), DigitRange) AreaGrid
dominos = drawers
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
    Drawers b (Grid C (Clue Int), DigitRange, DigitRange) AreaGrid
dominoPills = drawers
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
    Drawers b (Grid C (Maybe Int)) [Edge C]
numberlink = drawers
    drawIntGrid
    (placeGrid . fmap drawInt' . clues . fst
     <> solstyle . drawEdges . snd
     <> grid gDefault . fst)
  where
    drawInt' x = drawInt x <> (square 0.7 # lc white # fc white)

loopki :: Backend' b =>
          Drawers b (Grid C (Maybe MasyuPearl)) (Loop N)
loopki = drawers
    p
    (solstyle . drawEdges . snd <> p . fst)
  where
    p = placeGrid . fmap (scale 0.5 . pearl) . clues <> grid gSlither

scrabble :: Backend' b =>
            Drawers b (Grid C Bool, [String]) (Grid C (Maybe Char))
scrabble = drawers
    p
    (placeGrid . fmap drawCharFixed . clues . snd <> gr . fst . fst)
  where
    p (g, ws) = stackWords ws `besidesR` gr g
    gr = grid gDefault <> drawShade

neighbors :: Backend' b =>
             Drawers b (Grid C Bool, Grid C (Maybe Int)) (Grid C Int)
neighbors = drawers
    (placeGrid . fmap drawInt . clues . snd <> (grid gDefault <> drawShade) . fst)
    (placeGrid . fmap drawInt . snd <> (grid gDefault <> drawShade) . fst . fst)

starwars :: Backend' b =>
            Drawers b (AreaGrid, [MarkedLine C]) (Grid C (Maybe Star))
starwars = drawers
    p
    (p . fst <> placeGrid . fmap drawStar . clues . snd)
  where
    p = ((drawAreas <> grid gDefault) . fst <> drawMarkedLines . snd)

starbattle :: Backend' b =>
              Drawers b (AreaGrid, Int) (Grid C (Maybe Star))
starbattle = drawers
    (p <> n)
    ((p <> n) . fst <> placeGrid . fmap drawStar . clues . snd)
  where
    p = (drawAreas <> grid gDefault) . fst
    n (g, k) = placeNote (size' g)
                         (drawInt k ||| strutX 0.2 ||| drawStar Star)
    size' = size . Map.mapKeys toCoord

heyawake :: Backend' b =>
            Drawers b (AreaGrid, Grid C (Maybe Int)) (Grid C Bool)
heyawake = drawers
    (as <> cs)
    (as . fst <> drawShade . snd <> cs . fst)
  where
    as = (drawAreas <> grid gDefault) . fst
    cs = placeGrid . fmap drawInt . clues . snd

wormhole :: Backend' b =>
            Drawers b (Grid C (Maybe (Either Int Char))) ()
wormhole = drawers
    (placeGrid . fmap (either drawInt drawChar) . clues <> grid gDashed)
    mempty

pentominous ::
    Backend' b =>
    Drawers b (Grid C (Maybe Char)) (Grid C Char)
pentominous = drawers
    (placeGrid . fmap drawChar . clues <> grid gDashed)
    (placeGrid . fmap drawChar . clues . fst <>
     (drawAreas <> grid gDashed) . snd)

colorakari ::
    Backend' b =>
    Drawers b (Grid C (Maybe Char)) (Grid C (Maybe Char))
colorakari = drawers
    (placeGrid . fmap drawColorClue . clues <> grid gDefault)
    (unimplemented "color akari solution")
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
    Drawers b (AreaGrid, (Grid C (Maybe MEnd))) (Loop C)
persistenceOfMemory = drawers
    (ends_ <> areas)
    (ends_ . fst <> solstyle . drawEdges . snd <> areas . fst)
  where
    ends_ = placeGrid . fmap drawEnd . clues . snd
    areas = (drawAreas <> grid gDashed <> shadeGrid . fmap cols) . fst
    cols c | isUpper c  = Just (blend 0.25 black white)
           | otherwise  = Nothing

mappingTable :: Backend' b => [(String, String)] -> Diagram b
mappingTable = b . g
  where
    b = placeGrid . fmap drawText <> grid gPlain
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
abctje = drawers
    p
    ((mappingTable . h  ||| const (strutX 1.0) ||| mappingTable . h') . snd)
  where
    p (ds, cs) = (digNote ds `aboveT` (stackWordsLeft ws ||| strutX 1.0 ||| stackWordsRight ns))
                 `besidesR` (strutX 2.0 ||| mappingTable ps ||| strutX 1.0 ||| mappingTable ps')
      where
        ws = map fst cs
        ns = map (show . snd) cs
        ls = nub . sort . concatMap fst $ cs
        ps = [ (x:[], "") | x <- ls ]
        ps' = [ (show x, "") | x <- digitList ds ]
    digNote (DigitRange x y) = note . drawText $ show x ++ "-" ++ show y
    h = sortOn fst . map (\(x, y) -> (y:[], show x))
    h' = map (\(x, y) -> (show x, y:[]))

kropki ::
    Backend' b =>
    Drawers b (Map.Map (Edge N) KropkiDot) (Grid C Int)
kropki = drawers
    p
    (placeGrid . fmap drawInt . snd <> p . fst)
  where
    p = placeGrid' . Map.mapKeys midPoint . fmap kropkiDot <> grid gDefault . sizeGrid . sz
    sz m = edgeSize (Map.keys m)

statuepark ::
    Backend' b =>
    Drawers b (Grid C (Maybe MasyuPearl)) (Grid C Bool)
statuepark = drawers
    p
    (p . fst <> drawShade . snd)
  where
    p = placeGrid . fmap pearl . clues <> grid gDashed

pentominousBorders ::
    Backend' b =>
    Drawers b (Grid C (), [Edge N]) (Grid C Char)
pentominousBorders = drawers
    (drawEdges . snd <> grid gDashed . fst)
    ((drawAreas <> grid gDashed) . snd)

smallHintRooms ::
    Backend' b =>
    (AreaGrid, Grid C (Maybe Int)) -> Diagram b
smallHintRooms = ((drawAreas <> grid gDashed) . fst <> placeGrid . fmap hintTL . fmap show . clues . snd)

nanroSignpost ::
    Backend' b =>
    Drawers b (AreaGrid, Grid C (Maybe Int)) (Grid C Int)
nanroSignpost = drawers
    smallHintRooms
    (placeGrid . fmap drawInt . snd <> smallHintRooms . fst)

tomTom ::
    Backend' b =>
    Drawers b (AreaGrid, Grid C (Maybe String)) (Grid C Int)
tomTom = drawers
    p
    (placeGrid . fmap drawInt . snd <> p . fst)
  where
    p = ((drawAreas <> grid gDashed) . fst <> placeGrid . fmap hintTL . clues . snd)

horseSnake ::
    Backend' b =>
    Drawers b (Grid C (Maybe (Either MEnd Int))) [Edge C]
horseSnake = drawers
    p
    (solstyle . drawEdges . snd <> p . fst)
  where
    p = (placeGrid . fmap (either drawBigEnd drawInt) . clues <> grid gDashed)

illumination ::
    Backend' b =>
    Drawers b (OutsideClues C (Maybe Fraction)) (Grid N (Maybe PlainNode), [Edge N])
illumination = drawers
    p
    ((placeGrid . fmap (const (smallPearl MWhite)) . clues . fst <> drawEdges . snd) . snd <> p . fst)
  where
    p = placeGrid . fmap drawFraction . clues . outsideClues
        <> grid gDashed . outsideGrid

pentopia ::
    Backend' b =>
    Drawers b (Grid C (Maybe Myopia)) (Grid C Bool)
pentopia = drawers
    p
    (p . fst <> drawShade . snd)
  where
    p = placeGrid . fmap drawMyopia . clues <> grid gDefault

pentominoPipes ::
    Backend' b =>
    Drawers b (Grid N Char) (Grid N KropkiDot, [Edge N])
pentominoPipes = drawers
    (placeGrid . fmap drawCharOpaque <> grid gSlither . cellGrid)
    ((placeGrid . fmap kropkiDot . fst
      <> drawEdges . snd) . snd
     <> grid gSlither . cellGrid . fst)

greaterWall ::
    Backend' b =>
    Drawers b ([GreaterClue], [GreaterClue]) (Grid C Bool)
greaterWall = drawers
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
    Drawers b (Grid C (), Grid N (), Grid C (), Map.Map (Edge N) ()) AreaGrid
galaxies = drawers
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
mines = drawers
    p
    (p . fst <> placeGrid . fmap (const (pearl MBlack)) . Map.filter id . snd)
  where
    p = grid gDefault <> placeGrid . fmap (\i -> drawInt i <> fillBG lightgray) . clues

tents ::
    Backend' b =>
    Drawers b (OutsideClues C (Maybe Int), Grid C (Maybe Tree)) (Grid C (Maybe PlacedTent))
tents = drawers
    p
    (p . fst <> placeGrid . fmap drawTent . clues . snd)
  where
    p = placeGrid . fmap drawInt . clues . outsideClues . fst
        <> placeGrid . fmap drawTree . clues . snd
        <> grid gDashed . snd

pentominoSums :: Backend' b => Drawers b (OutsideClues C [String], String)
                               (Grid C (Either Pentomino Int), [(Char, Int)], OutsideClues C [String])
pentominoSums = drawers
    ((drawMultiOutsideGrid . fst <> n) ||| const (strutX 1.0) ||| emptyTable . fst)
    (solgrid ||| const (strutX 1.0) ||| table)
  where
    n (ocs, ds) = placeNoteTL (0, h ocs) (drawText ds # scale 0.8)
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
coralLits = drawers
    drawMultiOutsideGrid
    (skeletonStyle . drawEdges . skeletons . clues . snd
     <> drawMultiOutsideGrid . fst
     <> placeGrid . fmap (const (fillBG gray)) . clues . snd)
  where
    skeletonStyle = lc white . lwG (3 * onepix)

coralLitso ::
    Backend' b =>
    Drawers b (OutsideClues C [String]) (Grid C (Either Black Char))
coralLitso = drawers
    drawMultiOutsideGrid
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
snake = drawers p s
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
    Drawers b (AreaGrid, Grid C (Maybe Int)) (Loop C)
countryRoad = drawers
    smallHintRooms
    (solstyle . drawEdges . snd <> smallHintRooms . fst)
