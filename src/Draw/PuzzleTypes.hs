{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Draw.PuzzleTypes
  ( lits,
    geradeweg,
    fillomino,
    masyu,
    nurikabe,
    latintapa,
    sudoku,
    thermosudoku,
    pyramid,
    kpyramid,
    slither,
    liarslither,
    tightfitskyscrapers,
    wordloop,
    wordsearch,
    curvedata,
    doubleback,
    slalom,
    compass,
    meanderingnumbers,
    tapa,
    japanesesums,
    coral,
    maximallengths,
    labyrinth,
    bahnhof,
    cave,
    angleLoop,
    shikaku,
    slovaksums,
    blackoutDominos,
    anglers,
    skyscrapers,
    summon,
    baca,
    buchstabensalat,
    doppelblock,
    sudokuDoppelblock,
    dominos,
    skyscrapersStars,
    fillominoCheckered,
    numberlink,
    dominoPills,
    fillominoLoop,
    loopki,
    scrabble,
    neighbors,
    heyawake,
    pentominous,
    starbattle,
    colorakari,
    persistenceOfMemory,
    abctje,
    kropki,
    statuepark,
    pentominousBorders,
    nanroSignpost,
    tomTom,
    horseSnake,
    illumination,
    pentopia,
    greaterWall,
    galaxies,
    mines,
    tents,
    pentominoSums,
    coralLits,
    coralLitso,
    snake,
    countryRoad,
    killersudoku,
    japsummasyu,
    arrowsudoku,
    dualloop,
  )
where

import Data.Char (isUpper)
import Data.Elements
import qualified Data.Grid as Data
import Data.Grid
  ( AreaGrid,
    Grid,
    OutsideClues (..),
    ShadedGrid,
    clues,
  )
import Data.GridShape
  ( C (..),
    Edge,
    N,
    ToCoord (..),
  )
import Data.List
  ( nub,
    sort,
    sortOn,
  )
import qualified Data.Map.Strict as Map
import qualified Data.Pyramid as Pyr
import Diagrams.Prelude hiding
  ( Loop,
    N,
    coral,
    end,
    size,
    star,
  )
import Draw.Draw
import Draw.Elements hiding (dominos)
import qualified Draw.Elements
import Draw.Grid
import Draw.Lib
import Draw.PuzzleGrids
import qualified Draw.Pyramid as DPyr
import Draw.Style
import Draw.Widths

unimplemented :: Backend' b => String -> (p, s) -> Drawing b
unimplemented _ _ = mempty

lits :: Backend' b => Drawers b AreaGrid ShadedGrid
lits =
  Drawers
    (grid gDefault <> areasGray)
    ((areas <> grid gDefault) . fst <> shade . snd)

geradeweg :: Backend' b => Drawers b (Grid C (Maybe Int)) (Loop C)
geradeweg =
  Drawers
    intGrid
    ( placeGrid
        . fmap int
        . clues
        . fst
        <> solstyle
        . edges
        . snd
        <> grid gDefault
        . fst
    )

fillomino :: Backend' b => Drawers b (Grid C (Maybe Int)) (Grid C Int)
fillomino =
  Drawers
    (placeGrid . fmap int . clues <> grid gDashed)
    ((placeGrid . fmap int <> edges . Data.borders <> grid gDashed) . snd)

fillominoCheckered :: Backend' b => Drawers b (Grid C (Maybe Int)) (Grid C Int)
fillominoCheckered =
  Drawers
    (placeGrid . fmap int . clues <> grid gDashed)
    ( ( placeGrid
          . fmap int
          <> edges
          . Data.borders
          <> grid gDashed
          <> shadeGrid
          . checker
      )
        . snd
    )
  where
    checker = fmap pickColour . Data.colour
    pickColour 1 = Nothing
    pickColour 2 = Just gray
    pickColour _ = Just red

fillominoLoop ::
  Backend' b => Drawers b (Grid C (Maybe Int)) (Grid C Int, Loop C)
fillominoLoop =
  Drawers
    (placeGrid . fmap int . clues <> grid gDashed)
    ( ( placeGrid
          . fmap int
          . fst
          <> solstyle
          . edges
          . snd
          <> edges
          . Data.borders
          . fst
          <> grid gDashed
          . fst
      )
        . snd
    )

masyu :: Backend' b => Drawers b (Grid C (Maybe MasyuPearl)) (Loop C)
masyu = Drawers p (solstyle . edges . snd <> p . fst)
  where
    p = placeGrid . fmap pearl . clues <> grid gDefault

nurikabe :: Backend' b => Drawers b (Grid C (Maybe Int)) ShadedGrid
nurikabe = Drawers intGrid (intGrid . fst <> shade . snd)

latintapa ::
  Backend' b => Drawers b (Grid C (Maybe [String])) (Grid C (Maybe Char))
latintapa = Drawers l (l . fst <> placeGrid . fmap char . clues . snd)
  where
    l = grid gDefault <> wordsClues

sudoku :: Backend' b => Drawers b (Grid C (Maybe Int)) (Grid C (Maybe Int))
sudoku =
  Drawers
    (placeGrid . fmap int . clues <> sudokugrid)
    ((placeGrid . fmap int . clues <> sudokugrid) . snd)

thermosudoku ::
  Backend' b =>
  Drawers b (Grid C (Maybe Int), [Thermometer]) (Grid C (Maybe Int))
thermosudoku =
  Drawers
    (placeGrid . fmap int . clues . fst <> sudokugrid . fst <> thermos . snd)
    ( placeGrid . fmap int . clues . snd <> sudokugrid . snd <> thermos . snd . fst
    )

killersudoku ::
  Backend' b =>
  Drawers b (AreaGrid, Map.Map Char Int, Grid C (Maybe Int)) (Grid C Int)
killersudoku =
  Drawers
    (p <> placeGrid . fmap int . clues . trd3)
    (placeGrid . fmap int . snd <> p . fst)
  where
    fst3 (x, _, _) = x
    trd3 (_, _, z) = z
    p = sudokugrid . fst3 <> cages'
    cages' (g, m, _) = cages (Map.filter (/= '.') g) (Map.map int m)

pyramid :: Backend' b => Drawers b Pyr.Pyramid Pyr.PyramidSol
pyramid = Drawers DPyr.pyramid (DPyr.pyramid . merge)
  where
    merge (p, q) = Pyr.mergepyramidsol p q

kpyramid :: Backend' b => Drawers b Pyr.RowKropkiPyramid Pyr.PyramidSol
kpyramid = Drawers DPyr.kpyramid (DPyr.kpyramid . merge)
  where
    merge (p, q) = Pyr.mergekpyramidsol p q

slither :: Backend' b => Drawers b (Grid C (Maybe Int)) (Loop N)
slither = Drawers slitherGrid (slitherGrid . fst <> solstyle . edges . snd)

liarslither ::
  Backend' b => Drawers b (Grid C (Maybe Int)) (Loop N, Grid C Bool)
liarslither =
  Drawers
    slitherGrid
    ( placeGrid
        . fmap (solstyle . cross)
        . snd
        . snd
        <> slitherGrid
        . fst
        <> solstyle
        . edges
        . fst
        . snd
    )

tightfitskyscrapers ::
  Backend' b =>
  Drawers
    b
    (OutsideClues C (Maybe Int), Grid C (Tightfit ()))
    (Grid C (Tightfit Int))
tightfitskyscrapers =
  Drawers
    (placeOutside . fmap (fmap int) . fst <> tightGrid (const mempty) . snd)
    (placeOutside . fmap (fmap int) . fst . fst <> tightGrid int . snd)

wordgrid :: Backend' b => Grid C (Maybe Char) -> [String] -> Drawing b
wordgrid g ws = stackWords ws `besidesR'` charGrid g

wordloop ::
  Backend' b =>
  Drawers b (Grid C (Maybe Char), [String]) (Grid C (Maybe Char))
wordloop = Drawers (uncurry wordgrid) (charGrid . snd)

wordsearch ::
  Backend' b =>
  Drawers
    b
    (Grid C (Maybe Char), [String])
    (Grid C (Maybe Char), [MarkedWord])
wordsearch =
  Drawers
    (uncurry wordgrid)
    (solstyle . markedWords . snd . snd <> charGrid . fst . snd)

curvedata :: Backend' b => Drawers b (Grid C (Maybe [Edge N])) [Edge C]
curvedata =
  Drawers
    (placeGrid . fmap curve . clues <> grid gDefault)
    ( placeGrid
        . fmap curve
        . clues
        . fst
        <> solstyle
        . edges
        . snd
        <> grid gDefault
        . fst
    )

doubleback :: Backend' b => Drawers b AreaGrid (Loop C)
doubleback = Drawers p (solstyle . edges . snd <> p . fst)
  where
    p = grid gDefault <> areasGray

slalom :: Backend' b => Drawers b (Grid N (Maybe Int)) (Grid C SlalomDiag)
slalom = Drawers p (p . fst <> placeGrid . fmap (solstyle . slalomDiag) . snd)
  where
    p = placeGrid . fmap slalomClue . clues <> grid gDefault . Data.cellGrid

compass :: Backend' b => Drawers b (Grid C (Maybe CompassC)) AreaGrid
compass =
  Drawers
    (placeGrid . fmap compassClue . clues <> grid gDashed)
    ( placeGrid
        . fmap compassClue
        . clues
        . fst
        <> (grid gDashed <> areasGray)
        . snd
    )

meanderingnumbers ::
  Backend' b => Drawers b (AreaGrid, Grid C (Maybe Int)) (Grid C (Maybe Int))
meanderingnumbers =
  Drawers
    (grid gDefault . fst <> areas . fst <> placeGrid . fmap int . clues . snd)
    (intGrid . snd <> areas . fst . fst)

tapa :: Backend' b => Drawers b (Grid C (Maybe TapaClue)) ShadedGrid
tapa = Drawers tapaGrid (tapaGrid . fst <> shade . snd)
  where
    tapaGrid = placeGrid . fmap tapaClue . clues <> grid gDefault

japanesesums ::
  Backend' b =>
  Drawers b (OutsideClues C [Int], String) (Grid C (Either Black Int))
japanesesums =
  Drawers
    (outsideIntGrid . fst <> n)
    (outsideIntGrid . fst . fst <> japcells . snd)
  where
    n (ocs, ds) = placeNoteTL (0, h ocs) (text' ds # scale noteScale)
    japcells = placeGrid . fmap japcell
    japcell (Left Black) = fillBG gray
    japcell (Right x) = int x
    h = snd . Data.outsideSize

coral :: Backend' b => Drawers b (OutsideClues C [String]) ShadedGrid
coral = Drawers multiOutsideGrid (multiOutsideGrid . fst <> shade . snd)

maximallengths :: Backend' b => Drawers b (OutsideClues C (Maybe Int)) (Loop C)
maximallengths = Drawers g (solstyle . edges . snd <> g . fst)
  where
    g = placeOutside . fmap (fmap int) <> grid gDefault . Data.outsideGrid

labyrinth ::
  Backend' b =>
  Drawers b (Grid C (Maybe Int), [Edge N], String) (Grid C (Maybe Int))
labyrinth =
  Drawers
    (placeGrid . fmap int . clues . fst3 <> p <> n)
    (placeGrid . fmap int . clues . snd <> p . fst)
  where
    p (g, e, _) = edges e <> grid gPlain g
    n (g, _, ds) = placeNoteTR (size' g) (text' ds # scale noteScale)
    size' = Data.size . Map.mapKeys toCoord
    fst3 (x, _, _) = x

bahnhof :: Backend' b => Drawers b (Grid C (Maybe BahnhofClue)) [Edge C]
bahnhof =
  Drawers
    (placeGrid . fmap bahnhofClue . clues <> grid gDefault)
    ( placeGrid
        . fmap bahnhofStation
        . clues
        . fst
        <> solstyle
        . edges
        . snd
        <> grid gDefault
        . fst
    )
  where
    bahnhofStation = either int (const mempty)

blackoutDominos ::
  Backend' b =>
  Drawers b (Grid C (Clue Int), DigitRange) (Grid C (Clue Int), AreaGrid)
blackoutDominos =
  Drawers
    p
    ( ( placeGrid
          . fmap int
          . clues
          . fst
          <> grid gDashedThick
          . fst
          <> areas
          . snd
          <> shadeGrid
          . fmap cols
          . snd
      )
        . snd
    )
  where
    p (g, ds) =
      (placeGrid . fmap int . clues <> grid gDashedThick $ g)
        `aboveT'` Draw.Elements.dominos ds
    cols 'X' = Just gray
    cols _ = Nothing

angleLoop :: Backend' b => Drawers b (Grid N (Clue Int)) VertexLoop
angleLoop =
  Drawers
    (cs <> gr)
    ( cs
        . fst
        <> draw
        . lineJoin LineJoinBevel
        . solstyle
        . strokeLocLoop
        . vertexLoop
        . snd
        <> gr
        . fst
    )
  where
    cs = placeGrid . fmap anglePoly . clues
    gr = grid gPlainDashed . Data.cellGrid

anglers ::
  Backend' b =>
  Drawers b (OutsideClues C (Clue Int), Grid C (Maybe Fish)) [Edge C]
anglers = Drawers (p <> g) (p . fst <> solstyle . edges . snd <> g . fst)
  where
    p =
      placeOutside
        . fmap (fmap int')
        . fst
        <> placeGrid
        . fmap fish'
        . clues
        . snd
    g = grid gDefault . snd
    int' x = int x <> draw (square 0.6 # lc white # fc white)
    fish' x = fish x <> draw (square 0.6 # lc white # fc white)

cave :: Backend' b => Drawers b (Grid C (Maybe Int)) ShadedGrid
cave =
  Drawers
    (grid gPlainDashed <> placeGrid . fmap int . clues)
    ( edges
        . Data.edgesGen (/=) not
        . snd
        <> placeGrid
        . fmap int
        . clues
        . fst
        <> shade
        . snd
        <> grid gStyle
        . fst
    )
  where
    gStyle =
      GridStyle LineDashed LineNone (Just $ FrameStyle (8 / 3) gray) VertexNone

skyscrapers ::
  Backend' b =>
  Drawers b (OutsideClues C (Maybe Int), String) (Grid C (Maybe Int))
skyscrapers =
  Drawers
    (g . fst <> n)
    (g . fst . fst <> placeGrid . fmap int . clues . snd)
  where
    g = placeOutside . fmap (fmap int) <> grid gDefault . Data.outsideGrid
    n (oc, s) = placeNoteTR (Data.outsideSize oc) (text' s)

shikaku :: Backend' b => Drawers b (Grid C (Maybe Int)) AreaGrid
shikaku = Drawers p (areas . snd <> p . fst)
  where
    p = placeGrid . fmap int . clues <> grid gDashed

slovaksums ::
  Backend' b =>
  Drawers b (Grid C (Maybe SlovakClue), String) (Grid C (Maybe Int))
slovaksums =
  Drawers
    (p . fst <> n)
    (placeGrid . fmap int . clues . snd <> p . fst . fst)
  where
    n (g, ds) = placeNoteTR (size' g) (text' ds # scale noteScale)
    p = grid gDefault <> placeGrid . fmap slovakClue . clues
    size' = Data.size . Map.mapKeys toCoord

skyscrapersStars ::
  Backend' b =>
  Drawers b (OutsideClues C (Maybe Int), Int) (Grid C (Either Int Star))
skyscrapersStars =
  Drawers
    (g <> n)
    (g . fst <> placeGrid . fmap (either int star) . snd)
  where
    g =
      (placeOutside . fmap (fmap int) <> grid gDefault . Data.outsideGrid) . fst
    n (oc, s) =
      placeNoteTR (Data.outsideSize oc) (int s ||| strutX' 0.2 ||| star Star)

summon ::
  Backend' b =>
  Drawers
    b
    (AreaGrid, OutsideClues C (Maybe Int), String)
    (Grid C (Maybe Int))
summon = Drawers (p <> n) (placeGrid . fmap int . clues . snd <> p . fst)
  where
    p (g, oc, _) =
      grid gDefault g
        <> areasGray g
        <> (placeOutside . al . fmap (fmap (scale 0.7 . int)) $ oc)
    al ::
      Backend' b =>
      OutsideClues k (Maybe (Drawing b)) ->
      OutsideClues k (Maybe (Drawing b))
    al (OC l r b t) = OC l (map (fmap alignL') r) b t
    n (g, _, ds) = placeNoteBR (size' g) (text' ds # scale 0.7)
    size' = Data.size . Map.mapKeys toCoord

baca ::
  Backend' b =>
  Drawers
    b
    ( Grid C (Maybe Char),
      OutsideClues C [Int],
      OutsideClues C (Maybe Char)
    )
    (Grid C (Either Black Char))
baca =
  Drawers
    (inside <> outside)
    (outside . fst <> placeGrid . fmap drawVal . snd <> inside . fst)
  where
    inside (g, _, _) = placeGrid . fmap (fc gray . char) . clues $ g
    outside (g, tl, br) =
      grid gDefault g
        <> (placeMultiOutside . fmap (fmap int) $ tl)
        <> (placeOutside . fmap (fmap char) $ br)
    drawVal (Right c) = char c
    drawVal (Left _) = fillBG gray

buchstabensalat ::
  Backend' b =>
  Drawers b (OutsideClues C (Maybe Char), String) (Grid C (Maybe Char))
buchstabensalat =
  Drawers
    (p <> n)
    (p . fst <> placeGrid . fmap char . clues . snd)
  where
    p =
      (placeOutside . fmap (fmap char) <> grid gDefault . Data.outsideGrid) . fst
    n (ocs, ls) = placeNoteTR (Data.outsideSize ocs) (text' ls # scale noteScale)

doppelblock ::
  Backend' b =>
  Drawers b (OutsideClues C (Maybe Int)) (Grid C (Either Black Int))
doppelblock = Drawers (p <> n) (p . fst <> placeGrid . fmap drawVal . snd)
  where
    p = outsideGrid . fmap (fmap show)
    n ocs = placeNoteTL (0, h) (text' ds # scale noteScale)
      where
        h = snd (Data.outsideSize ocs)
        ds = "1-" ++ show (h - 2)
    drawVal (Right c) = int c
    drawVal (Left _) = fillBG gray

sudokuDoppelblock ::
  Backend' b =>
  Drawers
    b
    (AreaGrid, OutsideClues C (Maybe Int))
    (Grid C (Either Black Int))
sudokuDoppelblock = Drawers p (p . fst <> placeGrid . fmap drawVal . snd)
  where
    p =
      placeOutside
        . fmap (fmap (scale outsideScale . int))
        . snd
        <> (grid gDefault <> areas)
        . fst
    drawVal (Right c) = int c
    drawVal (Left _) = fillBG gray

dominos :: Backend' b => Drawers b (Grid C (Clue Int), DigitRange) AreaGrid
dominos =
  Drawers
    p
    ( placeGrid . fmap int . clues . fst . fst <> (grid gDashed <> areasGray) . snd
    )
  where
    p (g, r) =
      ((placeGrid . fmap int . clues <> grid gDashed) $ g)
        `aboveT'` Draw.Elements.dominos r

dominoPills ::
  Backend' b =>
  Drawers b (Grid C (Clue Int), DigitRange, DigitRange) AreaGrid
dominoPills =
  Drawers
    p
    ( placeGrid
        . fmap int
        . clues
        . fst3
        . fst
        <> (grid gDashed <> areasGray)
        . snd
    )
  where
    fst3 (a, _, _) = a
    p (g, ds, ps) =
      ((placeGrid . fmap int . clues <> grid gDashed) $ g)
        `aboveT'` (Draw.Elements.dominos ds ||| strutX' 0.5 ||| pills ps)

numberlink :: Backend' b => Drawers b (Grid C (Maybe Int)) [Edge C]
numberlink =
  Drawers
    intGrid
    ( placeGrid
        . fmap int'
        . clues
        . fst
        <> solstyle
        . edges
        . snd
        <> grid gDefault
        . fst
    )
  where
    int' x = int x <> draw (square 0.7 # lc white # fc white)

loopki :: Backend' b => Drawers b (Grid C (Maybe MasyuPearl)) (Loop N)
loopki = Drawers p (solstyle . edges . snd <> p . fst)
  where
    p = placeGrid . fmap (scale 0.5 . pearl) . clues <> grid gSlither

scrabble ::
  Backend' b => Drawers b (Grid C Bool, [String]) (Grid C (Maybe Char))
scrabble =
  Drawers
    p
    (placeGrid . fmap charFixed . clues . snd <> gr . fst . fst)
  where
    p (g, ws) = stackWords ws `besidesR'` gr g
    gr = grid gDefault <> shade

neighbors ::
  Backend' b => Drawers b (Grid C Bool, Grid C (Maybe Int)) (Grid C Int)
neighbors =
  Drawers
    (placeGrid . fmap int . clues . snd <> (grid gDefault <> shade) . fst)
    (placeGrid . fmap int . snd <> (grid gDefault <> shade) . fst . fst)

starbattle :: Backend' b => Drawers b (AreaGrid, Int) (Grid C (Maybe Star))
starbattle =
  Drawers
    (p <> n)
    ((p <> n) . fst <> placeGrid . fmap star . clues . snd)
  where
    p = (areas <> grid gDefault) . fst
    n (g, k) = placeNoteTR (size' g) (int k ||| strutX' 0.2 ||| star Star)
    size' = Data.size . Map.mapKeys toCoord

heyawake :: Backend' b => Drawers b (AreaGrid, Grid C (Maybe Int)) (Grid C Bool)
heyawake = Drawers (as <> cs) (as . fst <> shade . snd <> cs . fst)
  where
    as = (areas <> grid gDefault) . fst
    cs = placeGrid . fmap int . clues . snd

pentominous :: Backend' b => Drawers b (Grid C (Maybe Char)) (Grid C Char)
pentominous =
  Drawers
    (placeGrid . fmap char . clues <> grid gDashed)
    (placeGrid . fmap char . clues . fst <> (areas <> grid gDashed) . snd)

colorakari ::
  Backend' b => Drawers b (Grid C (Maybe Char)) (Grid C (Maybe Char))
colorakari =
  Drawers
    p
    (p . fst <> placeGrid . fmap drawColorLight . clues . snd)
  where
    p = placeGrid . fmap drawColorClue . clues <> grid gDefault
    drawColorClue 'X' = fillBG black
    drawColorClue c = case col c of
      Nothing -> mempty
      Just c' ->
        text' [c] # scale 0.5 <> circle (1 / 3) # fc c' # draw <> fillBG black
    drawColorLight c = case col c of
      Nothing -> mempty
      Just c' ->
        (text' [c] # scale 0.5 <> circle (1 / 3) # fc c' # lwG 0 # draw)
          # scale 1.2
    col c = case c of
      'R' -> Just red
      'G' -> Just green
      'B' -> Just blue
      'Y' -> Just yellow
      'C' -> Just cyan
      'M' -> Just magenta
      'W' -> Just white
      _ -> Nothing

persistenceOfMemory ::
  Backend' b => Drawers b (AreaGrid, (Grid C (Maybe MEnd))) (Loop C)
persistenceOfMemory =
  Drawers
    (ends_ <> areas')
    (ends_ . fst <> solstyle . edges . snd <> areas' . fst)
  where
    ends_ = placeGrid . fmap end . clues . snd
    areas' = (areas <> grid gDashed <> shadeGrid . fmap cols) . fst
    cols c
      | isUpper c = Just (blend 0.25 black white)
      | otherwise = Nothing

mappingTable :: Backend' b => [(String, String)] -> Drawing b
mappingTable = b . g
  where
    b = placeGrid . fmap text' <> grid gPlain
    g ps =
      Map.fromList $
        [(C 0 (l - i - 1), x) | (i, x) <- zip [0 ..] c1]
          ++ [(C 1 (l - i - 1), x) | (i, x) <- zip [0 ..] c2]
      where
        l = length ps
        c1 = map fst ps
        c2 = map snd ps

abctje :: Backend' b => Drawers b (DigitRange, [(String, Int)]) [(Int, Char)]
abctje =
  Drawers
    p
    ((mappingTable . h ||| const (strutX' 1.0) ||| mappingTable . h') . snd)
  where
    p (ds, cs) =
      ( digNote ds
          `aboveT'` (stackWordsLeft ws ||| strutX' 1.0 ||| stackWordsRight ns)
      )
        `besidesR'` ( strutX' 2.0
                        ||| mappingTable ps
                        ||| strutX' 1.0
                        ||| mappingTable ps'
                    )
      where
        ws = map fst cs
        ns = map (show . snd) cs
        ls = nub . sort . concatMap fst $ cs
        ps = [(x : [], "") | x <- ls]
        ps' = [(show x, "") | x <- digitList ds]
    digNote (DigitRange x y) = note . text' $ show x ++ "-" ++ show y
    h = sortOn fst . map (\(x, y) -> (y : [], show x))
    h' = map (\(x, y) -> (show x, y : []))

kropki :: Backend' b => Drawers b (Map.Map (Edge N) KropkiDot) (Grid C Int)
kropki = Drawers (p <> n) (placeGrid . fmap int . snd <> p . fst)
  where
    p =
      placeGrid'
        . Map.mapKeys midPoint
        . fmap kropkiDot
        <> grid gDefault
        . Data.sizeGrid
        . sz
    n g = placeNoteTR (w, h) (text' ds # scale noteScale)
      where
        (w, h) = sz g
        ds = "1-" ++ show h
    sz m = Data.edgeSize m

statuepark :: Backend' b => Drawers b (Grid C (Maybe MasyuPearl)) (Grid C Bool)
statuepark = Drawers p (p . fst <> shade . snd)
  where
    p = placeGrid . fmap pearl . clues <> grid gDashed

pentominousBorders ::
  Backend' b => Drawers b (Grid C (), [Edge N]) (Grid C Char)
pentominousBorders =
  Drawers (edges . snd <> grid gDashed . fst) ((areas <> grid gDashed) . snd)

smallHintRooms :: Backend' b => (AreaGrid, Grid C (Maybe Int)) -> Drawing b
smallHintRooms =
  ( (areas <> grid gDashed)
      . fst
      <> placeGrid
      . fmap hintTL
      . fmap show
      . clues
      . snd
  )

nanroSignpost ::
  Backend' b => Drawers b (AreaGrid, Grid C (Maybe Int)) (Grid C Int)
nanroSignpost =
  Drawers smallHintRooms (placeGrid . fmap int . snd <> smallHintRooms . fst)

tomTom :: Backend' b => Drawers b (AreaGrid, Grid C (Maybe String)) (Grid C Int)
tomTom = Drawers p (placeGrid . fmap int . snd <> p . fst)
  where
    p = ((areas <> grid gDashed) . fst <> placeGrid . fmap hintTL . clues . snd)

horseSnake ::
  Backend' b => Drawers b (Grid C (Maybe (Either MEnd Int))) [Edge C]
horseSnake = Drawers p (solstyle . edges . snd <> p . fst)
  where
    p = (placeGrid . fmap (either bigEnd int) . clues <> grid gDashed)

illumination ::
  Backend' b =>
  Drawers
    b
    (OutsideClues C (Maybe Fraction))
    (Grid N (Maybe PlainNode), [Edge N])
illumination =
  Drawers
    p
    ( (placeGrid . fmap (const (smallPearl MWhite)) . clues . fst <> edges . snd)
        . snd
        <> p
        . fst
    )
  where
    p = placeOutside . fmap (fmap fraction) <> grid gDashed . Data.outsideGrid

pentopia :: Backend' b => Drawers b (Grid C (Maybe Myopia)) (Grid C Bool)
pentopia = Drawers p (p . fst <> shade . snd)
  where
    p = placeGrid . fmap myopia . clues <> grid gDefault

greaterWall ::
  Backend' b => Drawers b ([GreaterClue], [GreaterClue]) (Grid C Bool)
greaterWall =
  Drawers
    ((placeMultiOutsideGW <> grid gDefault . Data.outsideGrid) . munge)
    undefined
  where
    munge (rs, cs) =
      OC
        (map (reverse . greaterClue) (reverse rs))
        []
        []
        (map (map (rotateBy (-1 / 4))) . map (reverse . greaterClue) $ cs)

galaxies ::
  Backend' b =>
  Drawers
    b
    (Grid C (), Grid N (), Grid C (), Map.Map (Edge N) ())
    AreaGrid
galaxies = Drawers p (p . fst <> areas . snd)
  where
    p = (gals <> grid gDashed . fst4)
    gal = const (kropkiDot KWhite)
    gals (_, a, b, c) =
      (placeGrid . fmap gal $ a)
        <> (placeGrid . fmap gal $ b)
        <> (placeGrid' . fmap gal . Map.mapKeys midPoint $ c)
    fst4 (a, _, _, _) = a

mines :: Backend' b => Drawers b (Grid C (Maybe Int)) ShadedGrid
mines =
  Drawers
    p
    (p . fst <> placeGrid . fmap (const (pearl MBlack)) . Map.filter id . snd)
  where
    p =
      grid gDefault <> placeGrid . fmap (\i -> int i <> fillBG lightgray) . clues

tents ::
  Backend' b =>
  Drawers
    b
    (OutsideClues C (Maybe Int), Grid C (Maybe Tree))
    (Grid C (Maybe PlacedTent))
tents = Drawers p (p . fst <> placeGrid . fmap tent . clues . snd)
  where
    p =
      placeOutside
        . fmap (fmap int)
        . fst
        <> placeGrid
        . fmap tree
        . clues
        . snd
        <> grid gDashed
        . snd

pentominoSums ::
  Backend' b =>
  Drawers
    b
    (OutsideClues C [String], String)
    ( Grid C (Either Pentomino Int),
      [(Char, Int)],
      OutsideClues C [String]
    )
pentominoSums = Drawers p (solgrid ||| const (strutX' 1.0) ||| table)
  where
    p (ocs, ds) =
      ( ((multiOutsideGrid ocs <> n (ocs, ds)) ||| strutX' 1.0 ||| emptyTable ocs)
          `aboveT'` pentominos
      )
    n (ocs, ds) = placeNoteTL (0, h ocs) (text' ds # scale noteScale)
    h = snd . Data.outsideSize
    emptyTable = mappingTable . emptys
    emptys = map (\k -> (k, "")) . nub . sort . concat . Data.outsideValues
    solgrid =
      skel . fst3 . snd <> multiOutsideGrid . trd3 . snd <> cells . fst3 . snd
    fst3 (x, _, _) = x
    trd3 (_, _, z) = z
    skel = skeletonStyle . edges . Data.skeletons . lefts
    skeletonStyle = lc white . lwG (3 * onepix)
    lefts = clues . fmap (either Just (const Nothing))
    cells =
      placeGrid
        . fmap
          ( \v -> case v of
              Left _ -> fillBG gray
              Right x -> int x
          )
    table ((cs, _), (_, m, _)) = mappingTable m'
      where
        m' = Map.toList (Map.union (Map.fromList a) (Map.fromList (emptys cs)))
        a = map (\(k, v) -> ([k], show v)) m

coralLits ::
  Backend' b => Drawers b (OutsideClues C [String]) (Grid C (Maybe Char))
coralLits =
  Drawers
    (\ocs -> multiOutsideGrid ocs `aboveT'` lITS)
    ( skeletonStyle
        . edges
        . Data.skeletons
        . clues
        . snd
        <> multiOutsideGrid
        . fst
        <> placeGrid
        . fmap (const (fillBG gray))
        . clues
        . snd
    )
  where
    skeletonStyle = lc white . lwG (3 * onepix)

coralLitso ::
  Backend' b =>
  Drawers b (OutsideClues C [String]) (Grid C (Either Black Char))
coralLitso =
  Drawers
    (\ocs -> multiOutsideGrid ocs `aboveT'` lITSO)
    ( multiOutsideGrid
        . fst
        <> skeletonStyle
        . edges
        . Data.skeletons
        . rights
        . snd
        <> placeGrid
        . fmap (const (fillBG gray))
        . lefts
        . snd
    )
  where
    skeletonStyle = solstyle
    lefts = clues . fmap (either Just (const Nothing))
    rights = clues . fmap (either (const Nothing) Just)

snake ::
  Backend' b =>
  Drawers
    b
    (OutsideClues C (Maybe Int), Grid C (Maybe MEnd))
    (Grid C (Maybe (Either MEnd Black)))
snake = Drawers p s
  where
    cs = placeOutside . fmap (fmap int) . fst
    p = cs <> placeGrid . fmap bigEnd . clues . snd <> grid gDefault . snd
    s =
      cs
        . fst
        <> grid gDefault
        . snd
        <> placeGrid
        . fmap (either (bigEnd <> gr) gr)
        . clues
        . snd
    gr = const (fillBG gray)

countryRoad :: Backend' b => Drawers b (AreaGrid, Grid C (Maybe Int)) (Loop C)
countryRoad =
  Drawers smallHintRooms (solstyle . edges . snd <> smallHintRooms . fst)

japsummasyu :: Backend' b => Drawers b (OutsideClues C [String]) ()
japsummasyu =
  Drawers
    ( placeMultiOutside
        . fmap (fmap (scale outsideScale . text'))
        <> grid gPlainDashed
        . Data.outsideGrid
    )
    (unimplemented "japsummasyu solution")

arrowsudoku ::
  Backend' b =>
  Drawers b (AreaGrid, Grid C (Maybe Int), [Thermometer]) (Grid C Int)
arrowsudoku =
  Drawers
    ( areas
        . fst3
        <> placeGrid
        . fmap int
        . clues
        . snd3
        <> arrows
        . trd3
        <> grid gDefault
        . fst3
    )
    ( areas
        . fst3
        . fst
        <> placeGrid
        . fmap int
        . snd
        <> thermos
        . trd3
        . fst
        <> grid gDefault
        . fst3
        . fst
    )
  where
    fst3 (a, _, _) = a
    snd3 (_, b, _) = b
    trd3 (_, _, c) = c

dualloop ::
  Backend' b =>
  Drawers b (Grid C (Maybe Int), Grid N (Maybe Int)) (Loop N, Loop C)
dualloop = Drawers p (s . snd <> p . fst)
  where
    p =
      placeGrid
        . fmap int
        . clues
        . fst
        <> placeGrid
        . fmap smallClue
        . clues
        . snd
        <> grid gDashDash
        . fst
    smallClue x = scale (2 / 3) (int x <> circle 0.5 # fc white # lwG 0 # draw)
    gDashDash = GridStyle LineDashed LineDashed Nothing VertexNone
    s = solstyle . edges . fst <> solstyle . edges . snd
