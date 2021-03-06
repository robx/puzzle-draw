{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Parse.PuzzleTypes
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
    yajilin,
    shakashaka,
    ringring,
  )
where

import Control.Applicative
import Control.Monad
import Data.Elements
import Data.Grid
import Data.GridShape
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Pyramid as Pyr
import Data.Yaml
import Parse.Puzzle
import Parse.Util

unimplemented :: String -> Value -> Parser ()
unimplemented _ _ = pure ()

lits :: ParsePuzzle AreaGrid (Grid C Bool)
lits = (parseGrid, parseShadedGrid)

geradeweg :: ParsePuzzle (Grid C (Maybe Int)) (Loop C)
geradeweg = (parseClueGrid, parseEdges)

fillomino :: ParsePuzzle (Grid C (Maybe Int)) (Grid C Int)
fillomino = (parseExtClueGrid, parseExtGrid)

fillominoLoop :: ParsePuzzle (Grid C (Maybe Int)) (Grid C Int, Loop C)
fillominoLoop =
  (,)
    parseClueGrid
    ( \v ->
        (,)
          <$> parseFrom ["grid"] parseExtGrid v
          <*> parseFrom ["loop"] parseEdges v
    )

masyu :: ParsePuzzle (Grid C (Maybe MasyuPearl)) (Loop C)
masyu = (parseClueGrid, parseEdges)

nurikabe :: ParsePuzzle (Grid C (Maybe Int)) (Grid C Bool)
nurikabe = (parseExtClueGrid, parseShadedGrid)

latintapa :: ParsePuzzle (Grid C (Maybe [String])) (Grid C (Maybe Char))
latintapa =
  ((unRG <$>) . parseJSON, fmap (fmap (fmap unAlpha)) . parseClueGrid')

sudoku :: ParsePuzzle (Grid C (Maybe Int)) (Grid C (Maybe Int))
sudoku = (parseClueGrid, parseClueGrid)

thermosudoku ::
  ParsePuzzle (Grid C (Maybe Int), [Thermometer]) (Grid C (Maybe Int))
thermosudoku = ((parseThermoGrid =<<) . parseJSON, parseClueGrid)

killersudoku ::
  ParsePuzzle (AreaGrid, Map Char Int, Grid C (Maybe Int)) (Grid C Int)
killersudoku =
  (,)
    ( \v ->
        (,,)
          <$> parseFrom ["cages"] parseGrid v
          <*> parseFrom ["clues"] parseCharMap v
          <*> (parseFrom ["grid"] parseClueGrid v <|> pure Map.empty)
    )
    parseGrid

pyramid :: ParsePuzzle Pyr.Pyramid Pyr.PyramidSol
pyramid = (parseJSON, parseJSON)

kpyramid :: ParsePuzzle Pyr.RowKropkiPyramid Pyr.PyramidSol
kpyramid = (parseJSON, parseJSON)

slither :: ParsePuzzle (Grid C (Clue Int)) (Loop N)
slither = (parseClueGrid, parseEdges)

newtype LSol = LSol {unLSol :: (Loop N, Grid C Bool)}

instance FromJSON LSol where
  parseJSON (Object v) =
    LSol
      <$> ( (,)
              <$> (parseEdges =<< v .: "loop")
              <*> (parseShadedGrid =<< v .: "liars")
          )
  parseJSON _ = mzero

liarslither :: ParsePuzzle (Grid C (Maybe Int)) (Loop N, Grid C Bool)
liarslither = (parseClueGrid, (unLSol <$>) . parseJSON)

tightfitskyscrapers ::
  ParsePuzzle
    (OutsideClues C (Maybe Int), Grid C (Tightfit ()))
    (Grid C (Tightfit Int))
tightfitskyscrapers = (parseTightOutside, parseSpacedGrid)

newtype GridWords = GW {unGW :: (Grid C (Maybe Char), [String])}

instance FromJSON GridWords where
  parseJSON (Object v) =
    GW
      <$> ( (,)
              <$> (fmap blankToMaybe <$> (parseIrregGrid =<< v .: "grid"))
              <*> v .: "words"
          )
  parseJSON _ = empty

wordloop :: ParsePuzzle (Grid C (Maybe Char), [String]) (Grid C (Maybe Char))
wordloop = ((unGW <$>) . parseJSON, parseClueGrid)

newtype GridMarked = GM {unGM :: (Grid C (Maybe Char), [MarkedWord])}

instance FromJSON GridMarked where
  parseJSON (Object v) =
    GM
      <$> ( (,)
              <$> (fmap blankToMaybe <$> (parseIrregGrid =<< v .: "grid"))
              <*> (map unPMW <$> v .: "words")
          )
  parseJSON _ = mzero

wordsearch ::
  ParsePuzzle
    (Grid C (Maybe Char), [String])
    (Grid C (Maybe Char), [MarkedWord])
wordsearch = ((unGW <$>) . parseJSON, (unGM <$>) . parseJSON)

newtype Curve = Curve {unCurve :: [Edge N]}

instance FromJSON Curve where
  parseJSON v = Curve <$> parseEdges v

curvedata :: ParsePuzzle (Grid C (Maybe [Edge N])) [Edge C]
curvedata = ((fmap (fmap unCurve) . unRG <$>) . parseJSON, parseEdges)

doubleback :: ParsePuzzle AreaGrid (Loop C)
doubleback = (parseGrid, parseEdges)

slalom :: ParsePuzzle (Grid N (Maybe Int)) (Grid C SlalomDiag)
slalom = (parseClueGrid, parseGrid)

compass :: ParsePuzzle (Grid C (Maybe CompassC)) AreaGrid
compass = ((fmap (fmap unPCC) . unRG <$>) . parseJSON, parseGrid)

meanderingnumbers ::
  ParsePuzzle (AreaGrid, Grid C (Maybe Int)) (Grid C (Maybe Int))
meanderingnumbers =
  (,)
    ( \v ->
        (,)
          <$> parseFrom ["regions"] parseGrid v
          <*> parseExtClueGridFrom ["clues"] ["replace"] v
    )
    parseGrid

tapa :: ParsePuzzle (Grid C (Maybe TapaClue)) (Grid C Bool)
tapa =
  (\v -> fmap (fmap unParseTapaClue) . unRG <$> parseJSON v, parseShadedGrid)

japanesesums ::
  ParsePuzzle (OutsideClues C [Int], String) (Grid C (Either Black Int))
japanesesums = (p, parseGrid)
  where
    p v@(Object o) = (,) <$> parseMultiOutsideClues v <*> o .: "digits"
    p _ = empty

coral :: ParsePuzzle (OutsideClues C [String]) (Grid C Bool)
coral =
  (,) (fmap (fmap (map unIntString)) . parseMultiOutsideClues) parseShadedGrid

maximallengths :: ParsePuzzle (OutsideClues C (Maybe Int)) (Loop C)
maximallengths = (\v -> fmap blankToMaybe <$> parseCharOutside v, parseEdges)

labyrinth ::
  ParsePuzzle (Grid C (Maybe Int), [Edge N], String) (Grid C (Maybe Int))
labyrinth = (p, parseClueGrid')
  where
    p v@(Object o) =
      tup <$> parseFrom ["grid"] parseCellEdges v <*> o .: "digits"
    p _ = mempty
    tup (x, y) z = (x, y, z)

bahnhof :: ParsePuzzle (Grid C (Maybe BahnhofClue)) [Edge C]
bahnhof = (parseClueGrid, parseEdges)

blackoutDominos ::
  ParsePuzzle (Grid C (Clue Int), DigitRange) (Grid C (Clue Int), AreaGrid)
blackoutDominos =
  (,)
    ( \v ->
        (,)
          <$> parseFrom ["grid"] parseIrregGrid v
          <*> parseFrom ["digits"] parseStringJSON v
    )
    ( \v ->
        (,)
          <$> parseFrom ["values"] parseIrregGrid v
          <*> parseFrom ["dominos"] parseIrregGrid v
    )

angleLoop :: ParsePuzzle (Grid N (Clue Int)) VertexLoop
angleLoop = (parseClueGrid, parseCoordLoop)

shikaku :: ParsePuzzle (Grid C (Maybe Int)) AreaGrid
shikaku = (parseExtClueGrid, parseGrid)

slovaksums ::
  ParsePuzzle (Grid C (Maybe SlovakClue), String) (Grid C (Maybe Int))
slovaksums = (p, parseClueGrid)
  where
    p v@(Object o) = (,) <$> g v <*> o .: "digits"
    p _ = empty
    g = (fmap (fmap unPSlovakClue) . unRG <$>) . parseJSON

anglers ::
  ParsePuzzle (OutsideClues C (Maybe Int), Grid C (Maybe Fish)) [Edge C]
anglers =
  ( parseOutsideGridMap blankToMaybe blankToMaybe',
    \v -> map (shift (-1, -1)) <$> parseEdges v
  )

cave :: ParsePuzzle (Grid C (Maybe Int)) (Grid C Bool)
cave = (parseExtClueGrid, parseShadedGrid)

parseOut :: FromJSON a => Value -> Parser (OutsideClues k (Maybe a))
parseOut v = fmap (blankToMaybe' . unEither') <$> parseOutside v

skyscrapers ::
  ParsePuzzle
    ( OutsideClues C (Maybe Int),
      Maybe String,
      Maybe (Grid C (Maybe Int))
    )
    (Grid C (Maybe Int))
skyscrapers =
  (,)
    ( \v ->
        (,,) <$> parseOut v
          <*> parseFromOptional ["digits"] parseJSON v
          <*> parseFromOptional ["grid"] parseClueGrid v
    )
    parseClueGrid

summon ::
  ParsePuzzle
    (AreaGrid, OutsideClues C (Maybe Int), String)
    (Grid C (Maybe Int))
summon =
  ( \v@(Object o) ->
      (,,)
        <$> parseFrom ["grid"] parseGrid v
        <*> parseFrom ["outside"] parseOut v
        <*> o
        .: "digits",
    parseClueGrid
  )

baca ::
  ParsePuzzle
    (Grid C (Maybe Char), OutsideClues C [Int], OutsideClues C (Maybe Char))
    (Grid C (Either Black Char))
baca =
  ( \v ->
      (,,)
        <$> parseFrom ["grid"] parseClueGrid v
        <*> parseFrom ["outside"] parseTopLeft v
        <*> parseFrom ["outside"] parseBottomRight v,
    parseGrid
  )
  where
    parseTopLeft (Object v) = do
      l <- reverse <$> v .: "left"
      t <- v .: "top"
      return $ OC (map reverse l) [] [] (map reverse t)
    parseTopLeft _ = empty
    parseBottomRight (Object v) = do
      b <- v .: "bottom"
      r <- reverse <$> v .: "right"
      oc <- OC [] <$> parseLine r <*> parseLine b <*> pure []
      return $ fmap blankToMaybe' oc
    parseBottomRight _ = empty

buchstabensalat ::
  ParsePuzzle (OutsideClues C (Maybe Char), String) (Grid C (Maybe Char))
buchstabensalat = (p, fmap (fmap blankToMaybe') . parseGrid)
  where
    p v =
      (,)
        <$> (fmap blankToMaybe <$> parseCharOutside v)
        <*> parseFrom ["letters"] parseJSON v

doppelblock ::
  ParsePuzzle (OutsideClues C (Maybe Int)) (Grid C (Either Black Int))
doppelblock =
  (\v -> fmap (blankToMaybe' . unEither') <$> parseOutside v, parseGrid)

sudokuDoppelblock ::
  ParsePuzzle
    (AreaGrid, OutsideClues C (Maybe Int))
    (Grid C (Either Black Int))
sudokuDoppelblock =
  ( \v ->
      (,)
        <$> parseFrom ["grid"] parseGrid v
        <*> parseFrom ["outside"] parseOutInts v,
    parseGrid
  )
  where
    parseOutInts v = fmap (blankToMaybe' . unEither') <$> parseOutside v

dominos :: ParsePuzzle (Grid C (Maybe Int), DigitRange) AreaGrid
dominos = (p, parseGrid)
  where
    p v =
      (,)
        <$> parseFrom ["grid"] parseClueGrid v
        <*> parseFrom ["digits"] parseStringJSON v

dominoPills :: ParsePuzzle (Grid C (Maybe Int), DigitRange, DigitRange) AreaGrid
dominoPills = (p, parseGrid)
  where
    p v =
      (,,)
        <$> parseFrom ["grid"] parseClueGrid v
        <*> parseFrom ["digits"] parseStringJSON v
        <*> parseFrom ["pills"] parseStringJSON v

numberlink :: ParsePuzzle (Grid C (Maybe Int)) [Edge C]
numberlink = (p, fmap collectLines . p)
  where
    p = fmap (fmap (blankToMaybe . unEither')) . parseExtGrid

loopki :: ParsePuzzle (Grid C (Maybe MasyuPearl)) (Loop N)
loopki = (parseClueGrid, parseEdges)

scrabble :: ParsePuzzle (Grid C Bool, [String]) (Grid C (Maybe Char))
scrabble = (p, parseClueGrid)
  where
    p v =
      (,)
        <$> parseFrom ["grid"] parseStarGrid v
        <*> parseFrom ["words"] parseJSON v
    parseStarGrid v = fmap ((==) '*') <$> parseGrid v

neighbors :: ParsePuzzle (Grid C Bool, Grid C (Maybe Int)) (Grid C Int)
neighbors = (p, parseGrid)
  where
    p v =
      (,)
        <$> parseFrom ["shading"] parseShadedGrid v
        <*> parseFrom ["clues"] parseGrid v

starbattle :: ParsePuzzle (AreaGrid, Int) (Grid C (Maybe Star))
starbattle = (p, parseClueGrid)
  where
    p v@(Object o) = (,) <$> parseFrom ["grid"] parseGrid v <*> o .: "stars"
    p _ = empty

heyawake :: ParsePuzzle (AreaGrid, Grid C (Maybe Int)) (Grid C Bool)
heyawake = (p, parseShadedGrid)
  where
    p v =
      (,)
        <$> parseFrom ["rooms"] parseGrid v
        <*> parseFrom ["clues"] parseClueGrid v

pentominous :: ParsePuzzle (Grid C (Maybe Char)) (Grid C Char)
pentominous = (,) parseClueGrid parseGrid

colorakari :: ParsePuzzle (Grid C (Maybe Char)) (Grid C (Maybe Char))
colorakari = (,) parseClueGrid parseClueGrid

persistenceOfMemory :: ParsePuzzle (AreaGrid, Grid C (Maybe MEnd)) (Loop C)
persistenceOfMemory = (p, parseEdges)
  where
    p v = do
      g <- parseGrid v
      return (areas g, ends_ g)
    areas =
      fmap
        ( \c -> case c of
            'o' -> '.'
            _ -> c
        )
    ends_ =
      fmap
        ( \c -> case c of
            'o' -> Just MEnd
            _ -> Nothing
        )

{-
  parsing the mappings in order, from something like

  - 1: A
  - 3: B
  - 2: X
-}
abctje :: ParsePuzzle (DigitRange, [(String, Int)]) [(Int, Char)]
abctje =
  (,)
    ( \v ->
        (,) <$> parseFrom ["numbers"] parseStringJSON v <*> parseFrom ["clues"] pl v
    )
    (\v -> pl v >>= sequence . map x)
  where
    pl :: FromJSON b => Value -> Parser [(String, b)]
    pl v = parseJSON v >>= sequence . map pair
    x :: FromString a => (String, b) -> Parser (a, b)
    x (k, v) = (\k' -> (k', v)) <$> parseString k
    pair :: Map a b -> Parser (a, b)
    pair m = if Map.size m == 1 then (return . head . Map.toList $ m) else empty

kropki :: ParsePuzzle (Map (Edge N) KropkiDot) (Grid C Int)
kropki = (,) parseAnnotatedEdges parseGrid

statuepark :: ParsePuzzle (Grid C (Maybe MasyuPearl)) (Grid C Bool)
statuepark = (\v -> parseFrom ["grid"] parseClueGrid v, parseShadedGrid)

pentominousBorders :: ParsePuzzle (Grid C (), [Edge N]) (Grid C Char)
pentominousBorders = (,) parseCellEdges parseGrid

nanroSignpost :: ParsePuzzle (AreaGrid, Grid C (Maybe Int)) (Grid C Int)
nanroSignpost =
  (,)
    ( \v ->
        (,) <$> parseFrom ["rooms"] parseGrid v <*> parseFrom ["clues"] parseGrid v
    )
    parseGrid

tomTom :: ParsePuzzle (AreaGrid, Grid C (Maybe String)) (Grid C Int)
tomTom =
  (,)
    ( \v ->
        (,)
          <$> parseFrom ["rooms"] parseGrid v
          <*> parseFrom ["clues"] ((unRG <$>) . parseJSON) v
    )
    parseGrid

illumination ::
  ParsePuzzle
    (OutsideClues C (Maybe Fraction))
    (Grid N (Maybe PlainNode), [Edge N])
illumination = (,) (fmap (fmap (fmap unPFraction)) . parseOut) parseNodeEdges

newtype Myo = Myo {unMyo :: Myopia}

instance FromJSON Myo where
  parseJSON v = do
    s <- parseJSON v
    fmap Myo . sequence . map parseChar $ s

pentopia :: ParsePuzzle (Grid C (Maybe Myopia)) (Grid C Bool)
pentopia =
  (,) (fmap (fmap (fmap unMyo)) . fmap unRG . parseJSON) parseShadedGrid

greaterWall :: ParsePuzzle ([GreaterClue], [GreaterClue]) (Grid C Bool)
greaterWall =
  (,)
    ( \v ->
        (,)
          <$> parseFrom ["rows"] parseGreaterClues v
          <*> parseFrom ["columns"] parseGreaterClues v
    )
    parseShadedGrid

galaxies ::
  ParsePuzzle (Grid C (), Grid N (), Grid C (), Map (Edge N) ()) AreaGrid
galaxies =
  (,)
    ( \v -> do
        (a, b, c) <- parseEdgeGrid v
        return $ (fmap (const ()) b, f a, f b, f c)
    )
    parseGrid
  where
    toUnit GalaxyCentre = ()
    f = fmap toUnit . Map.mapMaybe id . fmap blankToMaybe''

mines :: ParsePuzzle (Grid C (Maybe Int)) (Grid C Bool)
mines = (parseIrregGrid, parseShadedGrid)

tents ::
  ParsePuzzle
    (OutsideClues C (Maybe Int), Grid C (Maybe Tree))
    (Grid C (Maybe PlacedTent))
tents = (p, fmap (fmap fromTentOrTree) . parseClueGrid)
  where
    fromTentOrTree :: Maybe (Either Tree PlacedTent) -> Maybe PlacedTent
    fromTentOrTree = maybe Nothing (either (const Nothing) Just)
    p v =
      (,)
        <$> parseFrom ["clues"] parseOut v
        <*> parseFrom ["grid"] parseClueGrid v

pentominoSums ::
  ParsePuzzle
    (OutsideClues C [String], String)
    (Grid C (Either Pentomino Int), [(Char, Int)], OutsideClues C [String])
pentominoSums = (p, s)
  where
    p v@(Object o) = (,) <$> (fst coral) v <*> o .: "digits"
    p _ = empty
    s v =
      (,,)
        <$> parseFrom ["grid"] parseGrid v
        <*> parseFrom ["values"] values v
        <*> fst coral v
    values v = parseJSON v >>= sequence . map parseKey . Map.toList
    parseKey (k, v) = (,) <$> parseString k <*> pure v

coralLits :: ParsePuzzle (OutsideClues C [String]) (Grid C (Maybe Char))
coralLits = (,) (fst coral) (fmap (fmap (fmap unAlpha)) . parseClueGrid)

coralLitso :: ParsePuzzle (OutsideClues C [String]) (Grid C (Either Black Char))
coralLitso = (,) (fst coral) (fmap (fmap (fmap unAlpha)) . parseGrid)

snake ::
  ParsePuzzle
    (OutsideClues C (Maybe Int), Grid C (Maybe MEnd))
    (Grid C (Maybe (Either MEnd Black)))
snake = (p, parseClueGrid)
  where
    p v =
      (,)
        <$> parseFrom ["clues"] parseOut v
        <*> parseFrom ["grid"] parseClueGrid v

countryRoad :: ParsePuzzle (AreaGrid, Grid C (Maybe Int)) (Loop C)
countryRoad = (,) (fst nanroSignpost) parseEdges

japsummasyu :: ParsePuzzle (OutsideClues C [String]) ()
japsummasyu =
  (,)
    (fmap (fmap (map unIntString)) . parseMultiOutsideClues)
    (unimplemented "japsummasyu solution")

arrowsudoku ::
  ParsePuzzle (AreaGrid, Grid C (Maybe Int), [Thermometer]) (Grid C Int)
arrowsudoku =
  (,)
    ( \v ->
        (,,)
          <$> parseFrom ["regions"] parseGrid v
          <*> parseFrom ["givens"] parseClueGrid v
          <*> ( do
                  g <- parseFrom ["arrows"] parseJSON v
                  snd <$> parseThermoGrid g
              )
    )
    parseGrid

dualloop :: ParsePuzzle (Grid C (Clue Int), Grid N (Clue Int)) (Loop N, Loop C)
dualloop =
  (,)
    ( \v ->
        (,)
          <$> parseFrom ["edges"] parseClueGrid v
          <*> parseFrom ["dual"] parseClueGrid v
    )
    ( \v ->
        (,) <$> parseFrom ["edges"] parseEdges v <*> parseFrom ["dual"] parseEdges v
    )

yajilin ::
  ParsePuzzle (Grid C (Maybe (Maybe (Int, Dir')))) (Grid C Bool, Loop C)
yajilin =
  (,)
    ( \v -> do
        replace <-
          parseFrom
            ["clues"]
            (parseCharMapWith (fmap Just . parseYajClue))
            v
        parseFrom
          ["grid"]
          (parseGridWith (parseYajOrBlank (`Map.lookup` replace)))
          v
    )
    (\v -> unShade . toCells <$> parseNodeEdges v)
  where
    parseYajOrBlank repl c = case c of
      '.' -> pure Nothing
      _ -> pure $ repl c
    parseYajClue s = case words s of
      [a, b] -> (,) <$> parseString a <*> parseDir b
      _ -> fail "expected <number><space><direction>"
    parseDir s = case s of
      "right" -> pure R
      "left" -> pure L
      "up" -> pure U
      "down" -> pure D
      _ -> fail "expected right/left/up/down"
    unShade (g, l) = (unShaded <$> g, l)
    toCell :: N -> C
    toCell = fromCoord . toCoord
    toCells :: (Grid N a, [Edge N]) -> (Grid C a, [Edge C])
    toCells (x, y) = (Map.mapKeys toCell x, map (mapEdge toCell) y)

shakashaka ::
  ParsePuzzle (Grid C (Maybe (Either Black Int))) ()
shakashaka =
  (,)
    parseClueGrid
    (unimplemented "shakashaka solution")

ringring ::
  ParsePuzzle (Grid C (Maybe Black)) [Edge C]
ringring =
  (,)
    parseClueGrid
    parseEdges
