{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- List of specific puzzle types.
module Data.PuzzleTypes
  ( PuzzleType (..),
    lookupType,
    checkType,
    typeOptions,
    isGeneric,
  )
where

import Data.List (sort)
import Data.Tuple (swap)

-- | The list of specific puzzle types we can handle.
data PuzzleType
  = LITS
  | Geradeweg
  | Fillomino
  | Masyu
  | Nurikabe
  | LatinTapa
  | Sudoku
  | ThermoSudoku
  | Pyramid
  | RowKropkiPyramid
  | SlitherLink
  | SlitherLinkLiar
  | WordLoop
  | WordSearch
  | CurveData
  | DoubleBack
  | Slalom
  | Compass
  | MeanderingNumbers
  | Tapa
  | JapaneseSums
  | Coral
  | MaximalLengths
  | Labyrinth
  | Bahnhof
  | BlackoutDominos
  | TwilightTapa
  | TapaCave
  | DominoPillen
  | AngleLoop
  | Shikaku
  | SlovakSums
  | Anglers
  | Dominos
  | FillominoCheckered
  | FillominoLoop
  | Cave
  | Numberlink
  | Skyscrapers
  | SkyscrapersStars
  | SkyscrapersFrac
  | SkyscrapersTightfit
  | TurningFences
  | Summon
  | Baca
  | Buchstabensalat
  | Doppelblock
  | SudokuDoppelblock
  | Loopki
  | Scrabble
  | Neighbors
  | Starbattle
  | Heyawake
  | Pentominous
  | ColorAkari
  | PersistenceOfMemory
  | ABCtje
  | Kropki
  | StatuePark
  | PentominousBorders
  | NanroSignpost
  | TomTom
  | Illumination
  | Pentopia
  | GreaterWall
  | Galaxies
  | Mines
  | Tents
  | PentominoSums
  | CoralLITS
  | CoralLITSO
  | Snake
  | CountryRoad
  | KillerSudoku
  | JapaneseSumsMasyu
  | ArrowSudoku
  | DualLoop
  | Yajilin
  | ShakaShaka
  | RingRing
  deriving (Show, Eq)

typeNames :: [(PuzzleType, String)]
typeNames =
  [ (LITS, "lits"),
    (Geradeweg, "geradeweg"),
    (Fillomino, "fillomino"),
    (Masyu, "masyu"),
    (Nurikabe, "nurikabe"),
    (Sudoku, "sudoku"),
    (ThermoSudoku, "thermosudoku"),
    (Pyramid, "pyramid"),
    (SlitherLink, "slitherlink"),
    (SlitherLinkLiar, "slitherlinkliar"),
    (WordSearch, "wordsearch"),
    (CurveData, "curvedata"),
    (DoubleBack, "doubleback"),
    (Slalom, "slalom"),
    (Compass, "compass"),
    (MeanderingNumbers, "meanderingnumbers"),
    (Tapa, "tapa"),
    (JapaneseSums, "japanesesums"),
    (Coral, "coral"),
    (MaximalLengths, "maximallengths"),
    (Labyrinth, "magiclabyrinth"),
    (Bahnhof, "bahnhof"),
    (BlackoutDominos, "blackout-dominos"),
    (TwilightTapa, "twilight-tapa"),
    (AngleLoop, "angleloop"),
    (Shikaku, "shikaku"),
    (SlovakSums, "slovaksums"),
    (Anglers, "anglers"),
    (Dominos, "dominos"),
    (FillominoCheckered, "fillomino-checkered"),
    (Cave, "cave"),
    (Numberlink, "numberlink"),
    (Skyscrapers, "skyscrapers"),
    (SkyscrapersFrac, "skyscrapers-fractional"),
    (SkyscrapersTightfit, "skyscrapers-tightfit"),
    (TurningFences, "turning-fences"),
    (Summon, "summon"),
    (Baca, "baca"),
    (Buchstabensalat, "buchstabensalat"),
    (Doppelblock, "doppelblock"),
    (Scrabble, "scrabble"),
    (Neighbors, "neighbors"),
    (Starbattle, "starbattle"),
    (Heyawake, "heyawake"),
    (Pentominous, "pentominous"),
    (PersistenceOfMemory, "persistenceofmemory"),
    (ABCtje, "abctje"),
    (Kropki, "kropki"),
    (StatuePark, "statuepark"),
    (PentominousBorders, "pentominous-borders"),
    (NanroSignpost, "nanro-signpost"),
    (TomTom, "tomtom"),
    (Illumination, "illumination"),
    (Pentopia, "pentopia"),
    (GreaterWall, "greaterwall"),
    (Galaxies, "galaxies"),
    (Mines, "mines"),
    (Tents, "tents"),
    (Snake, "snake"),
    (CountryRoad, "country-road"),
    (KillerSudoku, "killersudoku"),
    (ArrowSudoku, "arrowsudoku"),
    (Yajilin, "yajilin"),
    (ShakaShaka, "shakashaka"),
    (RingRing, "ringring")
  ]

obscureTypes :: [(PuzzleType, String)]
obscureTypes =
  [ (ColorAkari, "color-akari"),
    (FillominoLoop, "fillomino-loop"),
    (DominoPillen, "domino-pillen"),
    (TapaCave, "tapa-cave"),
    (WordLoop, "wordloop"),
    (RowKropkiPyramid, "rowkropkipyramid"),
    (LatinTapa, "latintapa"),
    (SkyscrapersStars, "skyscrapers-doppelstern"),
    (SudokuDoppelblock, "sudoku-doppelblock"),
    (Loopki, "loopki"),
    (PentominoSums, "pentomino-sums"),
    (CoralLITS, "coral+lits"),
    (CoralLITSO, "coral+litso"),
    (JapaneseSumsMasyu, "japanesesums-masyu"),
    (DualLoop, "dualloop")
  ]

isGeneric :: PuzzleType -> Bool
isGeneric t = case t of
  Yajilin -> True
  ShakaShaka -> True
  RingRing -> True
  _ -> False

typeAliases :: [(PuzzleType, String)]
typeAliases = [(LITS, "litsplus")]

allTypeNames :: [(PuzzleType, String)]
allTypeNames = typeNames ++ obscureTypes ++ typeAliases

typeOptions :: [String]
typeOptions = sort . map snd $ typeNames ++ typeAliases

-- | Look up a puzzle type by name.
lookupType :: String -> Maybe PuzzleType
lookupType t = lookup t (map swap allTypeNames)

checkType :: Maybe String -> Either String PuzzleType
checkType mt = case mt of
  Nothing -> Left "no puzzle type given"
  Just t -> case lookupType t of
    Nothing -> Left $ "unknown puzzle type: " ++ t
    Just tt -> Right tt
