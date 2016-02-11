{-# LANGUAGE FlexibleContexts, RankNTypes #-}

-- |
-- List of specific puzzle types.

module Data.Puzzles.PuzzleTypes (
    PuzzleType(..)
  , lookupType
  , typeNames
  ) where

import Data.Tuple (swap)

-- | The list of specific puzzle types we can handle.
data PuzzleType = LITS
                | LITSPlus
                | LITSSym
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
                | TightfitSkyscrapers
                | WordLoop
                | WordSearch
                | CurveData
                | DoubleBack
                | Slalom
                | Compass
                | BoxOf2Or3
                | AfternoonSkyscrapers
                | MeanderingNumbers
                | Tapa
                | JapaneseSums
                | Coral
                | MaximalLengths
                | PrimePlace
                | Labyrinth
                | Bahnhof
                | BlackoutDominos
                | TwilightTapa
                | TapaCave
                | DominoPillen
                | SlitherLinkMulti
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
                | TurningFences
                | Summon
                | Baca
                | Buchstabensalat
                | Doppelblock
                | SudokuDoppelblock
                | Loopki
                | Scrabble
                | Neighbors
                | Starwars
                | Starbattle
                | Heyawake
                | Wormhole
                | Pentominous
                | ColorAkari
                | PersistenceOfMemory
    deriving (Show, Eq)

typeNames :: [(PuzzleType, String)]
typeNames = [ (LITS, "lits")
            , (LITSPlus, "litsplus")
            , (LITSSym, "lits-symmetry")
            , (Geradeweg, "geradeweg")
            , (Fillomino, "fillomino")
            , (Masyu, "masyu")
            , (Nurikabe, "nurikabe")
            , (LatinTapa, "latintapa")
            , (Sudoku, "sudoku")
            , (ThermoSudoku, "thermosudoku")
            , (Pyramid, "pyramid")
            , (RowKropkiPyramid, "rowkropkipyramid")
            , (SlitherLink, "slitherlink")
            , (SlitherLinkLiar, "slitherlinkliar")
            , (TightfitSkyscrapers, "skyscrapers-tightfit")
            , (WordLoop, "wordloop")
            , (WordSearch, "wordsearch")
            , (CurveData, "curvedata")
            , (DoubleBack, "doubleback")
            , (Slalom, "slalom")
            , (Compass, "compass")
            , (BoxOf2Or3, "boxof2or3")
            , (AfternoonSkyscrapers, "afternoonskyscrapers")
            , (MeanderingNumbers, "meanderingnumbers")
            , (Tapa, "tapa")
            , (JapaneseSums, "japanesesums")
            , (Coral, "coral")
            , (MaximalLengths, "maximallengths")
            , (PrimePlace, "primeplace")
            , (Labyrinth, "magiclabyrinth")
            , (Bahnhof, "bahnhof")
            , (BlackoutDominos, "blackout-dominos")
            , (TwilightTapa, "twilight-tapa")
            , (TapaCave, "tapa-cave")
            , (DominoPillen, "domino-pillen")
            , (SlitherLinkMulti, "slitherlink-multi")
            , (AngleLoop, "angleloop")
            , (Shikaku, "shikaku")
            , (SlovakSums, "slovaksums")
            , (Anglers, "anglers")
            , (Dominos, "dominos")
            , (FillominoCheckered, "fillomino-checkered")
            , (FillominoLoop, "fillomino-loop")
            , (Cave, "cave")
            , (Numberlink, "numberlink")
            , (Skyscrapers, "skyscrapers")
            , (SkyscrapersStars, "skyscrapers-doppelstern")
            , (SkyscrapersFrac, "skyscrapers-fractional")
            , (TurningFences, "turning-fences")
            , (Summon, "summon")
            , (Baca, "baca")
            , (Buchstabensalat, "buchstabensalat")
            , (Doppelblock, "doppelblock")
            , (SudokuDoppelblock, "sudoku-doppelblock")
            , (Loopki, "loopki")
            , (Scrabble, "scrabble")
            , (Neighbors, "neighbors")
            , (Starwars, "starwars")
            , (Starbattle, "starbattle")
            , (Heyawake, "heyawake")
            , (Wormhole, "wormhole")
            , (Pentominous, "pentominous")
            , (ColorAkari, "color-akari")
            , (PersistenceOfMemory, "persistenceofmemory")
            ]

-- | Look up a puzzle type by name.
lookupType :: String -> Maybe PuzzleType
lookupType t = lookup t (map swap typeNames)
