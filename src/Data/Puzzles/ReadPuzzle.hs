{-# LANGUAGE OverloadedStrings #-}
module Data.Puzzles.ReadPuzzle where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Yaml
import qualified Data.HashMap.Strict as HM
import Data.Char (isAlpha)

import Data.Puzzles.Grid
import Data.Puzzles.Pyramid
import Data.Puzzles.Read
import Data.Puzzles.Things

data Puzzle = P { puzzleType :: String
                , puzzle     :: Value
                , solution   :: Value
                }
    deriving Show

instance FromJSON Puzzle where
    parseJSON (Object v) = P               <$>
                           v .: "type"     <*>
                           v .: "puzzle"   <*>
                           v .: "solution"
    parseJSON _          = mzero

data ParsedPuzzle a b = PP { pzl :: a
                           , sol :: b
                           }
    deriving Show

type LITS = ParsedPuzzle AreaGrid ShadedGrid

parseLITS :: Puzzle -> Result LITS
parseLITS (P _ p s) = PP <$>
                           (readAreaGrid <$> fromJSON p) <*>
                           (readBoolGrid <$> fromJSON s)

parseLITSPlus :: Puzzle -> Result LITS
parseLITSPlus = parseLITS

type Geradeweg = ParsedPuzzle IntGrid Loop

parseGeradeweg :: Puzzle -> Result Geradeweg
parseGeradeweg (P _ p s) = PP <$>
                           (readIntGrid <$> fromJSON p) <*>
                           (readEdges' <$> fromJSON s)

type Fillomino = ParsedPuzzle IntGrid IntGrid

parseFillomino :: Puzzle -> Result Fillomino
parseFillomino (P _ p s) = PP <$>
                           (readIntGrid <$> fromJSON p) <*>
                           (readIntGrid <$> fromJSON s)

type Masyu = ParsedPuzzle MasyuGrid Loop

parseMasyu :: Puzzle -> Result Masyu
parseMasyu (P "masyu" p s) = PP <$>
                             (readMasyuGrid <$> fromJSON p) <*>
                             (readEdges' <$> fromJSON s)

type Nurikabe = ParsedPuzzle IntGrid ShadedGrid

parseNurikabe :: Puzzle -> Result Nurikabe
parseNurikabe (P _ p s) = PP <$>
                          (readWideIntGrid <$> fromJSON p) <*>
                          (readBoolGrid <$> fromJSON s)

newtype RefGrid a = RefGrid { unRG :: Grid (Maybe a) }

merge :: CharGrid -> HM.HashMap String a -> RefGrid a
merge g m = RefGrid (f <$> g)
    where f c | isref c   = Just (m HM.! [c])
              | otherwise = Nothing
          isref = isAlpha

instance (FromJSON a) => FromJSON (RefGrid a) where
    parseJSON (Object v) = merge <$>
                           (readCharGrid <$> (v .: "grid")) <*>
                           v .: "clues"

type LatinTapa = ParsedPuzzle (Grid (Clue [String])) CharClueGrid

parseLatinTapa :: Puzzle -> Result LatinTapa
parseLatinTapa (P _ p s) = PP <$>
                           (unRG <$> fromJSON p) <*>
                           (readCharClueGrid <$> fromJSON s)

type Sudoku = ParsedPuzzle IntGrid IntGrid

parseSudoku (P _ p s) = PP <$>
                        (readIntGrid <$> fromJSON p) <*>
                        (readIntGrid <$> fromJSON s)

type ThermoSudoku = ParsedPuzzle (IntGrid, [Thermometer]) IntGrid

parseThermoSudoku :: Puzzle -> Result ThermoSudoku
parseThermoSudoku (P _ p s) = PP <$>
                        (readThermos . readCharGrid <$> fromJSON p) <*>
                        (readIntGrid <$> fromJSON s)

type PPyramid = ParsedPuzzle Pyramid Pyramid

parsePyramid :: Puzzle -> Result PPyramid
parsePyramid (P _ p s) = PP <$>
                         (readPyramid . lines <$> fromJSON p) <*>
                         (readPlainPyramid . lines <$> fromJSON s)

type PKropkiPyramid = ParsedPuzzle RowKropkiPyramid Pyramid

parseKropkiPyramid :: Puzzle -> Result PKropkiPyramid
parseKropkiPyramid (P _ p s) = PP <$>
                         (readKropkiPyramid . lines <$> fromJSON p) <*>
                         (readPlainPyramid . lines <$> fromJSON s)

type SlitherLink = ParsedPuzzle IntGrid Loop

parseSlitherLink :: Puzzle -> Result SlitherLink
parseSlitherLink (P _ p s) = PP <$>
                           (readIntGrid <$> fromJSON p) <*>
                           (readEdges' <$> fromJSON s)

newtype LSol = LSol { unLSol :: (Loop, Grid (Maybe ())) }
instance FromJSON LSol where
    parseJSON (Object v) = LSol <$> ((,) <$>
                           (readEdges' <$> v .: "loop") <*>
                           (readXGrid <$> v .: "liars"))
    parseJSON _          = mzero

type LiarSlitherLink = ParsedPuzzle IntGrid (Loop, Grid (Maybe ()))

parseLiarSlitherLink :: Puzzle -> Result LiarSlitherLink
parseLiarSlitherLink (P _ p s) = PP <$>
                                 (readIntGrid <$> fromJSON p) <*>
                                 (unLSol <$> fromJSON s)

type TightfitSkyscrapers = ParsedPuzzle (OutsideClues (Maybe Int), Grid (Tightfit ()))
                                        (Grid (Tightfit Int))

parseTightfitSkyscraper :: Puzzle -> Result TightfitSkyscrapers
parseTightfitSkyscraper (P _ p s) = PP <$>
                                    (readTightOutside <$> fromJSON p) <*>
                                    (readTightIntGrid <$> fromJSON s)

type Wordloop = ParsedPuzzle (CharClueGrid, [String]) CharClueGrid

newtype GridWords = GW { unGW :: (CharClueGrid, [String]) }

instance FromJSON GridWords where
    parseJSON (Object v) = GW <$> ((,) <$>
                                   (readCharClueGrid <$> v .: "grid") <*>
                                   v .: "words")

parseWordloop :: Puzzle -> Result Wordloop
parseWordloop (P _ p s) = PP <$>
                          (unGW <$> fromJSON p) <*>
                          (readCharClueGrid <$> fromJSON s)

type Wordsearch = ParsedPuzzle (CharClueGrid, [String]) (CharClueGrid, [MarkedWord])

instance FromJSON MarkedWord where
    parseJSON v = MW <$>
                  ((,) <$> ((!!0) <$> x) <*> ((!!1) <$> x)) <*>
                  ((,) <$> ((!!2) <$> x) <*> ((!!3) <$> x))
        where x = map read . words <$> parseJSON v :: Parser [Int]

newtype GridMarked = GM { unGM :: (CharClueGrid, [MarkedWord]) }

instance FromJSON GridMarked where
    parseJSON (Object v) = GM <$> ((,) <$>
                                   (readCharClueGrid <$> v .: "grid") <*>
                                   (v .: "words"))
    parseJSON _          = mzero

parseWordsearch :: Puzzle -> Result Wordsearch
parseWordsearch (P _ p s) = PP <$>
                            (unGW <$> fromJSON p) <*>
                            (unGM <$> fromJSON s)

type CurveData = ParsedPuzzle (Grid (Maybe [Edge])) [Edge]

newtype Curve = Curve { unCurve :: [Edge] }

instance FromJSON Curve where
    parseJSON v = Curve <$> (readEdges <$> parseJSON v)

parseCurveData :: Puzzle -> Result CurveData
parseCurveData (P _ p s) = PP <$>
                           (fmap (fmap unCurve) . unRG <$> fromJSON p) <*>
                           (readEdges <$> fromJSON s)

type DoubleBack = ParsedPuzzle AreaGrid Loop
parseDoubleBack (P _ p s) = PP <$>
                            (readAreaGrid <$> fromJSON p) <*>
                            (readEdges' <$> fromJSON s)

type Slalom = ParsedPuzzle IntGrid CharGrid
parseSlalom (P _ p s) = PP <$>
                        (readIntGrid <$> fromJSON p) <*>
                        (readCharGrid <$> fromJSON s)

type Compass = ParsedPuzzle (Grid CompassClue) AreaGrid

instance FromJSON CompassC where
    parseJSON v = comp . words <$> parseJSON v
        where c "." = Nothing
              c x   = Just (read x)
              comp [n, e, s, w] = CC (c n) (c e) (c s) (c w)

parseCompass (P _ p s) = PP <$>
                         (unRG <$> fromJSON p) <*>
                         (readAreaGrid <$> fromJSON s)
