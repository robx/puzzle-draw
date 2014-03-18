{-# LANGUAGE OverloadedStrings #-}

module Data.Puzzles.ReadPuzzle (
    TypedPuzzle(..),
    puzzleType,
    dropType,
    RawPuzzle,
    ReadPuzzle,

    lits, litsplus, geradeweg, fillomino, masyu, nurikabe, latintapa,
    sudoku, thermosudoku, pyramid, kpyramid, slither,
    liarslither, tightfitskyscrapers, wordloop, wordsearch,
    curvedata, doubleback, slalom, compass,

    geradeweg'
    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (parse)

import Data.Yaml
import qualified Data.HashMap.Strict as HM
import Data.Char (isAlpha)

import Data.Puzzles.Grid
import qualified Data.Puzzles.Pyramid as Pyr
import Data.Puzzles.Read
import Data.Puzzles.Things
import Data.Puzzles.PuzzleTypes

data TypedPuzzle = TP String Value Value
    deriving Show

puzzleType :: TypedPuzzle -> String
puzzleType (TP t _ _) = t

instance FromJSON TypedPuzzle where
    parseJSON (Object v) = TP              <$>
                           v .: "type"     <*>
                           v .: "puzzle"   <*>
                           v .: "solution"
    parseJSON _          = mzero

data RawPuzzle = RP Value Value
    deriving Show

dropType :: TypedPuzzle -> RawPuzzle
dropType (TP _ p s) = RP p s

type ReadPuzzle a = RawPuzzle -> Result a

-- | A pair of parsers for a puzzle type.
-- First parses the puzzle, second the solution.
type ParsePuzzle a b = (Value -> Parser a, Value -> Parser b)

toRead :: ParsePuzzle a b -> ReadPuzzle (PuzzleDef a b)
toRead (pp, ps) (RP p s) = PD <$> parse pp p <*> parse ps s

lits :: ReadPuzzle LITS
lits (RP p s) = PD <$>
    (readAreaGrid <$> fromJSON p) <*>
    (readBoolGrid <$> fromJSON s)

litsplus :: ReadPuzzle LITS
litsplus = lits

geradeweg' :: ParsePuzzle (SGrid (Clue Int)) Loop
geradeweg' = (parseClueGrid, parseEdges)

geradeweg :: ReadPuzzle Geradeweg
geradeweg = toRead geradeweg'

fillomino :: ReadPuzzle Fillomino
fillomino (RP p s) = PD <$>
    (readIntGrid <$> fromJSON p) <*>
    (readIntGrid <$> fromJSON s)

masyu' :: ParsePuzzle (SGrid (Clue MasyuPearl)) Loop
masyu' = (parseClueGrid, parseEdges)

masyu :: ReadPuzzle Masyu
masyu = toRead masyu'

nurikabe :: ReadPuzzle Nurikabe
nurikabe (RP p s) = PD <$>
    (readWideIntGrid <$> fromJSON p) <*>
    (readBoolGrid <$> fromJSON s)

newtype RefGrid a = RefGrid { unRG :: SGrid (Maybe a) }

merge :: CharGrid -> HM.HashMap String a -> RefGrid a
merge g m = RefGrid (f <$> g)
    where f c | isref c   = Just (m HM.! [c])
              | otherwise = Nothing
          isref = isAlpha

instance (FromJSON a) => FromJSON (RefGrid a) where
    parseJSON (Object v) = merge <$>
                           (readCharGrid <$> (v .: "grid")) <*>
                           v .: "clues"

latintapa :: ReadPuzzle LatinTapa
latintapa (RP p s) = PD <$>
    (unRG <$> fromJSON p) <*>
    (readCharClueGrid <$> fromJSON s)

sudoku :: ReadPuzzle Sudoku
sudoku (RP p s) = PD <$>
    (readIntGrid <$> fromJSON p) <*>
    (readIntGrid <$> fromJSON s)

thermosudoku :: ReadPuzzle ThermoSudoku
thermosudoku (RP p s) = PD <$>
    (readThermos . readCharGrid <$> fromJSON p) <*>
    (readIntGrid <$> fromJSON s)

pyramid :: ReadPuzzle Pyramid
pyramid (RP p s) = PD <$>
    (Pyr.readPyramid . lines <$> fromJSON p) <*>
    (Pyr.readPlainPyramid . lines <$> fromJSON s)

kpyramid :: ReadPuzzle RowKropkiPyramid
kpyramid (RP p s) = PD <$>
    (Pyr.readKropkiPyramid . lines <$> fromJSON p) <*>
    (Pyr.readPlainPyramid . lines <$> fromJSON s)

slither :: ReadPuzzle SlitherLink
slither (RP p s) = PD <$>
    (readIntGrid <$> fromJSON p) <*>
    (readEdges' <$> fromJSON s)

newtype LSol = LSol { unLSol :: (Loop, SGrid (Maybe ())) }
instance FromJSON LSol where
    parseJSON (Object v) = LSol <$> ((,) <$>
                           (readEdges' <$> v .: "loop") <*>
                           (readXGrid <$> v .: "liars"))
    parseJSON _          = mzero

liarslither :: ReadPuzzle LiarSlitherLink
liarslither (RP p s) = PD <$>
    (readIntGrid <$> fromJSON p) <*>
    (unLSol <$> fromJSON s)

tightfitskyscrapers :: ReadPuzzle TightfitSkyscrapers
tightfitskyscrapers (RP p s) = PD <$>
    (readTightOutside <$> fromJSON p) <*>
    (readTightIntGrid <$> fromJSON s)

newtype GridWords = GW { unGW :: (CharClueGrid, [String]) }

instance FromJSON GridWords where
    parseJSON (Object v) = GW <$> ((,) <$>
                                   (readCharClueGrid <$> v .: "grid") <*>
                                   v .: "words")

wordloop :: ReadPuzzle Wordloop
wordloop (RP p s) = PD <$>
    (unGW <$> fromJSON p) <*>
    (readCharClueGrid <$> fromJSON s)

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

wordsearch :: ReadPuzzle Wordsearch
wordsearch (RP p s) = PD <$>
    (unGW <$> fromJSON p) <*>
    (unGM <$> fromJSON s)

newtype Curve = Curve { unCurve :: [Edge] }

instance FromJSON Curve where
    parseJSON v = Curve <$> (readEdges <$> parseJSON v)

curvedata :: ReadPuzzle CurveData
curvedata (RP p s) = PD <$>
    (fmap (fmap unCurve) . unRG <$> fromJSON p) <*>
    (readEdges <$> fromJSON s)

doubleback :: ReadPuzzle DoubleBack
doubleback (RP p s) = PD <$>
    (readAreaGrid <$> fromJSON p) <*>
    (readEdges' <$> fromJSON s)

slalom :: ReadPuzzle Slalom
slalom (RP p s) = PD <$>
    (readIntGrid <$> fromJSON p) <*>
    (readCharGrid <$> fromJSON s)

instance FromJSON CompassC where
    parseJSON v = comp . words <$> parseJSON v
        where c "." = Nothing
              c x   = Just (read x)
              comp [n, e, s, w] = CC (c n) (c e) (c s) (c w)

compass :: ReadPuzzle Compass
compass (RP p s) = PD <$>
    (unRG <$> fromJSON p) <*>
    (readAreaGrid <$> fromJSON s)
