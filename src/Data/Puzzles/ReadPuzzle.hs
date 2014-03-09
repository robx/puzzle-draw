{-# LANGUAGE OverloadedStrings #-}

module Data.Puzzles.ReadPuzzle (
    TypedPuzzle(..),
    puzzleType,
    dropType,

    lits, litsplus, geradeweg, fillomino, masyu, nurikabe, latintapa,
    sudoku, thermosudoku, pyramid, kpyramid, slither,
    liarslither, tightfitskyscrapers, wordloop, wordsearch,
    curvedata, doubleback, slalom, compass
    ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
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

data RawPuzzle = RP Value Value
    deriving Show

dropType :: TypedPuzzle -> RawPuzzle
dropType (TP _ p s) = RP p s

instance FromJSON TypedPuzzle where
    parseJSON (Object v) = TP              <$>
                           v .: "type"     <*>
                           v .: "puzzle"   <*>
                           v .: "solution"
    parseJSON _          = mzero

lits :: RawPuzzle -> Result LITS
lits (RP p s) = PD <$>
    (readAreaGrid <$> fromJSON p) <*>
    (readBoolGrid <$> fromJSON s)

litsplus :: RawPuzzle -> Result LITS
litsplus = lits

geradeweg :: RawPuzzle -> Result Geradeweg
geradeweg (RP p s) = PD <$>
    (readIntGrid <$> fromJSON p) <*>
    (readEdges' <$> fromJSON s)

fillomino :: RawPuzzle -> Result Fillomino
fillomino (RP p s) = PD <$>
    (readIntGrid <$> fromJSON p) <*>
    (readIntGrid <$> fromJSON s)

masyu :: RawPuzzle -> Result Masyu
masyu (RP p s) = PD <$>
    (readMasyuGrid <$> fromJSON p) <*>
    (readEdges' <$> fromJSON s)

nurikabe :: RawPuzzle -> Result Nurikabe
nurikabe (RP p s) = PD <$>
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

latintapa :: RawPuzzle -> Result LatinTapa
latintapa (RP p s) = PD <$>
    (unRG <$> fromJSON p) <*>
    (readCharClueGrid <$> fromJSON s)

sudoku (RP p s) = PD <$>
    (readIntGrid <$> fromJSON p) <*>
    (readIntGrid <$> fromJSON s)

thermosudoku :: RawPuzzle -> Result ThermoSudoku
thermosudoku (RP p s) = PD <$>
    (readThermos . readCharGrid <$> fromJSON p) <*>
    (readIntGrid <$> fromJSON s)

pyramid :: RawPuzzle -> Result Pyramid
pyramid (RP p s) = PD <$>
    (Pyr.readPyramid . lines <$> fromJSON p) <*>
    (Pyr.readPlainPyramid . lines <$> fromJSON s)

kpyramid :: RawPuzzle -> Result RowKropkiPyramid
kpyramid (RP p s) = PD <$>
    (Pyr.readKropkiPyramid . lines <$> fromJSON p) <*>
    (Pyr.readPlainPyramid . lines <$> fromJSON s)

slither :: RawPuzzle -> Result SlitherLink
slither (RP p s) = PD <$>
    (readIntGrid <$> fromJSON p) <*>
    (readEdges' <$> fromJSON s)

newtype LSol = LSol { unLSol :: (Loop, Grid (Maybe ())) }
instance FromJSON LSol where
    parseJSON (Object v) = LSol <$> ((,) <$>
                           (readEdges' <$> v .: "loop") <*>
                           (readXGrid <$> v .: "liars"))
    parseJSON _          = mzero

liarslither :: RawPuzzle -> Result LiarSlitherLink
liarslither (RP p s) = PD <$>
    (readIntGrid <$> fromJSON p) <*>
    (unLSol <$> fromJSON s)

tightfitskyscrapers :: RawPuzzle -> Result TightfitSkyscrapers
tightfitskyscrapers (RP p s) = PD <$>
    (readTightOutside <$> fromJSON p) <*>
    (readTightIntGrid <$> fromJSON s)

newtype GridWords = GW { unGW :: (CharClueGrid, [String]) }

instance FromJSON GridWords where
    parseJSON (Object v) = GW <$> ((,) <$>
                                   (readCharClueGrid <$> v .: "grid") <*>
                                   v .: "words")

wordloop :: RawPuzzle -> Result Wordloop
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

wordsearch :: RawPuzzle -> Result Wordsearch
wordsearch (RP p s) = PD <$>
    (unGW <$> fromJSON p) <*>
    (unGM <$> fromJSON s)

newtype Curve = Curve { unCurve :: [Edge] }

instance FromJSON Curve where
    parseJSON v = Curve <$> (readEdges <$> parseJSON v)

curvedata :: RawPuzzle -> Result CurveData
curvedata (RP p s) = PD <$>
    (fmap (fmap unCurve) . unRG <$> fromJSON p) <*>
    (readEdges <$> fromJSON s)

doubleback (RP p s) = PD <$>
    (readAreaGrid <$> fromJSON p) <*>
    (readEdges' <$> fromJSON s)

slalom (RP p s) = PD <$>
    (readIntGrid <$> fromJSON p) <*>
    (readCharGrid <$> fromJSON s)

instance FromJSON CompassC where
    parseJSON v = comp . words <$> parseJSON v
        where c "." = Nothing
              c x   = Just (read x)
              comp [n, e, s, w] = CC (c n) (c e) (c s) (c w)

compass (RP p s) = PD <$>
    (unRG <$> fromJSON p) <*>
    (readAreaGrid <$> fromJSON s)
