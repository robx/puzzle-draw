{-# LANGUAGE OverloadedStrings #-}

module Text.Puzzles.PuzzleTypes (
    lits, litsplus, geradeweg, fillomino, masyu, nurikabe, latintapa,
    sudoku, thermosudoku, pyramid, kpyramid, slither,
    liarslither, tightfitskyscrapers, wordloop, wordsearch,
    curvedata, doubleback, slalom, compass, boxof2or3,
    afternoonskyscrapers, countnumbers,
  ) where

import Prelude hiding (sequence)

import Control.Applicative
import Control.Monad hiding (sequence)

import Data.Yaml

import Text.Puzzles.Util
import Text.Puzzles.Puzzle
import Data.Puzzles.Grid
import Data.Puzzles.GridShape hiding (size)
import qualified Data.Puzzles.Pyramid as Pyr
import Data.Puzzles.Elements

lits :: ParsePuzzle AreaGrid ShadedGrid
lits = (parseGrid, parseShadedGrid)

litsplus :: ParsePuzzle AreaGrid ShadedGrid
litsplus = lits

geradeweg :: ParsePuzzle (SGrid (Clue Int)) Loop
geradeweg = (parseClueGrid, parseEdges)

fillomino :: ParsePuzzle IntGrid IntGrid
fillomino = (parseClueGrid, parseClueGrid)

masyu :: ParsePuzzle (SGrid (Clue MasyuPearl)) Loop
masyu = (parseClueGrid, parseEdges)

nurikabe :: ParsePuzzle IntGrid ShadedGrid
nurikabe = (parseSpacedClueGrid, parseShadedGrid)

latintapa :: ParsePuzzle (SGrid (Clue [String])) (SGrid (Maybe Char))
latintapa = ((unRG <$>) . parseJSON, parseClueGrid)

sudoku :: ParsePuzzle IntGrid IntGrid
sudoku = (parseClueGrid, parseClueGrid)

thermosudoku :: ParsePuzzle (SGrid Int, [Thermometer]) IntGrid
thermosudoku = ((parseThermoGrid =<<) . parseJSON, parseClueGrid)

pyramid :: ParsePuzzle Pyr.Pyramid Pyr.PyramidSol
pyramid = (parseJSON, parseJSON)

kpyramid :: ParsePuzzle Pyr.RowKropkiPyramid Pyr.PyramidSol
kpyramid = (parseJSON, parseJSON)

slither :: ParsePuzzle (SGrid (Clue Int)) Loop
slither = (parseClueGrid, parseEdges)

newtype LSol = LSol { unLSol :: (Loop, SGrid Bool) }
instance FromJSON LSol where
    parseJSON (Object v) = LSol <$> ((,) <$>
                           (parseEdges =<< v .: "loop") <*>
                           (parseShadedGrid =<< v .: "liars"))
    parseJSON _          = mzero

liarslither :: ParsePuzzle (SGrid (Clue Int)) (Loop, SGrid Bool)
liarslither = (parseClueGrid, (unLSol <$>) . parseJSON)

tightfitskyscrapers :: ParsePuzzle
                       (OutsideClues (Maybe Int), SGrid (Tightfit ()))
                       (SGrid (Tightfit Int))
tightfitskyscrapers = (parseTightOutside, parseTightIntGrid)

newtype GridWords = GW { unGW :: (CharClueGrid, [String]) }

instance FromJSON GridWords where
    parseJSON (Object v) = GW <$> ((,) <$>
                                   (parseClueGrid =<< v .: "grid") <*>
                                   v .: "words")
    parseJSON _ = empty

wordloop :: ParsePuzzle (CharClueGrid, [String]) CharClueGrid
wordloop = ((unGW <$>) . parseJSON, parseClueGrid)

newtype GridMarked = GM { unGM :: (CharClueGrid, [MarkedWord]) }

instance FromJSON GridMarked where
    parseJSON (Object v) = GM <$> ((,) <$>
                                   (parseClueGrid =<< v .: "grid") <*>
                                   (map unPMW <$> v .: "words"))
    parseJSON _          = mzero

wordsearch :: ParsePuzzle (CharClueGrid, [String]) (CharClueGrid, [MarkedWord])
wordsearch = ((unGW <$>) . parseJSON, (unGM <$>) . parseJSON)

newtype Curve = Curve { unCurve :: [Edge] }

instance FromJSON Curve where
    parseJSON v = Curve <$> parsePlainEdges v

curvedata :: ParsePuzzle (SGrid (Clue [Edge])) [Edge]
curvedata = ((fmap (fmap unCurve) . unRG <$>) . parseJSON, parsePlainEdges)

doubleback :: ParsePuzzle AreaGrid Loop
doubleback = (parseGrid, parseEdges)

slalom :: ParsePuzzle (SGrid (Clue Int)) (SGrid SlalomDiag)
slalom = (parseClueGrid, \v -> rectToSGrid <$> parseJSON v)

compass :: ParsePuzzle (SGrid (Clue CompassC)) CharGrid
compass = ((fmap (fmap unPCC) . unRG <$>) . parseJSON, parseGrid)

boxof2or3 :: ParsePuzzle (SGrid MasyuPearl, [Edge]) ()
boxof2or3 = (parseNodeEdges, error "boxof2or3 parsing not implemented")

afternoonskyscrapers :: ParsePuzzle (SGrid Shade) IntGrid
afternoonskyscrapers = (parseAfternoonGrid, parseGrid)

-- this should be changed to support clue numbers
countnumbers :: ParsePuzzle AreaGrid IntGrid
countnumbers = (parseGrid, parseGrid)