{-# LANGUAGE OverloadedStrings #-}

module Text.Puzzles.PuzzleTypes (
    lits, litsplus, geradeweg, fillomino, masyu, nurikabe, latintapa,
    sudoku, thermosudoku, pyramid, kpyramid, slither,
    liarslither, tightfitskyscrapers, wordloop, wordsearch,
    curvedata, doubleback, slalom, compass, boxof2or3,
    afternoonskyscrapers, meanderingnumbers, tapa, japanesesums, coral,
    maximallengths, primeplace, labyrinth, bahnhof, cave, angleLoop,
    shikaku, slovaksums
  ) where

import Control.Applicative
import Control.Monad

import Data.Yaml

import Text.Puzzles.Util
import Text.Puzzles.Puzzle
import Data.Puzzles.Grid
import Data.Puzzles.GridShape
import qualified Data.Puzzles.Pyramid as Pyr
import Data.Puzzles.Elements

lits :: ParsePuzzle AreaGrid (Grid C Bool)
lits = (parseGrid, parseShadedGrid)

litsplus :: ParsePuzzle AreaGrid (Grid C Bool)
litsplus = lits

geradeweg :: ParsePuzzle (Grid C (Maybe Int)) (Loop C)
geradeweg = (parseClueGrid, parseEdges)

fillomino :: ParsePuzzle (Grid C (Maybe Int)) (Grid C Int)
fillomino = (parseExtClueGrid, parseExtGrid)

masyu :: ParsePuzzle (Grid C (Maybe MasyuPearl)) (Loop C)
masyu = (parseClueGrid, parseEdges)

nurikabe :: ParsePuzzle (Grid C (Maybe Int)) (Grid C Bool)
nurikabe = (parseExtClueGrid, parseShadedGrid)

latintapa :: ParsePuzzle (Grid C (Maybe [String])) (Grid C (Maybe Char))
latintapa = ((unRG <$>) . parseJSON,
             fmap (fmap (fmap unAlpha)) . parseClueGrid')

sudoku :: ParsePuzzle (Grid C (Maybe Int)) (Grid C (Maybe Int))
sudoku = (parseClueGrid, parseClueGrid)

thermosudoku :: ParsePuzzle (Grid C (Maybe Int), [Thermometer])
                            (Grid C (Maybe Int))
thermosudoku = ((parseThermoGrid =<<) . parseJSON, parseClueGrid)

pyramid :: ParsePuzzle Pyr.Pyramid Pyr.PyramidSol
pyramid = (parseJSON, parseJSON)

kpyramid :: ParsePuzzle Pyr.RowKropkiPyramid Pyr.PyramidSol
kpyramid = (parseJSON, parseJSON)

slither :: ParsePuzzle (Grid C (Clue Int)) (Loop N)
slither = (parseClueGrid, parseEdges)

newtype LSol = LSol { unLSol :: (Loop N, Grid C Bool) }
instance FromJSON LSol where
    parseJSON (Object v) = LSol <$> ((,) <$>
                           (parseEdges =<< v .: "loop") <*>
                           (parseShadedGrid =<< v .: "liars"))
    parseJSON _          = mzero

liarslither :: ParsePuzzle (Grid C (Maybe Int)) (Loop N, Grid C Bool)
liarslither = (parseClueGrid, (unLSol <$>) . parseJSON)

tightfitskyscrapers :: ParsePuzzle
                       (OutsideClues C (Maybe Int), Grid C (Tightfit ()))
                       (Grid C (Tightfit Int))
tightfitskyscrapers = (parseTightOutside, parseSpacedGrid)

newtype GridWords = GW { unGW :: (Grid C (Maybe Char), [String]) }

instance FromJSON GridWords where
    parseJSON (Object v) = GW <$> ((,) <$>
                                   (parseClueGrid =<< v .: "grid") <*>
                                   v .: "words")
    parseJSON _ = empty

wordloop :: ParsePuzzle (Grid C (Maybe Char), [String]) (Grid C (Maybe Char))
wordloop = ((unGW <$>) . parseJSON, parseClueGrid)

newtype GridMarked = GM { unGM :: (Grid C (Maybe Char), [MarkedWord]) }

instance FromJSON GridMarked where
    parseJSON (Object v) = GM <$> ((,) <$>
                                   (parseClueGrid =<< v .: "grid") <*>
                                   (map unPMW <$> v .: "words"))
    parseJSON _          = mzero

wordsearch :: ParsePuzzle (Grid C (Maybe Char), [String])
                          (Grid C (Maybe Char), [MarkedWord])
wordsearch = ((unGW <$>) . parseJSON, (unGM <$>) . parseJSON)

newtype Curve = Curve { unCurve :: [Edge N] }

instance FromJSON Curve where
    parseJSON v = Curve <$> parsePlainEdges v

curvedata :: ParsePuzzle (Grid C (Maybe [Edge N])) [Edge C]
curvedata = ((fmap (fmap unCurve) . unRG <$>) . parseJSON, parsePlainEdges)

doubleback :: ParsePuzzle AreaGrid (Loop C)
doubleback = (parseGrid, parseEdges)

slalom :: ParsePuzzle (Grid N (Maybe Int)) (Grid C SlalomDiag)
slalom = (parseClueGrid, parseGrid)

compass :: ParsePuzzle (Grid C (Maybe CompassC)) AreaGrid
compass = ((fmap (fmap unPCC) . unRG <$>) . parseJSON, parseGrid)

boxof2or3 :: ParsePuzzle (Grid N MasyuPearl, [Edge N]) ()
boxof2or3 = (parseNodeEdges, error "boxof2or3 parsing not implemented")

afternoonskyscrapers :: ParsePuzzle (Grid C Shade) (Grid C (Maybe Int))
afternoonskyscrapers = (parseAfternoonGrid, parseGrid)

-- this should be changed to support clue numbers
meanderingnumbers :: ParsePuzzle AreaGrid (Grid C (Maybe Int))
meanderingnumbers = (parseGrid, parseGrid)

tapa :: ParsePuzzle (Grid C (Maybe TapaClue)) (Grid C Bool)
tapa = (\v -> fmap (fmap unParseTapaClue) . unRG <$> parseJSON v,
        parseShadedGrid)

japanesesums :: ParsePuzzle (OutsideClues C [Int], String)
                            (Grid C (Either Black Int))
japanesesums = (p, parseGrid)
  where
    p v@(Object o) = (,) <$> parseMultiOutsideClues v <*> o .: "digits"
    p _            = empty

coral :: ParsePuzzle (OutsideClues C [String]) (Grid C Bool)
coral = (,)
    (fmap (fmap (map unIntString)) . parseMultiOutsideClues)
    parseShadedGrid

maximallengths :: ParsePuzzle (OutsideClues C (Maybe Int)) (Loop C)
maximallengths = (\v -> fmap blankToMaybe <$> parseCharOutside v,
                  parseEdges)

primeplace :: ParsePuzzle (Grid C PrimeDiag) (Grid C Int)
primeplace = (parseIrregGrid, parseIrregGrid)

labyrinth :: ParsePuzzle (Grid C (Maybe Int), [Edge N]) (Grid C (Maybe Int))
labyrinth = (parseCellEdges, parseClueGrid')

bahnhof :: ParsePuzzle (Grid C (Maybe BahnhofClue)) [Edge C]
bahnhof = (parseClueGrid, parseEdges)

cave :: ParsePuzzle (Grid C (Maybe Int)) (Grid C Bool)
cave = (parseClueGrid, parseShadedGrid)

angleLoop :: ParsePuzzle (Grid N (Clue Int)) VertexLoop
angleLoop = (parseClueGrid, parseCoordLoop)

shikaku :: ParsePuzzle (Grid C (Maybe Int)) AreaGrid
shikaku = (parseExtClueGrid, parseGrid)

slovaksums :: ParsePuzzle (Grid C (Maybe SlovakClue)) (Grid C (Maybe Int))
slovaksums = ((fmap (fmap unPSlovakClue) . unRG <$>) . parseJSON, parseClueGrid)
