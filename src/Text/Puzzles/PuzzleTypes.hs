{-# LANGUAGE OverloadedStrings #-}

module Text.Puzzles.PuzzleTypes (
    lits, litsplus, geradeweg, fillomino, masyu, nurikabe, latintapa,
    sudoku, thermosudoku, pyramid, kpyramid, slither,
    liarslither, tightfitskyscrapers, wordloop, wordsearch,
    curvedata, doubleback, slalom, compass, boxof2or3,
    afternoonskyscrapers, countnumbers, tapa, japanesesums, coral,
    maximallengths, primeplace, labyrinth, bahnhof, blackoutDominos,
    angleloop, anglers, cave, skyscrapers, summon, baca,
    buchstabensalat, doppelblock, sudokuDoppelblock, dominos,
    skyscrapersStars, numberlink
  ) where

import Control.Applicative
import Control.Monad

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

fillomino :: ParsePuzzle IntGrid (SGrid Int)
fillomino = (parseClueGrid, parseExtGrid)

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

tapa :: ParsePuzzle (SGrid TapaClue) ShadedGrid
tapa = (\v -> fmap unParseTapaClue . unRG <$> parseJSON v,
        parseShadedGrid)

japanesesums :: ParsePuzzle (OutsideClues [Int]) (SGrid (Either Black Int))
japanesesums = (parseMultiOutsideClues, parseGrid)

coral :: ParsePuzzle (OutsideClues [String]) ShadedGrid
coral = (parseMultiOutsideClues, parseShadedGrid)

maximallengths :: ParsePuzzle (OutsideClues (Maybe Int)) Loop
maximallengths = (\v -> fmap blankToMaybe <$> parseCharOutside v,
                  parseEdges)

primeplace :: ParsePuzzle (SGrid PrimeDiag) (SGrid Int)
primeplace = (parseIrregGrid, parseIrregGrid)

labyrinth :: ParsePuzzle (SGrid (Clue Int), [Edge]) (SGrid (Clue Int))
labyrinth = (parseCellEdges, parseClueGrid')

bahnhof :: ParsePuzzle (SGrid (Clue Char)) ()
bahnhof = (parseClueGrid, error "bahnhof solution not implemented")

blackoutDominos :: ParsePuzzle (SGrid (Clue Int)) (SGrid (Clue Int), AreaGrid)
blackoutDominos = (,)
    parseIrregGrid
    (\v -> (,) <$> parseFrom ["values"] parseIrregGrid v
               <*> parseFrom ["dominos"] parseIrregGrid v)

angleloop :: ParsePuzzle (SGrid (Clue Int)) VertexLoop
angleloop = (parseClueGrid, parseCoordLoop)

anglers :: ParsePuzzle (OutsideClues (Maybe Int), SGrid (Maybe Fish)) ()
anglers = ( parseOutsideGridMap blankToMaybe blankToMaybe'
          , error "anglers solution not implemented")

cave :: ParsePuzzle (SGrid (Clue Int)) ShadedGrid
cave = (parseClueGrid, parseShadedGrid)

parseOut :: FromJSON a =>
            Value -> Parser (OutsideClues (Maybe a))
parseOut v = fmap (blankToMaybe' . unEither') <$> parseOutside v

skyscrapers :: ParsePuzzle (OutsideClues (Maybe Int)) IntGrid
skyscrapers = (parseOut, parseClueGrid)

skyscrapersStars :: ParsePuzzle (OutsideClues (Maybe Int))
                                (SGrid (Either Int Star))
skyscrapersStars = (parseOut, parseGrid)

summon :: ParsePuzzle (AreaGrid, OutsideClues (Maybe Int)) IntGrid
summon = ( \v -> (,) <$> parseFrom ["grid"] parseGrid v
                     <*> parseFrom ["outside"] parseOut v
         , parseClueGrid
         )

baca :: ParsePuzzle
            (SGrid (Maybe Char), OutsideClues [Int], OutsideClues (Maybe Char))
            ()
baca = ( \v -> (,,) <$> parseFrom ["grid"] parseClueGrid v
                    <*> parseFrom ["outside"] parseTopLeft v
                    <*> parseFrom ["outside"] parseBottomRight v
       , error "baca solution not implemented"
       )
  where
    parseTopLeft (Object v) = do
        l <- v .: "left"
        t <- v .: "top"
        return $ OC l [] [] t
    parseTopLeft _ = empty
    parseBottomRight (Object v) = do
        b <- v .: "bottom"
        r <- v .: "right"
        oc <- OC [] <$> parseLine r <*> parseLine b <*> pure []
        return $ fmap blankToMaybe' oc
    parseBottomRight _ = empty

buchstabensalat :: ParsePuzzle (OutsideClues (Maybe Char)) (SGrid (Maybe Char))
buchstabensalat =
    ( \v -> fmap blankToMaybe <$> parseCharOutside v
    , fmap (fmap blankToMaybe') . parseGrid
    )

doppelblock :: ParsePuzzle (OutsideClues (Maybe Int)) ()
doppelblock =
    ( \v -> fmap (blankToMaybe' . unEither') <$> parseOutside v
    , error "doppelblock solution not implemented"
    )

sudokuDoppelblock :: ParsePuzzle (AreaGrid, OutsideClues (Maybe Int)) ()
sudokuDoppelblock =
    ( \v -> (,) <$> parseFrom ["grid"] parseGrid v
                <*> parseFrom ["outside"] parseOutInts v
    , error "solution not implemented"
    )
  where
    parseOutInts v = fmap (blankToMaybe' . unEither') <$> parseOutside v

dominos :: ParsePuzzle (SGrid (Maybe Int)) AreaGrid
dominos = (parseClueGrid, parseGrid)

numberlink :: ParsePuzzle (SGrid (Maybe Int)) [Edge]
numberlink = (p, fmap collectLines . p)
  where
    p = fmap (fmap (blankToMaybe . unEither')) . parseExtGrid
