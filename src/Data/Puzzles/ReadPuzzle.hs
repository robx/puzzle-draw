{-# LANGUAGE OverloadedStrings #-}

module Data.Puzzles.ReadPuzzle (
    TypedPuzzle(..),
    puzzleType,
    dropType,
    RawPuzzle(..),
    ParsePuzzle,

    lits, litsplus, geradeweg, fillomino, masyu, nurikabe, latintapa,
    sudoku, thermosudoku, pyramid, kpyramid, slither,
    liarslither, tightfitskyscrapers, wordloop, wordsearch,
    curvedata, doubleback, slalom, compass,
  ) where

import Control.Applicative
import Control.Monad

import Data.Yaml
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable
import Data.Char (isAlpha)

import Data.Puzzles.Grid
import qualified Data.Puzzles.Pyramid as Pyr
import Data.Puzzles.Read
import Data.Puzzles.Things

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

-- | A pair of parsers for a puzzle type.
-- First parses the puzzle, second the solution.
type ParsePuzzle a b = (Value -> Parser a, Value -> Parser b)

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

newtype RefGrid a = RefGrid { unRG :: SGrid a }

data Ref = Ref { unRef :: Char }
    deriving Show

instance FromChar Ref where
    parseChar c | isAlpha c = pure (Ref c)
    parseChar _             = empty

hashmaptomap :: (Eq a, Hashable a, Ord a) => HM.HashMap a b -> Map.Map a b
hashmaptomap = Map.fromList . HM.toList

compose :: (Ord a, Ord b) => Map.Map a b -> Map.Map b c -> Maybe (Map.Map a c)
compose m1 m2 = Traversable.sequence . Map.map (flip Map.lookup m2) $ m1

instance FromJSON a => FromJSON (RefGrid a) where
    parseJSON (Object v) = RefGrid <$> do
        Grid s refs <- fmap (fmap ((:[]) . unRef)) . rectToClueGrid <$>
                       (v .: "grid" :: Parser (Rect (Either Blank Ref)))
        m <- hashmaptomap <$> v .: "clues"
        case compose (Map.mapMaybe id refs) m of
            Nothing -> mzero
            Just m' -> return $ Grid s m'
    parseJSON _ = empty

latintapa :: ParsePuzzle (SGrid (Clue [String])) (SGrid (Maybe Char))
latintapa = ((unRG <$>) . parseJSON, parseClueGrid)

sudoku :: ParsePuzzle IntGrid IntGrid
sudoku = (parseClueGrid, parseClueGrid)

thermosudoku :: ParsePuzzle (SGrid Int, [Thermometer]) IntGrid
thermosudoku = ((parseThermoGrid =<<) . parseJSON, parseClueGrid)

pyramid :: ParsePuzzle Pyr.Pyramid Pyr.Pyramid
pyramid = (parseJSON, parseJSON)

kpyramid :: ParsePuzzle Pyr.RowKropkiPyramid Pyr.Pyramid
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
