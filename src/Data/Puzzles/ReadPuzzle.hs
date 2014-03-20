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

    geradeweg', tightfitskyscrapers', slalom', kpyramid', compass',
    thermosudoku'
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (parse)

import Data.Yaml
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.Map as Map
import qualified Data.Traversable as Traversable
import Data.Char (isAlpha)
import qualified Data.Text as T

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

-- | Almost-inverse of toRead.
-- We actually need to supply a valid solution to successfully
-- parse a valid puzzle without solution, we hope "." will do.
fromRead :: ReadPuzzle (PuzzleDef a b) -> ParsePuzzle a b
fromRead r = (p, s)
  where
    vempty = String (T.pack ".")
    p v = case r (RP v vempty) of Success (PD p' _) -> pure p'
                                  Error _           -> empty
    s v = case r (RP vempty v) of Success (PD _ s') -> pure s'
                                  Error _           -> empty

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

latintapa :: ReadPuzzle LatinTapa
latintapa (RP p s) = PD <$>
    (unRG <$> fromJSON p) <*>
    (readCharClueGrid <$> fromJSON s)

sudoku :: ReadPuzzle Sudoku
sudoku (RP p s) = PD <$>
    (readIntGrid <$> fromJSON p) <*>
    (readIntGrid <$> fromJSON s)

thermosudoku :: ReadPuzzle ThermoSudoku
thermosudoku = toRead thermosudoku'

thermosudoku' :: ParsePuzzle (SGrid Int, [Thermometer]) IntGrid
thermosudoku' = ((parseThermoGrid =<<) . parseJSON, parseClueGrid)

pyramid' :: ParsePuzzle Pyr.Pyramid Pyr.Pyramid
pyramid' = (parseJSON, parseJSON)

pyramid :: ReadPuzzle Pyramid
pyramid = toRead pyramid'

kpyramid' :: ParsePuzzle Pyr.RowKropkiPyramid Pyr.Pyramid
kpyramid' = (parseJSON, parseJSON)

kpyramid :: ReadPuzzle RowKropkiPyramid
kpyramid = toRead kpyramid'

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

tightfitskyscrapers' :: ParsePuzzle
                        (OutsideClues (Maybe Int), SGrid (Tightfit ()))
                        (SGrid (Tightfit Int))
tightfitskyscrapers' = (parseTightOutside, parseTightIntGrid)

tightfitskyscrapers :: ReadPuzzle TightfitSkyscrapers
tightfitskyscrapers = toRead tightfitskyscrapers'

newtype GridWords = GW { unGW :: (CharClueGrid, [String]) }

instance FromJSON GridWords where
    parseJSON (Object v) = GW <$> ((,) <$>
                                   (readCharClueGrid <$> v .: "grid") <*>
                                   v .: "words")
    parseJSON _ = empty

wordloop :: ReadPuzzle Wordloop
wordloop (RP p s) = PD <$>
    (unGW <$> fromJSON p) <*>
    (readCharClueGrid <$> fromJSON s)

newtype GridMarked = GM { unGM :: (CharClueGrid, [MarkedWord]) }

instance FromJSON GridMarked where
    parseJSON (Object v) = GM <$> ((,) <$>
                                   (readCharClueGrid <$> v .: "grid") <*>
                                   (map unPMW <$> v .: "words"))
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

slalom' :: ParsePuzzle (SGrid (Clue Int)) (SGrid SlalomDiag)
slalom' = (parseClueGrid, \v -> rectToSGrid <$> parseJSON v)

slalom :: ReadPuzzle Slalom
slalom = toRead slalom'

compass :: ReadPuzzle Compass
compass (RP p s) = PD <$>
    (fmap (fmap unPCC) . unRG <$> fromJSON p) <*>
    (readAreaGrid <$> fromJSON s)

compass' :: ParsePuzzle (SGrid (Clue CompassC)) CharGrid
compass' = fromRead compass
