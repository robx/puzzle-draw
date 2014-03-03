{-# LANGUAGE OverloadedStrings #-}
module Data.Puzzles.Puzzle where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Yaml
import Data.List
import qualified Data.HashMap.Strict as HM

import Data.Puzzles.Grid

{-

LITS, LITS+	area grid	shaded
geradeweg	int grid	loop
nurikabe	int grid	shaded
kpyramid	kpyramid	int pyramid
doubleback	area grid	loop
skyscraper	tightfit grid	a/b int grid
		  (border int, '/\')
compass		ref grid, compass	area grid
curve data	ref grid, curves	loop'
slither link	int grid	edge loop
slither link liar	int grid	edge loop, X grid
latin tapa	ref grid, words		letter grid
slalom		int grid	/\ grid
sudoku		int grid	int grid
thermo sudoku	int/thermo grid	int grid
wordloop	char grid	char grid
wordsearch	char grid, words	char grid, lines

-}

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

type AreaGrid = CharGrid
type ShadedGrid = Grid Bool
type CharClueGrid = Grid (Maybe Char)

readCharGrid = fromListList . lines
readAreaGrid = readCharGrid
readCharClueGrid = fmap charToCharClue . readCharGrid
readBoolGrid = fmap (`elem` ['x', 'X']) . readCharGrid
readIntGrid = fmap charToIntClue . readCharGrid
readMasyuGrid = fmap charToMasyuClue . readCharGrid

parseLITS :: Puzzle -> Result LITS
parseLITS (P _ p s) = PP <$>
                           (readAreaGrid <$> (fromJSON p)) <*>
                           (readBoolGrid <$> (fromJSON s))

parseLITSPlus :: Puzzle -> Result LITS
parseLITSPlus = parseLITS

type Loop = [Edge] -- should have some properties...

-- parses a string like
--  o-o-o
--  |   |
--  o-o o
--    | |
--    o-o
readEdges :: String -> [Edge]
readEdges s = horiz ++ vert
    where g = readCharGrid s
          (w, h) = size g
          w' = w `div` 2
          h' = h `div` 2
          isHoriz (x, y) = g ! (2 * x + 1, 2 * y) == '-'
          isVert  (x, y) = g ! (2 * x, 2 * y + 1) == '|'
          horiz = [ E (x, y) H | x <- [0 .. w' - 1]
                               , y <- [0 .. h']
                               , isHoriz (x, y)
                               ]
          vert =  [ E (x, y) V | x <- [0 .. w']
                               , y <- [0 .. h' - 1]
                               , isVert (x, y)
                               ]

-- parses a string like
--  ┌┐┌─┐
--  ││└┐│
--  │└─┘│
--  └──┐│
--     └┘
readEdges' :: String -> [Edge]
readEdges' s = nub . sort . concat . map edges . points $ g
    where g = readCharGrid s
          (w, h) = size g
          isV c = c `elem` "│└┘"
          isH c = c `elem` "─└┌"
          edges p = [ E p V | isV (g ! p) ] ++ [ E p H | isH (g ! p) ]

type Geradeweg = ParsedPuzzle IntGrid Loop

parseGeradeweg :: Puzzle -> Result Geradeweg
parseGeradeweg (P _ p s) = PP <$>
                           (readIntGrid <$> (fromJSON p)) <*>
                           (readEdges' <$> (fromJSON s))

type Fillomino = ParsedPuzzle IntGrid IntGrid

parseFillomino :: Puzzle -> Result Fillomino
parseFillomino (P _ p s) = PP <$>
                           (readIntGrid <$> (fromJSON p)) <*>
                           (readIntGrid <$> (fromJSON s))

type Masyu = ParsedPuzzle MasyuGrid Loop

parseMasyu :: Puzzle -> Result Masyu
parseMasyu (P "masyu" p s) = PP <$>
                             (readMasyuGrid <$> (fromJSON p)) <*>
                             (readEdges' <$> (fromJSON s))

type Nurikabe = ParsedPuzzle IntGrid ShadedGrid

parseNurikabe :: Puzzle -> Result Nurikabe
parseNurikabe (P _ p s) = PP <$>
                          (readIntGrid <$> (fromJSON p)) <*>
                          (readBoolGrid <$> (fromJSON s))

newtype RefGrid a = RefGrid { unRG :: Grid (Maybe a) }

merge :: CharGrid -> HM.HashMap String a -> RefGrid a
merge g m = RefGrid (f <$> g)
    where f c | isref c   = Just (m HM.! [c])
              | otherwise = Nothing
          isref c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

instance (FromJSON a) => FromJSON (RefGrid a) where
    parseJSON (Object v) = merge <$>
                           (readCharGrid <$> (v .: "grid")) <*>
                           v .: "clues"

type LatinTapa = ParsedPuzzle (Grid [String]) CharClueGrid

parseLatinTapa (P _ p s) = PP <$>
                           (unRG <$> (fromJSON p)) <*>
                           (readCharClueGrid <$> (fromJSON s))

type Sudoku = ParsedPuzzle IntGrid IntGrid

parseSudoku (P _ p s) = PP <$>
                        (readIntGrid <$> (fromJSON p)) <*>
                        (readIntGrid <$> (fromJSON s))
