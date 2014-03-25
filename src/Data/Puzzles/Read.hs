{-# LANGUAGE OverloadedStrings #-}

module Data.Puzzles.Read (
    TypedPuzzle(..),
    puzzleType,
    dropType,
    RawPuzzle(..),
    ParsePuzzle,

    lits, litsplus, geradeweg, fillomino, masyu, nurikabe, latintapa,
    sudoku, thermosudoku, pyramid, kpyramid, slither,
    liarslither, tightfitskyscrapers, wordloop, wordsearch,
    curvedata, doubleback, slalom, compass,

    parseChar, -- for tests
  ) where

import Prelude hiding (sequence)

import Control.Applicative
import Control.Arrow
import Control.Monad hiding (sequence)

import Data.Hashable
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HMap
import Data.Traversable (traverse, sequence, sequenceA, Traversable)
import Data.Foldable (Foldable, fold)
import Data.Monoid ((<>))

import Data.Char (digitToInt, isAlpha, isDigit)
import Text.Read (readMaybe)
import qualified Data.Text as T

import Data.Yaml

import Data.Puzzles.Grid hiding (neighbours)
import Data.Puzzles.GridShape hiding (size)
import qualified Data.Puzzles.Pyramid as Pyr
import Data.Puzzles.Things

class FromChar a where
    parseChar :: Char -> Parser a

instance FromChar Char where
    parseChar = pure

class FromString a where
    parseString :: String -> Parser a

instance FromChar Int where
    parseChar c
        | isDigit c  = digitToInt <$> parseChar c
        | otherwise  = empty

newtype Alpha = Alpha { unAlpha :: Char }
    deriving (Show, Ord, Eq)

instance FromChar Alpha where
    parseChar c
        | isAlpha c  = Alpha <$> parseChar c
        | otherwise  = empty

-- | A rectangle. Each row has length `w`.
data Rect a = Rect !Int !Int [[a]]
    deriving Show

instance Functor Rect where
    fmap f (Rect w h ls) = Rect w h (map (map f) ls)

instance FromChar a => FromJSON (Rect a) where
    parseJSON (String t) = Rect w h <$> filled
      where
        ls = map T.stripEnd . T.lines $ t
        w = maximum . map T.length $ ls
        h = length ls
        filledc = map (T.unpack . T.justifyLeft w ' ') ls
        filled = sequence . map (mapM parseChar) $ filledc
    parseJSON _          = empty

data Border a = Border [a] [a] [a] [a]
    deriving Show

-- | This instance might be a lie.
instance Foldable Border where
    fold (Border l r b t) = fold l <> fold r <> fold b <> fold t

instance Traversable Border where
    sequenceA (Border l r b t) = Border <$> sequenceA l
                                        <*> sequenceA r
                                        <*> sequenceA b
                                        <*> sequenceA t

instance Functor Border where
    f `fmap` (Border l r b t) = Border (f <$> l) (f <$> r) (f <$> b) (f <$> t)

data BorderedRect a b = BorderedRect !Int !Int [[a]] (Border b)
    deriving Show

instance (FromChar a, FromChar b) => FromJSON (BorderedRect a b) where
    parseJSON v = do
        Rect w h ls <- parseJSON v
        let b = Border (reverse . map head . middle h $ ls)
                       (reverse . map last . middle h $ ls)
                       (middle w . last $ ls)
                       (middle w . head $ ls)
            ls' = map (middle w) . middle h $ ls
        mapM_ ((parseChar :: Char -> Parser Space) . flip ($) ls)
              [head . head, head . last, last . head, last . last]
        lsparsed <- sequence . map (mapM parseChar) $ ls'
        bparsed  <- sequenceA . fmap parseChar $ b
        return $ BorderedRect (w-2) (h-2) lsparsed bparsed
      where
        middle len = take (len - 2) . drop 1

newtype SpacedRect a = SpacedRect { unSpaced :: Rect a }

instance FromString a => FromJSON (SpacedRect a) where
    parseJSON (String t) = if w == wmin then SpacedRect . Rect w h <$> p
                                        else empty
      where
        ls = map T.words . T.lines $ t
        w = maximum . map length $ ls
        wmin = minimum . map length $ ls
        h = length ls
        p = sequence . map (mapM (parseString . T.unpack)) $ ls
    parseJSON _          = empty

instance FromChar MasyuPearl where
    parseChar '*' = pure MBlack
    parseChar 'o' = pure MWhite
    parseChar _   = empty

data Space = Space

instance FromChar Space where
    parseChar ' ' = pure Space
    parseChar _   = empty

data Blank = Blank

instance FromChar Blank where
    parseChar '.' = pure Blank
    parseChar '-' = pure Blank
    parseChar _   = empty

instance FromString Blank where
    parseString "." = pure Blank
    parseString "-" = pure Blank
    parseString _   = empty

instance FromChar SlalomDiag where
    parseChar '/'  = pure SlalomForward
    parseChar '\\' = pure SlalomBackward
    parseChar _    = empty

instance (FromChar a, FromChar b) => FromChar (Either a b) where
    parseChar c = Left <$> parseChar c <|> Right <$> parseChar c

instance (FromString a, FromString b) => FromString (Either a b) where
    parseString c = Left <$> parseString c <|> Right <$> parseString c

instance FromChar a => FromChar (Maybe a) where
    parseChar = optional . parseChar

listListToMap :: [[a]] -> Map.Map (Cell Square) a
listListToMap ls = Map.fromList . concat
                 . zipWith (\y -> zipWith (\x -> (,) (x, y)) [0..]) [h-1,h-2..]
                 $ ls
  where
    h = length ls

rectToSGrid :: Rect a -> SGrid a
rectToSGrid (Rect w h ls) = Grid (Square w h) (listListToMap ls)

rectToClueGrid :: Rect (Either Blank a) -> SGrid (Clue a)
rectToClueGrid = fmap (either (const Nothing) Just) . rectToSGrid

newtype Shaded = Shaded { unShaded :: Bool }

instance FromChar Shaded where
    parseChar 'x'  = pure . Shaded $ True
    parseChar 'X'  = pure . Shaded $ True
    parseChar _    = pure . Shaded $ False

parseShadedGrid :: Value -> Parser (SGrid Bool)
parseShadedGrid v = rectToSGrid . fmap unShaded <$> parseJSON v

parseGrid :: FromChar a => Value -> Parser (SGrid a)
parseGrid v = rectToSGrid <$> parseJSON v

parseClueGrid :: FromChar a => Value -> Parser (SGrid (Clue a))
parseClueGrid v = rectToClueGrid <$> parseJSON v

parseSpacedClueGrid :: FromString a => Value -> Parser (SGrid (Clue a))
parseSpacedClueGrid v = rectToClueGrid . unSpaced <$> parseJSON v

-- parses a string like
--  o-o-o
--  |   |
--  o-o o
--    | |
--    o-o
parsePlainEdges :: Value -> Parser [Edge]
parsePlainEdges v = readEdges <$> parseGrid v

readEdges :: SGrid Char -> [Edge]
readEdges g = horiz ++ vert
    where (w, h) = size g
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

parseGridChars :: FromChar a => SGrid Char -> Parser (SGrid a)
parseGridChars = traverse parseChar

-- | Parse a grid of edges with values at the nodes.
--
-- E.g. o-*-*-o
--      | |
--      *-o
-- to a grid of masyu pearls and some edges.
parseNodeEdges :: FromChar a =>
                  Value -> Parser (SGrid a, [Edge])
parseNodeEdges v = (,) <$>
                   (parseGridChars =<< halveGrid =<< parseGrid v) <*>
                   parsePlainEdges v
  where
    halveGrid (Grid (Square w h) m)
        | odd w && odd h = pure $ Grid s' m'
        | otherwise      = empty
      where
        s' = Square ((w + 1) `div` 2) ((h + 1) `div` 2)
        m' = Map.mapKeys (both (`div` 2))
           . Map.filterWithKey (const . (uncurry (&&) . both even))
           $ m
        both f (x, y) = (f x, f y)

data HalfDirs = HalfDirs {unHalfDirs :: [Dir]}

instance FromChar HalfDirs where
    parseChar c | c == '└'        = pure . HalfDirs $ [V, H]
                | c `elem` "│┘"   = pure . HalfDirs $ [V]
                | c `elem` "─└┌"  = pure . HalfDirs $ [H]
                | otherwise       = pure . HalfDirs $ []

-- parses a string like
--  ┌┐┌─┐
--  ││└┐│
--  │└─┘│
--  └──┐│
--     └┘
parseEdges :: Value -> Parser [Edge]
parseEdges v = do
    Grid _ m <- rectToSGrid . fmap unHalfDirs <$> parseJSON v
    return [ E p d | (p, ds) <- Map.toList m, d <- ds ]

type ThermoRect = Rect (Either Blank (Either Int Alpha))

partitionEithers :: Ord k => Map.Map k (Either a b) -> (Map.Map k a, Map.Map k b)
partitionEithers = Map.foldrWithKey insertEither (Map.empty, Map.empty)
  where
    insertEither k = either (first . Map.insert k) (second . Map.insert k)

parseThermos :: SGrid Alpha -> Parser [Thermometer]
parseThermos (Grid s m) = catMaybes <$> mapM parseThermo (Map.keys m)
  where
    m' = fmap unAlpha m
    parseThermo :: Cell Square -> Parser (Maybe Thermometer)
    parseThermo p | not (isIsolated p)  = empty
                  | not (isStart p)     = pure Nothing
                  | otherwise           = Just <$> parseThermo' p
    parseThermo' :: Cell Square -> Parser Thermometer
    parseThermo' p = do
        q <- next p
        maybe empty (fmap (p:) . parseThermo'') q
    parseThermo'' :: Cell Square -> Parser Thermometer
    parseThermo'' p = do
        q <- next p
        maybe (pure []) (fmap (p:) . parseThermo'') q
    next :: Cell Square -> Parser (Maybe (Cell Square))
    next p = case succs p of
        []   -> pure Nothing
        [q]  -> pure (Just q)
        _    -> empty
    succs      p = filter    (test ((==) . succ) p) . neighbours s $ p
    isIsolated p = not . any (test (==)          p) . neighbours s $ p
    isStart    p = not . any (test ((==) . pred) p) . neighbours s $ p
    test f p q = maybe False (f (m' Map.! p)) (Map.lookup q m')

parseThermoGrid :: ThermoRect -> Parser (SGrid Int, [Thermometer])
parseThermoGrid (Rect w h ls) = (,) (Grid s ints) <$>
                                (parseThermos $ Grid s alphas)
  where
    s = Square w h
    (ints, alphas) = partitionEithers . snd . partitionEithers $
                     listListToMap ls

newtype Tight = Tight { unTight :: Tightfit () }

instance FromChar Tight where
    parseChar '.'  = pure . Tight $ Single ()
    parseChar '/'  = pure . Tight $ UR () ()
    parseChar '\\' = pure . Tight $ DR () ()
    parseChar _    = empty

parseTightOutside :: Value -> Parser (OutsideClues (Maybe Int),
                                      SGrid (Tightfit ()))
parseTightOutside v = do
    BorderedRect w h ls b <- parseJSON v
        :: Parser (BorderedRect Tight (Either Blank Int))
    return (outside . fmap (either (const Nothing) Just) $ b,
            fmap unTight . rectToSGrid $ Rect w h ls)
  where outside (Border l r b t) = OC l r b t

instance FromChar a => FromString (Tightfit a) where
    parseString [c]           = Single <$> parseChar c
    parseString (c: '/':d:[]) = UR <$> parseChar c <*> parseChar d
    parseString (c:'\\':d:[]) = DR <$> parseChar c <*> parseChar d
    parseString _             = empty

parseTightIntGrid :: Value -> Parser (SGrid (Tightfit Int))
parseTightIntGrid v = rectToSGrid . unSpaced <$> parseJSON v

newtype PMarkedWord = PMW {unPMW :: MarkedWord}

parseNWords :: Int -> String -> Parser [String]
parseNWords n s | length ws == n  = pure ws
                | otherwise       = empty
  where
    ws = words s

instance FromJSON PMarkedWord where
    parseJSON v = PMW <$> (MW <$>
                  ((,) <$> ((!!0) <$> x) <*> ((!!1) <$> x)) <*>
                  ((,) <$> ((!!2) <$> x) <*> ((!!3) <$> x)))
        where x = parseJSON v >>= parseNWords 4 >>= mapM parseString

instance FromString Int where
    parseString s = maybe empty pure $ readMaybe s

newtype PCompassC = PCC {unPCC :: CompassC}

instance FromJSON PCompassC where
    parseJSON (String t) = comp . map T.unpack . T.words $ t
        where c "." = pure Nothing
              c x   = Just <$> parseString x
              comp [n, e, s, w] = PCC <$> (CC <$> c n <*> c e <*> c s <*> c w)
              comp _            = empty
    parseJSON _          = empty

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

hashmaptomap :: (Eq a, Hashable a, Ord a) => HMap.HashMap a b -> Map.Map a b
hashmaptomap = Map.fromList . HMap.toList

compose :: (Ord a, Ord b) => Map.Map a b -> Map.Map b c -> Maybe (Map.Map a c)
compose m1 m2 = sequence . Map.map (flip Map.lookup m2) $ m1

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
