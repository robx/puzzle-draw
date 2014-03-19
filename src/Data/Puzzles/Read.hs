module Data.Puzzles.Read where

import Data.Puzzles.Grid hiding (neighbours)
import Data.Puzzles.GridShape (Square(..), Cell, neighbours)
import Data.Puzzles.Things

import Text.Read (readMaybe)
import Data.Char (digitToInt, isAlpha, isDigit)
import Data.List (nub, sort)
import Data.Maybe (catMaybes)

import Data.Yaml
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import qualified Data.Map as Map
import Control.Arrow

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

data Rect a = Rect !Int !Int [[a]]

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

charToIntClue c
    | isDigit c  = Just $ digitToInt c
    | otherwise  = Nothing

strToIntClue :: String -> IntClue
strToIntClue = readMaybe

charToCharClue c
    | c `elem` [' ', '.', '-']  = Nothing
    | otherwise                 = Just c

instance FromChar MasyuPearl where
    parseChar '*' = pure MBlack
    parseChar 'o' = pure MWhite
    parseChar _   = empty

data Blank = Blank

instance FromChar Blank where
    parseChar '.' = pure Blank
    parseChar _   = empty

instance FromChar SlalomDiag where
    parseChar '/'  = pure SlalomForward
    parseChar '\\' = pure SlalomBackward
    parseChar _    = empty

instance (FromChar a, FromChar b) => FromChar (Either a b) where
    parseChar c = Left <$> parseChar c <|> Right <$> parseChar c

instance FromChar a => FromChar (Maybe a) where
    parseChar = optional . parseChar

type MasyuRect = Rect (Either Blank MasyuPearl)

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

charToMasyuClue :: Char -> MasyuClue
charToMasyuClue '*' = Just MBlack
charToMasyuClue 'o' = Just MWhite
charToMasyuClue c
    | c == ' ' || c == '.'  = Nothing

type CharGrid = SGrid Char
type MasyuGrid = SGrid MasyuClue
type AreaGrid = CharGrid
type ShadedGrid = SGrid Bool
type CharClueGrid = SGrid (Maybe Char)
type IntGrid = SGrid (Clue Int)

readCharGridUnfilled = fromListList . lines

-- | Read a grid of characters, filling short lines
-- to form a rectangle.
readCharGrid g = fromListList filled
  where
    ls = lines g
    w = maximum . map length $ ls
    filled = map (take w . (++ (repeat ' '))) ls

readAreaGrid = readCharGrid

readCharClueGrid = fmap charToCharClue . readCharGrid
readBoolGrid = fmap (`elem` ['x', 'X']) . readCharGrid
readIntGrid = fmap charToIntClue . readCharGrid
readStrGrid = fromListList . map words . lines
readWideIntGrid = fmap strToIntClue . readStrGrid
readMasyuGrid = fmap charToMasyuClue . readCharGrid
parseClueGrid v = rectToClueGrid <$> parseJSON v

readXGrid = fmap f . readCharGrid
    where f 'X' = Just ()
          f _   = Nothing

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
readEdges' s = nub . sort . concatMap edges . cells $ g
    where g = readCharGrid s
          (w, h) = size g
          isV c = c `elem` "│└┘"
          isH c = c `elem` "─└┌"
          edges p = [ E p V | isV (g ! p) ] ++ [ E p H | isH (g ! p) ]

data HalfDirs = HalfDirs {unHalfDirs :: [Dir]}

instance FromChar HalfDirs where
    parseChar c | c == '└'        = pure . HalfDirs $ [V, H]
                | c `elem` "│┘"   = pure . HalfDirs $ [V]
                | c `elem` "─└┌"  = pure . HalfDirs $ [H]
                | otherwise       = pure . HalfDirs $ []

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
parseThermoGrid (Rect w h ls) = (,) (Grid shape ints) <$>
                                (parseThermos $ Grid shape alphas)
  where
    shape = Square w h
    (ints, alphas) = partitionEithers . snd . partitionEithers $
                     listListToMap ls

readTightOutside :: String -> (OutsideClues (Maybe Int), SGrid (Tightfit ()))
readTightOutside s = (OC l r b t, gt)
    where g = readCharGrid s
          (w', h') = size g
          w = w' - 2
          h = h' - 2
          l = map charToIntClue [ g ! (0, y+1) | y <- [0..h-1] ]
          r = map charToIntClue [ g ! (w'-1, y+1) | y <- [0..h-1] ]
          b = map charToIntClue [ g ! (x+1, 0) | x <- [0..w-1] ]
          t = map charToIntClue [ g ! (x+1, h'-1) | x <- [0..w-1] ]
          readTight '.' = Single ()
          readTight '/' = UR () ()
          readTight '\\' = DR () ()
          gt = fromListList [ [ readTight (g ! (x, y)) | x <- [1..w'-2] ]
                            | y <- [h'-2,h'-3..1]
                            ]

instance FromChar a => FromString (Tightfit a) where
    parseString [c]           = Single <$> parseChar c
    parseString (c: '/':d:[]) = UR <$> parseChar c <*> parseChar d
    parseString (c:'\\':d:[]) = DR <$> parseChar c <*> parseChar d
    parseString _             = empty

readTightInt [c] = Single (digitToInt c)
readTightInt (c:'/':d:[]) = UR (digitToInt c) (digitToInt d)
readTightInt (c:'\\':d:[]) = DR (digitToInt c) (digitToInt d)

readTightIntGrid :: String -> SGrid (Tightfit Int)
readTightIntGrid = fromListList . map (map readTightInt . words) . lines

parseTightIntGrid :: Value -> Parser (SGrid (Tightfit Int))
parseTightIntGrid v = rectToSGrid . unSpaced <$> parseJSON v
