module Data.Puzzles.Read where

import Data.Puzzles.Grid hiding (neighbours)
import Data.Puzzles.GridShape (Square(..), Cell, neighbours)
import Data.Puzzles.Things

import Text.Read (readMaybe)
import Data.Char (digitToInt, isAlpha, isDigit)
import Data.Maybe (catMaybes)

import Data.Yaml
import qualified Data.Text as T
import Control.Applicative
import qualified Data.Map as Map
import Control.Arrow
import Data.Traversable hiding (mapM, sequence)
import Data.Foldable (Foldable, fold)
import Data.Monoid ((<>))

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

charToIntClue :: Char -> Maybe Int
charToIntClue c
    | isDigit c  = Just $ digitToInt c
    | otherwise  = Nothing

strToIntClue :: String -> IntClue
strToIntClue = readMaybe

charToCharClue :: Char -> Maybe Char
charToCharClue c
    | c `elem` [' ', '.', '-']  = Nothing
    | otherwise                 = Just c

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

type CharGrid = SGrid Char
type MasyuGrid = SGrid MasyuClue
type AreaGrid = CharGrid
type ShadedGrid = SGrid Bool
type CharClueGrid = SGrid (Maybe Char)
type IntGrid = SGrid (Clue Int)

readCharGridUnfilled :: String -> SGrid Char
readCharGridUnfilled = fromListList . lines

-- | Read a grid of characters, filling short lines
-- to form a rectangle.
readCharGrid :: String -> SGrid Char
readCharGrid g = fromListList filled
  where
    ls = lines g
    w = maximum . map length $ ls
    filled = map (take w . (++ (repeat ' '))) ls

readAreaGrid :: String -> SGrid Char
readAreaGrid = readCharGrid

readCharClueGrid :: String -> SGrid (Clue Char)
readCharClueGrid = fmap charToCharClue . readCharGrid
readBoolGrid :: String -> SGrid Bool
readBoolGrid = fmap (`elem` ['x', 'X']) . readCharGrid
readIntGrid :: String -> SGrid (Clue Int)
readIntGrid = fmap charToIntClue . readCharGrid
readStrGrid :: String -> SGrid String
readStrGrid = fromListList . map words . lines
readWideIntGrid :: String -> SGrid (Maybe Int)
readWideIntGrid = fmap strToIntClue . readStrGrid

parseClueGrid :: FromChar a => Value -> Parser (SGrid (Clue a))
parseClueGrid v = rectToClueGrid <$> parseJSON v

readXGrid :: String -> SGrid (Maybe ())
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

instance FromJSON PMarkedWord where
    parseJSON v = PMW <$> (MW <$>
                  ((,) <$> ((!!0) <$> x) <*> ((!!1) <$> x)) <*>
                  ((,) <$> ((!!2) <$> x) <*> ((!!3) <$> x)))
        where x = map read . words <$> parseJSON v :: Parser [Int]

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
