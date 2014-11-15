{-# LANGUAGE OverloadedStrings #-}

module Text.Puzzles.Util where

import Prelude hiding (mapM)

import Control.Applicative
import Control.Arrow
import Control.Monad hiding (mapM)

import Data.Hashable
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HMap
import Data.Traversable (traverse, sequenceA, mapM, Traversable)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid ((<>))
import Data.List (intersect)

import Data.Char (digitToInt, isAlpha, isDigit)
import Text.Read (readMaybe)
import qualified Data.Text as T

import Data.Yaml

import Data.Puzzles.Grid
import Data.Puzzles.GridShape hiding (size)
import Data.Puzzles.Elements

type Path = [String]

field :: Path -> Value -> Parser Value
field = field' . map T.pack
  where
    field' [] v                = pure v
    field' (f:fs) (Object v) = v .: f >>= field' fs
    field' _  _                = empty

parseFrom :: Path -> (Value -> Parser b) -> (Value -> Parser b)
parseFrom fs p v = field fs v >>= p

class FromChar a where
    parseChar :: Char -> Parser a

failChar :: Char -> String -> Parser a
failChar c expect = fail $ "got '" ++ [c] ++ "', expected " ++ expect

instance FromChar Char where
    parseChar = pure

class FromString a where
    parseString :: String -> Parser a

parseLine :: FromChar a => String -> Parser [a]
parseLine = mapM parseChar

instance FromChar Int where
    parseChar c
        | isDigit c  = digitToInt <$> parseChar c
        | otherwise  = fail $ "expected a digit, got '" ++ [c] ++ "'"

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
        filled = mapM (mapM parseChar) filledc
    parseJSON _          = fail "expected string"

data Border a = Border [a] [a] [a] [a]
    deriving Show

-- | This instance might be a lie.
instance Foldable Border where
    foldMap f (Border l r b t) = foldMap f l <> foldMap f r
                              <> foldMap f b <> foldMap f t

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
        lsparsed <- mapM (mapM parseChar) ls'
        bparsed  <- mapM parseChar b
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
        p = mapM (mapM (parseString . T.unpack)) ls
    parseJSON _          = empty

data Space = Space

instance FromChar Space where
    parseChar ' ' = pure Space
    parseChar _   = empty

data Blank = Blank
data Blank' = Blank'
data Empty = Empty

instance FromChar Blank where
    parseChar '.' = pure Blank
    parseChar _   = empty

parseCharJSON :: FromChar a => Value -> Parser a
parseCharJSON v = do
    [c] <- parseJSON v
    parseChar c

instance FromJSON Blank where
   parseJSON = parseCharJSON

instance FromChar Blank' where
    parseChar '.' = pure Blank'
    parseChar '-' = pure Blank'
    parseChar _   = empty

instance FromJSON Blank' where
    parseJSON (String ".") = pure Blank'
    parseJSON (String "-") = pure Blank'
    parseJSON _            = empty

instance FromChar Empty where
    parseChar ' ' = pure Empty
    parseChar _   = empty

instance FromString Blank where
    parseString "." = pure Blank
    parseString _   = empty

data PlainNode = PlainNode

instance FromChar PlainNode where
    parseChar 'o' = pure PlainNode
    parseChar _   = empty

instance FromChar MasyuPearl where
    parseChar '*' = pure MBlack
    parseChar 'o' = pure MWhite
    parseChar c   = failChar c "'*' or 'o'"

instance FromChar SlalomDiag where
    parseChar '/'  = pure SlalomForward
    parseChar '\\' = pure SlalomBackward
    parseChar _    = empty

instance FromChar Black where
    parseChar 'X' = pure Black
    parseChar 'x' = pure Black
    parseChar _   = empty

instance (FromChar a, FromChar b) => FromChar (Either a b) where
    parseChar c = Left <$> parseChar c <|> Right <$> parseChar c

instance (FromString a, FromString b) => FromString (Either a b) where
    parseString c = Left <$> parseString c <|> Right <$> parseString c

newtype Either' a b = Either' { unEither' :: Either a b }

instance (FromChar a, FromChar b) => FromChar (Either' a b) where
    parseChar c = Either' <$> parseChar c

instance (FromJSON a, FromJSON b) => FromJSON (Either' a b) where
    parseJSON v = Either' <$>
                      (Left <$> parseJSON v <|> Right <$> parseJSON v)

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

blankToMaybe :: Either Blank a -> Maybe a
blankToMaybe = either (const Nothing) Just

blankToMaybe' :: Either Blank' a -> Maybe a
blankToMaybe' = either (const Nothing) Just

rectToClueGrid :: Rect (Either Blank a) -> SGrid (Clue a)
rectToClueGrid = fmap blankToMaybe . rectToSGrid

rectToClueGrid' :: Rect (Either Blank' a) -> SGrid (Clue a)
rectToClueGrid' = fmap blankToMaybe' . rectToSGrid

rectToIrregGrid :: Rect (Either Empty a) -> SGrid a
rectToIrregGrid = fmap fromRight . filterG isRight . rectToSGrid
  where
    isRight = either (const False) (const True)
    fromRight (Right r) = r
    fromRight _         = error "no way"

newtype Shaded = Shaded { unShaded :: Bool }

instance FromChar Shaded where
    parseChar 'x'  = pure . Shaded $ True
    parseChar 'X'  = pure . Shaded $ True
    parseChar _    = pure . Shaded $ False

parseShadedGrid :: Value -> Parser (SGrid Bool)
parseShadedGrid v = rectToSGrid . fmap unShaded <$> parseJSON v

parseGrid :: FromChar a => Value -> Parser (SGrid a)
parseGrid v = rectToSGrid <$> parseJSON v

parseGridWith :: (Char -> Parser a) -> Value -> Parser (SGrid a)
parseGridWith pChar v = traverse pChar =<< parseGrid v

parseWithReplacement :: FromChar a =>
    (Char -> Maybe a) -> Char -> Parser a
parseWithReplacement replace c = maybe (parseChar c) pure (replace c)

parseCharMap :: FromJSON a => Value -> Parser (Map.Map Char a)
parseCharMap v = do
    m <- parseJSON v
    guard . all (\k -> length k == 1) . Map.keys $ m
    return $ Map.mapKeys head m

parseExtGrid :: (FromChar a, FromJSON a) => Value -> Parser (SGrid a)
parseExtGrid v@(String _) = parseGrid v
parseExtGrid v = do
    repl <- parseFrom ["replace"] parseCharMap v
    parseFrom ["grid"] (parseGridWith
                        (parseWithReplacement (`Map.lookup` repl))) v

parseClueGrid :: FromChar a => Value -> Parser (SGrid (Clue a))
parseClueGrid v = rectToClueGrid <$> parseJSON v

parseClueGrid' :: FromChar a => Value -> Parser (SGrid (Clue a))
parseClueGrid' v = rectToClueGrid' <$> parseJSON v

parseIrregGrid :: FromChar a => Value -> Parser (SGrid a)
parseIrregGrid v = rectToIrregGrid <$> parseJSON v

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

-- | Parse a grid with edges and values at nodes and in cells.
--
-- E.g. o-*-*-o
--      |1|2 3
--      *-o
-- to a grid of masyu pearls, a grid of integers, and some edges.
parseEdgeGrid :: (FromChar a, FromChar b) =>
                 Value -> Parser (SGrid a, SGrid b, [Edge])
parseEdgeGrid v = uncurry (,,) <$>
                  parseBoth <*>
                  parsePlainEdges v
  where
    parseBoth = do
        g <- parseGrid v
        (gn, gc) <- halveGrid g
        gn' <- parseGridChars gn
        gc' <- parseGridChars gc
        return (gn', gc')
    both f (x, y) = (f x, f y)
    halveGrid (Grid (Square w h) m)
        | odd w && odd h = pure (Grid snode (divkeys mnode),
                                 Grid scell (divkeys mcell))
        | otherwise      = fail "non-odd grid size"
      where
        w' = (w + 1) `div` 2
        h' = (h + 1) `div` 2
        snode = Square w' h'
        scell = Square (w' - 1) (h' - 1)
        mnode = Map.filterWithKey (const . uncurry (&&) . both even) m
        mcell = Map.filterWithKey (const . uncurry (&&) . both odd)  m
        divkeys = Map.mapKeys (both (`div` 2))

-- | Parse a grid of edges with values at the nodes.
--
-- E.g. o-*-*-o
--      | |
--      *-o
-- to a grid of masyu pearls and some edges.
parseNodeEdges :: FromChar a =>
                  Value -> Parser (SGrid a, [Edge])
parseNodeEdges v = proj13 <$> parseEdgeGrid v
  where
    proj13 :: (SGrid a, SGrid Empty, [Edge]) -> (SGrid a, [Edge])
    proj13 (x,_,z) = (x,z)

parseCellEdges :: FromChar a =>
                  Value -> Parser (SGrid a, [Edge])
parseCellEdges v = proj23 <$> parseEdgeGrid v
  where
    proj23 :: (SGrid PlainNode, SGrid a, [Edge]) -> (SGrid a, [Edge])
    proj23 (_,y,z) = (y,z)

data HalfDirs = HalfDirs {unHalfDirs :: [Dir]}

instance FromChar HalfDirs where
    parseChar c | c `elem` "└┴├┼" = pure . HalfDirs $ [V, H]
                | c `elem` "│┘┤"  = pure . HalfDirs $ [V]
                | c `elem` "─┌┬"  = pure . HalfDirs $ [H]
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
    parseThermo p | not (isStart p)           = pure Nothing
                  | not (isAlmostIsolated p)  = fail $ show p ++ " not almost isolated"
                  | otherwise                 = Just <$> parseThermo' p
    parseThermo' :: Cell Square -> Parser Thermometer
    parseThermo' p = do
        q <- next p
        maybe (fail "no succ for thermo bulb") (fmap (p:) . parseThermo'') q
    parseThermo'' :: Cell Square -> Parser Thermometer
    parseThermo'' p = do
        q <- next p
        maybe (pure [p]) (fmap (p:) . parseThermo'') q
    next :: Cell Square -> Parser (Maybe (Cell Square))
    next p = case succs p of
        []   -> pure Nothing
        [q]  -> pure (Just q)
        _    -> fail "multiple successors"
    succs      p = filter    (test ((==) . succ) p) . vertexNeighbours s $ p
    isStart    p = not . any (test ((==) . pred) p) . vertexNeighbours s $ p
    test f p q = maybe False (f (m' Map.! p)) (Map.lookup q m')
    isAlmostIsolated p = all disjointSucc . vertexNeighbours s $ p
      where
        disjointSucc q = null $ intersect (succs p) (succs' q)
        succs' q = maybe [] (const $ succs q) (Map.lookup q m')

parseThermoGrid :: ThermoRect -> Parser (SGrid Int, [Thermometer])
parseThermoGrid (Rect w h ls) = (,) (Grid s ints)
                              <$> parseThermos (Grid s alphas)
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
        :: Parser (BorderedRect Tight (Either Blank' Int))
    return (outside . fmap (either (const Nothing) Just) $ b,
            fmap unTight . rectToSGrid $ Rect w h ls)
  where outside (Border l r b t) = OC l r b t

instance FromChar a => FromString (Tightfit a) where
    parseString [c]           = Single <$> parseChar c
    parseString [c, '/',d]    = UR <$> parseChar c <*> parseChar d
    parseString [c,'\\',d]    = DR <$> parseChar c <*> parseChar d
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

newtype RefGrid a = RefGrid { unRG :: SGrid a }

data Ref = Ref { unRef :: Char }
    deriving Show

instance FromChar Ref where
    parseChar c | isAlpha c = pure (Ref c)
    parseChar _             = empty

hashmaptomap :: (Eq a, Hashable a, Ord a) => HMap.HashMap a b -> Map.Map a b
hashmaptomap = Map.fromList . HMap.toList

compose :: (Ord a, Ord b) => Map.Map a b -> Map.Map b c -> Maybe (Map.Map a c)
compose m1 m2 = mapM (`Map.lookup` m2) m1

instance FromJSON a => FromJSON (RefGrid a) where
    parseJSON (Object v) = RefGrid <$> do
        Grid s refs <- fmap (fmap ((:[]) . unRef)) . rectToClueGrid <$>
                       (v .: "grid" :: Parser (Rect (Either Blank Ref)))
        m <- hashmaptomap <$> v .: "clues"
        case compose (Map.mapMaybe id refs) m of
            Nothing -> mzero
            Just m' -> return $ Grid s m'
    parseJSON _ = empty

parseAfternoonGrid :: Value -> Parser (SGrid Shade)
parseAfternoonGrid v = do
    (Grid s _ , es) <- parseNodeEdges v :: Parser (SGrid Char, [Edge])
    let (m, b) = splitBorder s $ toMap es
    guard $ Map.null b
    return $ Grid (shrink s) m
  where
    shrink (Square w h) = Square (w-1) (h-1)
    toShade V = Shade False True
    toShade H = Shade True  False
    merge (Shade a b) (Shade c d)
        | a && c || b && d  = error "shading collision"
        | otherwise         = Shade (a || c) (b || d)
    toMap es = Map.fromListWith
        merge
        [(p, toShade d) | E p d <- es]
    splitBorder (Square w h) = Map.partitionWithKey
        (\(x, y) _ -> x < w - 1 && y < h - 1)

newtype ParseTapaClue = ParseTapaClue { unParseTapaClue :: TapaClue }

instance FromJSON ParseTapaClue where
    parseJSON v = do xs <- parseJSON v
                     guard $ length xs > 0 && length xs <= 4
                     return . ParseTapaClue . TapaClue $ xs

reorientOutside :: OutsideClues a -> OutsideClues a
reorientOutside (OC l r b t) = OC (reverse l) (reverse r) b t

parseCharOutside :: FromChar a => Value -> Parser (OutsideClues a)
parseCharOutside (Object v) = reorientOutside <$>
                              (OC <$>
                               pfield "left" <*> pfield "right" <*>
                               pfield "bottom" <*> pfield "top" )
  where
    pfield f = parseLine . fromMaybe [] =<< v .:? f
parseCharOutside _          = empty

parseOutside :: FromJSON a => Value -> Parser (OutsideClues a)
parseOutside (Object v) = reorientOutside <$>
                              (OC <$>
                               pfield "left" <*> pfield "right" <*>
                               pfield "bottom" <*> pfield "top" )
  where
    pfield f = pure . fromMaybe [] =<< v .:? f
parseOutside _          = empty

parseMultiOutsideClues :: FromJSON a => Value -> Parser (OutsideClues [a])
parseMultiOutsideClues (Object v) = rev <$> raw
  where
    raw = OC <$> v `ml` "left" <*> v `ml` "right" <*> v `ml` "bottom" <*> v `ml` "top"
    v' `ml` k = fromMaybe [] <$> v' .:? k
    rev (OC l r b t) = reorientOutside $
                       OC (map reverse l) r b (map reverse t)
parseMultiOutsideClues _ = empty

instance FromChar PrimeDiag where
    parseChar '.'  = pure $ PrimeDiag (False, False)
    parseChar '/'  = pure $ PrimeDiag (True,  False)
    parseChar '\\' = pure $ PrimeDiag (False, True)
    parseChar 'X'  = pure $ PrimeDiag (True,  True)
    parseChar _    = empty

instance FromChar Crossing where
    parseChar '+' = pure Crossing
    parseChar _   = fail "expected '+'"
