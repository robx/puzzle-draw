{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Parse.Util where

import Control.Applicative
import Control.Arrow
import Control.Monad hiding (mapM)
import Data.Char
  ( digitToInt,
    isAlpha,
    isDigit,
  )
import Data.Either (isRight)
import Data.Elements
import Data.Grid
import Data.GridShape
import qualified Data.HashMap.Strict as HMap
import Data.Lib
import Data.List
  ( intersect,
    sortBy,
  )
import qualified Data.Map.Strict as Map
import Data.Maybe
  ( catMaybes,
    fromJust,
    fromMaybe,
    isJust,
  )
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Traversable (mapM)
import Data.Yaml
import Parse.Parsec
import Text.Read (readMaybe)
import Prelude hiding (mapM)

type Path = [String]

field :: Path -> Value -> Parser Value
field = field' . map T.pack
  where
    field' [] v = pure v
    field' (f : fs) (Object v) = v .: f >>= field' fs
    field' (f : _) _ = fail $ "expected field '" ++ T.unpack f ++ "'"

parseFrom :: Path -> (Value -> Parser b) -> Value -> Parser b
parseFrom fs p v =
  do
    v' <- field fs v
    p v'

parseFromOptional :: Path -> (Value -> Parser b) -> Value -> Parser (Maybe b)
parseFromOptional fs p v =
  do
    mv <- optional (field fs v)
    sequence (p <$> mv)

chars :: [Char] -> Char -> Parser Char
chars cs c =
  if c `elem` cs
    then pure c
    else (fail $ "got '" ++ [c] ++ "', expected '" ++ cs ++ "'")

char :: Char -> Char -> Parser Char
char c = chars [c]

class FromChar a where
  parseChar :: Char -> Parser a

instance FromChar Char where
  parseChar = pure

class FromString a where
  parseString :: String -> Parser a

instance FromString Char where
  parseString [c] = pure c
  parseString s = fail $ "expected a single character, got `" ++ s ++ "`"

parseStringJSON :: FromString a => Value -> Parser a
parseStringJSON v = parseJSON v >>= parseString

parseLine :: FromChar a => String -> Parser [a]
parseLine = mapM parseChar

instance FromChar Int where
  parseChar c
    | isDigit c = digitToInt <$> parseChar c
    | otherwise = fail $ "expected a digit, got '" ++ [c] ++ "'"

newtype Alpha = Alpha {unAlpha :: Char}
  deriving (Show, Ord, Eq)

instance FromChar Alpha where
  parseChar c
    | isAlpha c = Alpha <$> parseChar c
    | otherwise = fail $ "expected a letter"

-- | Helper to parse strings from number-formatted YAML fields.
--   Somewhat dodgy.
newtype IntString = IntString {unIntString :: String}

instance FromJSON IntString where
  parseJSON v@(Number _) = IntString . (show :: Int -> String) <$> parseJSON v
  parseJSON v = IntString <$> parseJSON v

-- | A rectangle. Each row has length `w`.
data Rect a = Rect !Int !Int [[a]]
  deriving (Show)

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
  parseJSON _ = fail "expected string"

parseRectWith :: FromChar a => (a -> Parser b) -> Value -> Parser [[b]]
parseRectWith p v = do
  Rect _ _ cs <- parseJSON v
  traverse (traverse p) cs

data Border a = Border [a] [a] [a] [a]
  deriving (Show)

-- | This instance might be a lie.
instance Foldable Border where
  foldMap f (Border l r b t) =
    foldMap f l <> foldMap f r
      <> foldMap f b
      <> foldMap f t

instance Traversable Border where
  sequenceA (Border l r b t) =
    Border <$> sequenceA l
      <*> sequenceA r
      <*> sequenceA b
      <*> sequenceA t

instance Functor Border where
  f `fmap` (Border l r b t) = Border (f <$> l) (f <$> r) (f <$> b) (f <$> t)

data BorderedRect a b = BorderedRect !Int !Int [[a]] (Border b)
  deriving (Show)

parseBorderedRect ::
  (Char -> Parser a) ->
  (Char -> Parser b) ->
  Value ->
  Parser (BorderedRect a b)
parseBorderedRect parseIn parseOut v = do
  Rect w h ls <- parseJSON v
  let b =
        Border
          (reverse . map head . middle h $ ls)
          (reverse . map last . middle h $ ls)
          (middle w . last $ ls)
          (middle w . head $ ls)
      ls' = map (middle w) . middle h $ ls
  mapM_
    ((parseChar :: Char -> Parser Space) . flip ($) ls)
    [head . head, head . last, last . head, last . last]
  lsparsed <- mapM (mapM parseIn) ls'
  bparsed <- mapM parseOut b
  return $ BorderedRect (w - 2) (h - 2) lsparsed bparsed
  where
    middle len = take (len - 2) . drop 1

instance (FromChar a, FromChar b) => FromJSON (BorderedRect a b) where
  parseJSON = parseBorderedRect parseChar parseChar

newtype SpacedRect a = SpacedRect {unSpaced :: Rect a}

instance FromString a => FromJSON (SpacedRect a) where
  parseJSON (String t) =
    if w == wmin
      then SpacedRect . Rect w h <$> p
      else empty
    where
      ls = map T.words . T.lines $ t
      w = maximum . map length $ ls
      wmin = minimum . map length $ ls
      h = length ls
      p = mapM (mapM (parseString . T.unpack)) ls
  parseJSON _ = empty

instance FromChar () where
  parseChar = fmap (const ()) . chars ['.', ' ']

data Space = Space

instance FromChar Space where
  parseChar = fmap (const Space) . char ' '

data Blank = Blank

data Blank' = Blank'

data Blank'' = Blank''

data Empty = Empty

instance FromChar Blank where
  parseChar = fmap (const Blank) . char '.'

parseCharJSON :: FromChar a => Value -> Parser a
parseCharJSON v = do
  [c] <- parseJSON v
  parseChar c

instance FromJSON Blank where
  parseJSON = parseCharJSON

instance FromChar Blank' where
  parseChar = fmap (const Blank') . chars ['.', '-']

instance FromJSON Blank' where
  parseJSON (String ".") = pure Blank'
  parseJSON (String "-") = pure Blank'
  parseJSON _ = fail "expected '.-'"

instance FromChar Blank'' where
  parseChar = fmap (const Blank'') . chars ['.', ' ', '-', '|']

instance FromChar Empty where
  parseChar = fmap (const Empty) . char ' '

instance FromString Blank where
  parseString "." = pure Blank
  parseString _ = fail "expected '.'"

instance FromChar PlainNode where
  parseChar = fmap (const PlainNode) . char 'o'

instance FromChar MasyuPearl where
  parseChar = fmap f . chars ['*', 'o']
    where
      f '*' = MBlack
      f 'o' = MWhite
      f _ = impossible

instance FromChar SlalomDiag where
  parseChar '/' = pure SlalomForward
  parseChar '\\' = pure SlalomBackward
  parseChar _ = empty

instance FromChar Black where
  parseChar = fmap (const Black) . chars "xX#"

instance FromChar Fish where
  parseChar = fmap (const Fish) . char '*'

instance FromChar Star where
  parseChar = fmap (const Star) . char '*'

instance FromChar MEnd where
  parseChar = fmap (const MEnd) . chars "o*"

instance FromChar GalaxyCentre where
  parseChar = fmap (const GalaxyCentre) . char 'o'

instance FromChar Tree where
  parseChar = fmap (const Tree) . char '*'

instance FromChar PlacedTent where
  parseChar = fmap Tent . parseChar

instance FromChar Pentomino where
  parseChar c =
    if c `elem` ("FILNPTUVWXYZ" :: String)
      then pure (Pentomino c)
      else fail "expected FILNPTUVWXYZ"

instance (FromChar a, FromChar b) => FromChar (Either a b) where
  parseChar c = Left <$> parseChar c <|> Right <$> parseChar c

instance (FromString a, FromString b) => FromString (Either a b) where
  parseString c = Left <$> parseString c <|> Right <$> parseString c

newtype Either' a b = Either' {unEither' :: Either a b}

instance (FromChar a, FromChar b) => FromChar (Either' a b) where
  parseChar c = Either' <$> parseChar c

instance (FromJSON a, FromJSON b) => FromJSON (Either' a b) where
  parseJSON v =
    Either'
      <$> (Left <$> parseJSON v <|> Right <$> parseJSON v)

instance FromChar a => FromChar (Maybe a) where
  parseChar = optional . parseChar

listListToMap :: [[a]] -> Grid Coord a
listListToMap ls =
  Map.fromList
    . concat
    . zipWith (\y -> zipWith (\x -> (,) (x, y)) [0 ..]) [h - 1, h - 2 ..]
    $ ls
  where
    h = length ls

rectToCoordGrid :: Rect a -> Grid Coord a
rectToCoordGrid (Rect _ _ ls) = listListToMap ls

blankToMaybe :: Either Blank a -> Maybe a
blankToMaybe = either (const Nothing) Just

blankToMaybe' :: Either Blank' a -> Maybe a
blankToMaybe' = either (const Nothing) Just

blankToMaybe'' :: Either Blank'' a -> Maybe a
blankToMaybe'' = either (const Nothing) Just

rectToIrregGrid :: Rect (Either Empty a) -> Grid Coord a
rectToIrregGrid = fmap fromRight . Map.filter isRight . rectToCoordGrid
  where
    fromRight (Right r) = r
    fromRight _ = impossible

newtype Shaded = Shaded {unShaded :: Bool}

instance FromChar Shaded where
  parseChar 'x' = pure . Shaded $ True
  parseChar 'X' = pure . Shaded $ True
  parseChar _ = pure . Shaded $ False

parseShadedGrid :: Key k => Value -> Parser (Grid k Bool)
parseShadedGrid v = fmap unShaded <$> parseGrid v

parseCoordGrid :: (FromChar a) => Value -> Parser (Grid Coord a)
parseCoordGrid v = rectToCoordGrid <$> parseJSON v

parseGrid :: (Key k, FromChar a) => Value -> Parser (Grid k a)
parseGrid v = fromCoordGrid <$> parseCoordGrid v

parseGridWith :: Key k => (Char -> Parser a) -> Value -> Parser (Grid k a)
parseGridWith pChar v = traverse pChar =<< parseGrid v

parseWithReplacement :: FromChar a => (Char -> Maybe a) -> Char -> Parser a
parseWithReplacement replace = parseWithReplacementWith replace parseChar

parseWithReplacementWith ::
  (Char -> Maybe a) -> (Char -> Parser a) -> Char -> Parser a
parseWithReplacementWith replace p c = maybe (p c) pure (replace c)

parseSpacedGrid :: (Key k, FromString a) => Value -> Parser (Grid k a)
parseSpacedGrid v = fromCoordGrid . rectToCoordGrid . unSpaced <$> parseJSON v

parseCharMap :: FromJSON a => Value -> Parser (Map.Map Char a)
parseCharMap = parseCharMapWith parseJSON

parseCharMapWith ::
  FromJSON b => (b -> Parser a) -> Value -> Parser (Map.Map Char a)
parseCharMapWith p v = do
  mj <- parseJSON v
  m <- sequence $ p <$> mj
  guard . all (\k -> length k == 1) . Map.keys $ m
  return $ Map.mapKeys head m

parseExtGridFrom ::
  (Key k, FromJSON a, FromChar b) => Path -> Path -> (a -> b) -> Value -> Parser (Grid k b)
parseExtGridFrom gridp replp f v = do
  repl <- (fmap f . fromMaybe Map.empty) <$> parseFromOptional replp parseCharMap v
  parseFrom
    gridp
    (parseGridWith (parseWithReplacement (`Map.lookup` repl)))
    v

parseExtGrid' ::
  (Key k, FromJSON a, FromChar b) => (a -> b) -> Value -> Parser (Grid k b)
parseExtGrid' _ v@(String _) = parseGrid v
parseExtGrid' f v = do
  repl <- fmap f <$> parseFrom ["replace"] parseCharMap v
  parseFrom
    ["grid"]
    (parseGridWith (parseWithReplacement (`Map.lookup` repl)))
    v

parseExtGrid :: (Key k, FromChar a, FromJSON a) => Value -> Parser (Grid k a)
parseExtGrid = parseExtGrid' id

parseExtClueGrid ::
  (Key k, FromChar a, FromJSON a) => Value -> Parser (Grid k (Maybe a))
parseExtClueGrid v = fmap blankToMaybe <$> parseExtGrid' Right v

parseExtClueGridFrom ::
  (Key k, FromChar a, FromJSON a) => Path -> Path -> Value -> Parser (Grid k (Maybe a))
parseExtClueGridFrom grid repl v = fmap blankToMaybe <$> parseExtGridFrom grid repl Right v

fromCoordGrid :: Key k => Grid Coord a -> Grid k a
fromCoordGrid = Map.mapKeys fromCoord

fromCoordEdge :: Key k => Edge Coord -> Edge k
fromCoordEdge (E c d) = E (fromCoord c) d

fromCoordEdges :: Key k => [Edge Coord] -> [Edge k]
fromCoordEdges = map fromCoordEdge

parseClueGrid :: (FromChar a, Key k) => Value -> Parser (Grid k (Maybe a))
parseClueGrid v = fmap blankToMaybe <$> parseGrid v

parseClueGrid' :: (FromChar a, Key k) => Value -> Parser (Grid k (Maybe a))
parseClueGrid' v = fmap blankToMaybe' <$> parseGrid v

parseIrregGrid :: (Key k, FromChar a) => Value -> Parser (Grid k a)
parseIrregGrid v = fromCoordGrid . rectToIrregGrid <$> parseJSON v

-- parses a string like
--  o-o-o
--  |   |
--  o-o o
--    | |
--    o-o
parseEdges :: Key k => Value -> Parser [Edge k]
parseEdges v = filterPlainEdges <$> parseAnnotatedEdges v

filterPlainEdges :: Map.Map (Edge k) Char -> [Edge k]
filterPlainEdges = Map.keys . Map.filterWithKey p
  where
    p (E _ Horiz) '-' = True
    p (E _ Vert) '|' = True
    p _ _ = False

parseAnnotatedEdges ::
  (Key k, FromChar a) => Value -> Parser (Map.Map (Edge k) a)
parseAnnotatedEdges = parseAnnotatedEdgesWith parseChar

parseAnnotatedEdgesWith ::
  (Key k) => (Char -> Parser a) -> Value -> Parser (Map.Map (Edge k) a)
parseAnnotatedEdgesWith p v = do
  g <- readEdges <$> parseCoordGrid v
  Map.mapKeys fromCoordEdge <$> traverse p g

readEdges :: Grid Coord Char -> Map.Map (Edge Coord) Char
readEdges =
  Map.mapKeysMonotonic fromJust
    . Map.filterWithKey (const . isJust)
    . Map.mapKeys toEdge
  where
    toEdge c@(x, y) = case (x `mod` 2, y `mod` 2) of
      (1, 0) -> Just $ E (div2 c) Horiz
      (0, 1) -> Just $ E (div2 c) Vert
      _ -> Nothing
    div2 (x', y') = (x' `div` 2, y' `div` 2)

parseGridChars :: FromChar a => Grid k Char -> Parser (Grid k a)
parseGridChars = traverse parseChar

-- | Parse a grid with edges and values at nodes and in cells.
--
-- E.g. o-*-*-o
--      |1|2 3
--      *-o
-- to a grid of masyu pearls, a grid of integers, and some annotated edges.
parseEdgeGrid ::
  (FromChar a, FromChar b, FromChar c) =>
  Value ->
  Parser (Grid N a, Grid C b, Map.Map (Edge N) c)
parseEdgeGrid = parseEdgeGridWith parseChar parseChar parseChar

parseEdgeGridWith ::
  (Char -> Parser a) ->
  (Char -> Parser b) ->
  (Char -> Parser c) ->
  Value ->
  Parser (Grid N a, Grid C b, Map.Map (Edge N) c)
parseEdgeGridWith pn pc pe v =
  uncurry (,,) <$> parseBoth <*> parseAnnotatedEdgesWith pe v
  where
    parseBoth = do
      g <- parseCoordGrid v
      let (gn, gc) = halveGrid g
      gn' <- traverse pn gn
      gc' <- traverse pc gc
      return (gn', gc')
    both f (x, y) = (f x, f y)
    halveGrid m =
      (fromCoordGrid . divkeys $ mnode, fromCoordGrid . divkeys $ mcell)
      where
        mnode = Map.filterWithKey (const . uncurry (&&) . both even) m
        mcell = Map.filterWithKey (const . uncurry (&&) . both odd) m
        divkeys = Map.mapKeys (both (`div` 2))

parsePlainEdgeGrid ::
  (FromChar a, FromChar b) => Value -> Parser (Grid N a, Grid C b, [Edge N])
parsePlainEdgeGrid v =
  (\(a, b, c) -> (a, b, filterPlainEdges c)) <$> parseEdgeGrid v

-- | Parse a grid of edges with values at the nodes.
--
-- E.g. o-*-*-o
--      | |
--      *-o
-- to a grid of masyu pearls and some edges.
parseNodeEdges :: FromChar a => Value -> Parser (Grid N a, [Edge N])
parseNodeEdges v = proj13 <$> parsePlainEdgeGrid v
  where
    proj13 :: (Grid N a, Grid C Char, [Edge N]) -> (Grid N a, [Edge N])
    proj13 (x, _, z) = (x, z)

parseCellEdges :: FromChar a => Value -> Parser (Grid C a, [Edge N])
parseCellEdges v = proj23 <$> parsePlainEdgeGrid v
  where
    proj23 :: (Grid N Char, Grid C a, [Edge N]) -> (Grid C a, [Edge N])
    proj23 (_, y, z) = (y, z)

instance FromChar Dir' where
  parseChar 'u' = pure U
  parseChar 'U' = pure U
  parseChar 'd' = pure D
  parseChar 'D' = pure D
  parseChar 'r' = pure R
  parseChar 'R' = pure R
  parseChar 'l' = pure L
  parseChar 'L' = pure L
  parseChar _ = fail "expected 'uUdDrRlL'"

type ThermoRect = Rect (Either Blank (Either Int Alpha))

partitionEithers ::
  Ord k => Map.Map k (Either a b) -> (Map.Map k a, Map.Map k b)
partitionEithers = Map.foldrWithKey insertEither (Map.empty, Map.empty)
  where
    insertEither k = either (first . Map.insert k) (second . Map.insert k)

parseThermos :: Grid C Alpha -> Parser [Thermometer]
parseThermos m = catMaybes <$> mapM parseThermo (Map.keys m')
  where
    m' = fmap unAlpha m
    parseThermo :: C -> Parser (Maybe Thermometer)
    parseThermo p
      | not (isStart p) = pure Nothing
      | not (isAlmostIsolated p) = fail $ show p ++ " not almost isolated"
      | otherwise = Just <$> parseThermo' p
    parseThermo' :: C -> Parser Thermometer
    parseThermo' p = do
      q <- next p
      maybe (fail "no succ for thermo bulb") (fmap (p :) . parseThermo'') q
    parseThermo'' :: C -> Parser Thermometer
    parseThermo'' p = do
      q <- next p
      maybe (pure [p]) (fmap (p :) . parseThermo'') q
    next :: C -> Parser (Maybe C)
    next p = case succs p of
      [] -> pure Nothing
      [q] -> pure (Just q)
      _ -> fail "multiple successors"
    succs p = filter (test ((==) . succ) p) . vertexNeighbours $ p
    isStart p = not . any (test ((==) . pred) p) . vertexNeighbours $ p
    test f p q = maybe False (f (m' Map.! p)) (Map.lookup q m')
    isAlmostIsolated p = all disjointSucc . vertexNeighbours $ p
      where
        disjointSucc q = null $ intersect (succs p) (succs' q)
        succs' q = maybe [] (const $ succs q) (Map.lookup q m')

parseThermoGrid :: ThermoRect -> Parser (Grid C (Maybe Int), [Thermometer])
parseThermoGrid (Rect _ _ ls) = (,) ints <$> parseThermos alphas
  where
    m = fromCoordGrid $ listListToMap ls
    ints = either (const Nothing) (either Just (const Nothing)) <$> m
    alphas =
      fmap fromRight
        . Map.filter isRight
        . fmap fromRight
        . Map.filter isRight
        $ m
    fromRight (Left _) = impossible
    fromRight (Right x) = x

parseOutsideGrid ::
  Key k =>
  (Char -> Parser a) ->
  (Char -> Parser b) ->
  Value ->
  Parser (OutsideClues k b, Grid k a)
parseOutsideGrid parseIn parseOut v = do
  BorderedRect w h ls b <- parseBorderedRect parseIn parseOut v
  return (outside b, fromCoordGrid . rectToCoordGrid $ Rect w h ls)
  where
    outside (Border l r b t) = OC l r b t

parseOutsideGridMap ::
  (Key k, FromChar a, FromChar b) =>
  (a -> c) ->
  (b -> d) ->
  Value ->
  Parser (OutsideClues k d, Grid k c)
parseOutsideGridMap mapIn mapOut v = do
  (o, g) <- parseOutsideGrid parseChar parseChar v
  return (mapOut <$> o, mapIn <$> g)

newtype Tight = Tight {unTight :: Tightfit ()}

instance FromChar Tight where
  parseChar '.' = pure . Tight $ TightSingle ()
  parseChar '/' = pure . Tight $ TightUR () ()
  parseChar '\\' = pure . Tight $ TightDR () ()
  parseChar _ = empty

parseTightOutside ::
  Value -> Parser (OutsideClues C (Maybe Int), Grid C (Tightfit ()))
parseTightOutside = parseOutsideGridMap unTight unBlank'
  where
    unBlank' :: Either Blank' Int -> Maybe Int
    unBlank' = either (const Nothing) Just

instance FromChar a => FromString (Tightfit a) where
  parseString [c] = TightSingle <$> parseChar c
  parseString [c, '/', d] = TightUR <$> parseChar c <*> parseChar d
  parseString [c, '\\', d] = TightDR <$> parseChar c <*> parseChar d
  parseString _ = empty

newtype PMarkedWord = PMW {unPMW :: MarkedWord}

parseNWords :: Int -> String -> Parser [String]
parseNWords n s
  | length ws == n = pure ws
  | otherwise = empty
  where
    ws = words s

parseDoublePair :: FromString a => Value -> Parser ((a, a), (a, a))
parseDoublePair v =
  (,)
    <$> ((,) <$> ((!! 0) <$> x) <*> ((!! 1) <$> x))
    <*> ((,) <$> ((!! 2) <$> x) <*> ((!! 3) <$> x))
  where
    x = parseJSON v >>= parseNWords 4 >>= mapM parseString

instance FromJSON PMarkedWord where
  parseJSON v = PMW . uncurry MW <$> parseDoublePair v

instance FromString Int where
  parseString s = maybe empty pure $ readMaybe s

newtype PCompassC = PCC {unPCC :: CompassC}

instance FromJSON PCompassC where
  parseJSON (String t) = comp . map T.unpack . T.words $ t
    where
      c "." = pure Nothing
      c x = Just <$> parseString x
      comp [n, e, s, w] = PCC <$> (CC <$> c n <*> c e <*> c s <*> c w)
      comp _ = empty
  parseJSON _ = empty

newtype PSlovakClue = PSlovakClue {unPSlovakClue :: SlovakClue}

instance FromJSON PSlovakClue where
  parseJSON (String t) = svk . map T.unpack . T.words $ t
    where
      svk [s, c] = PSlovakClue <$> (SlovakClue <$> parseString s <*> parseString c)
      svk _ = fail "expect two integers"
  parseJSON _ = fail "expect string of two integers"

newtype RefGrid k a = RefGrid {unRG :: Grid k (Maybe a)}

hashmaptomap :: Ord a => HMap.HashMap a b -> Map.Map a b
hashmaptomap = Map.fromList . HMap.toList

compose :: Ord b => Map.Map a b -> Map.Map b c -> Maybe (Map.Map a c)
compose m1 m2 = mapM (`Map.lookup` m2) m1

newtype MaybeMap k a = MM {unMaybeMap :: Map.Map k (Maybe a)}

instance Functor (MaybeMap k) where
  fmap f (MM m) = MM (fmap (fmap f) m)

instance Foldable (MaybeMap k) where
  foldMap f (MM m) = foldMap (foldMap f) m

instance Traversable (MaybeMap k) where
  traverse f m = MM <$> traverse (traverse f) (unMaybeMap m)

compose' ::
  Ord b => Map.Map a (Maybe b) -> Map.Map b c -> Maybe (Map.Map a (Maybe c))
compose' m1 m2 = unMaybeMap <$> mapM (`Map.lookup` m2) (MM m1)

instance (Key k, FromJSON a) => FromJSON (RefGrid k a) where
  parseJSON v = RefGrid <$> do
    refs <-
      fmap (fmap ((: []) . unAlpha) . blankToMaybe)
        <$> parseFrom ["grid"] parseGrid v
    m <- hashmaptomap <$> parseFrom ["clues"] parseJSON v
    case compose' refs m of
      Nothing -> mzero
      Just m' -> return m'

newtype ParseTapaClue = ParseTapaClue {unParseTapaClue :: TapaClue}

instance FromJSON ParseTapaClue where
  parseJSON v = do
    xs <- parseJSON v
    guard $ length xs > 0 && length xs <= 4
    return . ParseTapaClue . TapaClue $ xs

reorientOutside :: OutsideClues k a -> OutsideClues k a
reorientOutside (OC l r b t) = OC (reverse l) (reverse r) b t

parseCharOutside :: FromChar a => Value -> Parser (OutsideClues k a)
parseCharOutside (Object v) =
  reorientOutside
    <$> ( OC <$> pfield "left" <*> pfield "right" <*> pfield "bottom"
            <*> pfield
              "top"
        )
  where
    pfield f = parseLine . fromMaybe [] =<< v .:? f
parseCharOutside _ = empty

parseOutside :: FromJSON a => Value -> Parser (OutsideClues k a)
parseOutside (Object v) =
  reorientOutside
    <$> ( OC <$> pfield "left" <*> pfield "right" <*> pfield "bottom"
            <*> pfield
              "top"
        )
  where
    pfield f = pure . fromMaybe [] =<< v .:? f
parseOutside _ = empty

parseMultiOutsideClues :: FromJSON a => Value -> Parser (OutsideClues k [a])
parseMultiOutsideClues (Object v) = rev <$> raw
  where
    raw =
      OC <$> v `ml` "left" <*> v `ml` "right" <*> v `ml` "bottom" <*> v `ml` "top"
    v' `ml` k = fromMaybe [] <$> v' .:? k
    rev (OC l r b t) = reorientOutside $ OC (map reverse l) r b (map reverse t)
parseMultiOutsideClues _ = empty

parseCoordLoop :: Value -> Parser VertexLoop
parseCoordLoop v = sortCoords <$> parseClueGrid v
  where
    sortCoords :: Grid N (Maybe Char) -> VertexLoop
    sortCoords = map fst . sortBy (comparing snd) . Map.toList . clues

instance FromString DigitRange where
  parseString s = do
    let (a, b) = break (== '-') s
    b' <- case b of
      ('-' : cs) -> pure cs
      _ -> fail "exected '-' in range"
    DigitRange <$> parseString a <*> parseString b'

newtype PFraction = PFraction {unPFraction :: Fraction}

instance FromJSON PFraction where
  parseJSON v = PFraction <$> toStringParser fraction v

instance FromChar Crossing where
  parseChar '+' = pure Crossing
  parseChar _ = fail "expected '+'"

instance FromChar KropkiDot where
  parseChar '*' = pure KBlack
  parseChar 'o' = pure KWhite
  parseChar ' ' = pure KNone
  parseChar '.' = pure KNone
  parseChar _ = fail "expected '*o '"

instance FromChar Relation where
  parseChar '<' = pure RLess
  parseChar '>' = pure RGreater
  parseChar '=' = pure REqual
  parseChar ' ' = pure RUndetermined
  parseChar _ = fail "expected '<>= '"

parseGreaterClues :: Value -> Parser [GreaterClue]
parseGreaterClues v = do
  Rect _ _ ls <- parseJSON v
  mapM parseGreaterClue ls

parseGreaterClue :: [Char] -> Parser GreaterClue
parseGreaterClue [] = pure []
parseGreaterClue xs = p RUndetermined xs
  where
    p rel ('.' : cs) = (rel :) <$> q cs
    p _ (' ' : _) = pure [] -- Rect fills the lines up with spaces...
    p _ _ = fail "expected '.'"
    q [] = pure []
    q (r : cs) = do
      rel <- parseChar r
      p rel cs
