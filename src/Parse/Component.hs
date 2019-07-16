{-# LANGUAGE OverloadedStrings #-}

module Parse.Component where

import           Data.Yaml
import qualified Data.Map.Strict               as Map

import qualified Data.Elements                 as E
import           Data.Component
import           Data.Grid
import           Data.GridShape
import qualified Parse.Util                    as Util

parseComponent :: Value -> Parser (TaggedComponent a)
parseComponent = withObject "Component" $ \o -> do
  t     <- o .: "type" :: Parser String
  tag   <- parseTag o
  place <- parsePlacement o
  c     <- case t of
    "grid"    -> parseGrid o
    "regions" -> Regions <$> parseRegions o
    "nodes"   -> NodeGrid <$> parseNodeGrid o
    "cells"   -> parseCellGrid o
    "edges"   -> EdgeGrid <$> parseEdgeGrid o
    "full"    -> parseFullGrid o
    "note"    -> Note <$> parseNote o
    _         -> fail $ "unknown component type: " ++ t
  pure $ TaggedComponent tag (PlacedComponent place c)

parseTag :: Object -> Parser (Maybe Tag)
parseTag o = do
  t <- o .:? "tag" :: Parser (Maybe String)
  case t of
    Nothing         -> pure Nothing
    Just "puzzle"   -> pure (Just Puzzle)
    Just "solution" -> pure (Just Solution)
    Just "code"     -> pure (Just Code)
    Just x          -> fail $ "unknown tag: " ++ x

parsePlacement :: Object -> Parser Placement
parsePlacement o = do
  p <- o .:? "place" :: Parser (Maybe String)
  case p of
    Nothing          -> pure Atop
    Just "north"     -> pure North
    Just "west"      -> pure West
    Just "top-right" -> pure TopRight
    Just x           -> fail $ "unknown placement: " ++ x

data Shape = ShapeSquare | ShapeShifted

parseShape :: Object -> Parser Shape
parseShape o = do
  s <- o .:? "shape"
  case s of
    Nothing        -> pure ShapeSquare
    Just "square"  -> pure ShapeSquare
    Just "shifted" -> pure ShapeShifted
    Just x         -> fail $ "unknown shape: " ++ x

parseGrid :: Object -> Parser (Component a)
parseGrid o = do
  g  <- o .: "grid" >>= Util.parseIrregGrid
  s  <- o .: "style"
  gs <- case s of
    "default"           -> pure GridDefault
    "default-irregular" -> pure GridDefaultIrregular
    "dashed"            -> pure GridDashed
    "dots"              -> pure GridDots
    "plain"             -> pure GridPlain
    "plain-dashed"      -> pure GridPlainDashed
    _                   -> fail $ "unknown grid style: " ++ s
  sh <- parseShape o
  case sh of
    ShapeSquare  -> pure $ Grid gs g
    ShapeShifted -> case gs of
      GridPlain -> pure $ Pyramid (Map.mapKeys ShiftC g)
      _         -> fail $ "unsupported shifted grid style: " ++ s

parseRegions :: Object -> Parser (Grid C Char)
parseRegions o = do
  g <- o .: "grid"
  Util.parseGrid g

parseNodeGrid :: Object -> Parser (Grid N Decoration)
parseNodeGrid o = do
  g <- o .: "grid"
  r <- parseReplacements o
  Util.parseGridWith (parseDecorationWithReplacements r) g

parseCellGrid :: Object -> Parser (Component a)
parseCellGrid o = do
  sh <- parseShape o
  g  <- o .: "grid"
  r  <- parseReplacements o
  gg <- Util.parseGridWith (parseDecorationWithReplacements r) g
  pure $ case sh of
    ShapeSquare  -> CellGrid gg
    ShapeShifted -> CellPyramid $ Map.mapKeys ShiftC $ gg

parseEdgeGrid :: Object -> Parser (Map.Map (Edge N) Decoration)
parseEdgeGrid o = do
  g <- o .: "grid"
  r <- parseReplacements o
  Util.parseAnnotatedEdgesWith (parseDecorationWithReplacements r) g

parseFullGrid :: Object -> Parser (Component a)
parseFullGrid o = do
  g <- o .: "grid"
  r <- parseReplacements o
  let pdec = parseDecorationWithReplacements r
  (ns, cs, es) <- Util.parseEdgeGridWith pdec pdec pdec g
  return $ FullGrid ns cs es

parseNote :: Object -> Parser [Decoration]
parseNote o = do
  ds <- o .: "contents"
  sequence . map parseExtendedDecoration $ ds

parseReplacements :: Object -> Parser (Map.Map Char Decoration)
parseReplacements o = do
  ms <- o .:? "substitute"
  case ms of
    Nothing -> pure Map.empty
    Just s  -> Util.parseCharMapWith parseExtendedDecoration s

parseDecorationWithReplacements
  :: Map.Map Char Decoration -> Char -> Parser Decoration
parseDecorationWithReplacements repl c = case Map.lookup c repl of
  Just dec -> pure dec
  Nothing  -> parseDecoration c

parseDecoration :: Char -> Parser Decoration
parseDecoration c = return $ case c of
  '.'  -> Blank
  'o'  -> DecKropkiDot E.KWhite
  '*'  -> DecKropkiDot E.KBlack
  '/'  -> DarkDiagonal $ E.PrimeDiag (True, False)
  '\\' -> DarkDiagonal $ E.PrimeDiag (False, True)
  '#'  -> Shade
  '-'  -> Edge Horiz
  '|'  -> Edge Vert
  '>'  -> TriangleRight
  'v'  -> TriangleDown
  _    -> Letter c

parseExtendedDecoration :: Util.IntString -> Parser Decoration
parseExtendedDecoration (Util.IntString s) = case words s of
  [w1] -> case w1 of
    "kropki-white"      -> pure $ DecKropkiDot E.KWhite
    "kropki-black"      -> pure $ DecKropkiDot E.KBlack
    "small-pearl-white" -> pure $ SmallPearl E.MWhite
    "small-pearl-black" -> pure $ SmallPearl E.MBlack
    "pearl-white"       -> pure $ Pearl E.MWhite
    "pearl-black"       -> pure $ Pearl E.MBlack
    "blank"             -> pure Blank
    "afternoon-west"    -> pure $ AfternoonWest
    "afternoon-south"   -> pure $ AfternoonSouth
    "light-diagonal-forward" ->
      pure $ LightDiagonal $ E.PrimeDiag (True, False)
    "light-diagonal-back"   -> pure $ LightDiagonal $ E.PrimeDiag (False, True)
    "light-diagonal-both"   -> pure $ LightDiagonal $ E.PrimeDiag (True, True)
    "dark-diagonal-forward" -> pure $ DarkDiagonal $ E.PrimeDiag (True, False)
    "dark-diagonal-back"    -> pure $ DarkDiagonal $ E.PrimeDiag (False, True)
    "dark-diagonal-both"    -> pure $ DarkDiagonal $ E.PrimeDiag (True, True)
    "edge-horiz"            -> pure $ Edge Horiz
    "edge-vert"             -> pure $ Edge Vert
    "thin-edge-horiz"       -> pure $ ThinEdge Horiz
    "thin-edge-vert"        -> pure $ ThinEdge Vert
    "sol-edge-horiz"        -> pure $ SolEdge Horiz
    "sol-edge-vert"         -> pure $ SolEdge Vert
    "dot"                   -> pure $ Dot
    "small-dot"             -> pure $ SmallDot
    "star"                  -> pure $ Star
    "shade"                 -> pure $ Shade
    "dark-shade"            -> pure $ DarkShade
    "black"                 -> pure $ Black
    "light-shade"           -> pure $ LightShade
    "triangle-right"        -> pure $ TriangleRight
    "triangle-down"         -> pure $ TriangleDown
    "miniloop"              -> pure $ MiniLoop
    "ship-square"           -> pure $ ShipSquare
    "ship-end-left"         -> pure $ Ship R
    "ship-end-right"        -> pure $ Ship L
    "ship-end-top"          -> pure $ Ship D
    "ship-end-bottom"       -> pure $ Ship U
    "tent"                  -> pure $ Tent
    "tree"                  -> pure $ Tree
    _                       -> pure $ Letters s
  [w1, w2] -> case w1 of
    "triangle-right" -> pure $ LabeledTriangleRight w2
    "triangle-down"  -> pure $ LabeledTriangleDown w2
    "arrow-right"    -> pure $ LabeledArrow R w2
    "arrow-left"     -> pure $ LabeledArrow L w2
    "arrow-up"       -> pure $ LabeledArrow U w2
    "arrow-down"     -> pure $ LabeledArrow D w2
    "inverted-arrow-down"     -> pure $ InvertedLabeledArrow D w2
    "inverted-arrow-right"    -> pure $ InvertedLabeledArrow R w2
    "inverted-arrow-left"     -> pure $ InvertedLabeledArrow L w2
    "inverted-arrow-up"       -> pure $ InvertedLabeledArrow U w2
    "inverted-letters" -> pure $ InvertedLetters w2
    _                -> fail $ "unknown unary function: " ++ w1
  _ -> fail $ "unknown decoration: " ++ show s
