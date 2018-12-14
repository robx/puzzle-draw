{-# LANGUAGE OverloadedStrings #-}

module Parse.Component where

import           Data.Yaml
import qualified Data.Map.Strict               as Map

import           Data.Elements
import           Data.Component
import           Data.Grid
import           Data.GridShape
import qualified Parse.Util                    as Util

parseComponent :: Value -> Parser TaggedComponent
parseComponent = withObject "Component" $ \o -> do
  t     <- o .: "type" :: Parser String
  tag   <- parseTag o
  place <- parsePlacement o
  c     <- case t of
    "grid"    -> parseGrid o
    "regions" -> Regions <$> parseRegions o
    "nodes"   -> NodeGrid <$> parseNodeGrid o
    "cells"   -> CellGrid <$> parseCellGrid o
    "edges"   -> EdgeGrid <$> parseEdgeGrid o
    "full"    -> parseFullGrid o
    _         -> fail $ "unknown component type: " ++ t
  pure $ TaggedComponent tag (PlacedComponent place c)

parseTag :: Object -> Parser (Maybe Tag)
parseTag o = do
  t <- o .:? "tag" :: Parser (Maybe String)
  case t of
    Nothing         -> pure Nothing
    Just "puzzle"   -> pure (Just Puzzle)
    Just "solution" -> pure (Just Solution)
    Just x          -> fail $ "unknown tag: " ++ x

parsePlacement :: Object -> Parser Placement
parsePlacement o = do
  p <- o .:? "place" :: Parser (Maybe String)
  case p of
    Nothing      -> pure Atop
    Just "north" -> pure North
    Just "west"  -> pure West
    Just x       -> fail $ "unknown placement: " ++ x

parseGrid :: Object -> Parser Component
parseGrid o = do
  g  <- o .: "grid" >>= Util.parseIrregGrid
  s  <- o .: "style"
  gs <- case s of
    "default"           -> pure GridDefault
    "default-irregular" -> pure GridDefaultIrregular
    "dashed"            -> pure GridDashed
    "dots"              -> pure GridDots
    _                   -> fail $ "unknown grid style: " ++ s
  return $ Grid gs g

parseRegions :: Object -> Parser (Grid C Char)
parseRegions o = do
  g <- o .: "grid"
  Util.parseGrid g

parseNodeGrid :: Object -> Parser (Grid N Decoration)
parseNodeGrid o = do
  g <- o .: "grid"
  r <- parseReplacements o
  Util.parseGridWith (parseDecorationWithReplacements r) g

parseCellGrid :: Object -> Parser (Grid C Decoration)
parseCellGrid o = do
  g <- o .: "grid"
  r <- parseReplacements o
  Util.parseGridWith (parseDecorationWithReplacements r) g

parseEdgeGrid :: Object -> Parser (Map.Map (Edge N) Decoration)
parseEdgeGrid o = do
  g <- o .: "grid"
  r <- parseReplacements o
  Util.parseAnnotatedEdgesWith (parseDecorationWithReplacements r) g

parseFullGrid :: Object -> Parser Component
parseFullGrid o = do
  g <- o .: "grid"
  r <- parseReplacements o
  let pdec = parseDecorationWithReplacements r
  (ns, cs, es) <- Util.parseEdgeGridWith pdec pdec pdec g
  return $ FullGrid ns cs es

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
  'o'  -> DecKropkiDot KWhite
  '*'  -> DecKropkiDot KBlack
  '/'  -> DarkDiagonal $ PrimeDiag (True, False)
  '\\' -> DarkDiagonal $ PrimeDiag (False, True)
  '#'  -> Shade
  '-'  -> Edge Horiz
  '|'  -> Edge Vert
  _    -> Letter c

parseExtendedDecoration :: Util.IntString -> Parser Decoration
parseExtendedDecoration (Util.IntString s) = case s of
  "kropki-white"           -> pure $ DecKropkiDot KWhite
  "kropki-black"           -> pure $ DecKropkiDot KBlack
  "small-pearl-white"      -> pure $ SmallPearl MWhite
  "small-pearl-black"      -> pure $ SmallPearl MBlack
  "pearl-white"            -> pure $ Pearl MWhite
  "pearl-black"            -> pure $ Pearl MBlack
  "blank"                  -> pure Blank
  "afternoon-west"         -> pure $ AfternoonWest
  "afternoon-south"        -> pure $ AfternoonSouth
  "light-diagonal-forward" -> pure $ LightDiagonal $ PrimeDiag (True, False)
  "light-diagonal-back"    -> pure $ LightDiagonal $ PrimeDiag (False, True)
  "light-diagonal-both"    -> pure $ LightDiagonal $ PrimeDiag (True, True)
  "dark-diagonal-forward"  -> pure $ DarkDiagonal $ PrimeDiag (True, False)
  "dark-diagonal-back"     -> pure $ DarkDiagonal $ PrimeDiag (False, True)
  "dark-diagonal-both"     -> pure $ DarkDiagonal $ PrimeDiag (True, True)
  "edge-horiz"             -> pure $ Edge Horiz
  "edge-vert"              -> pure $ Edge Vert
  "thin-edge-horiz"        -> pure $ ThinEdge Horiz
  "thin-edge-vert"         -> pure $ ThinEdge Vert
  "sol-edge-horiz"         -> pure $ SolEdge Horiz
  "sol-edge-vert"          -> pure $ SolEdge Vert
  "dot"                    -> pure $ Dot
  "small-dot"              -> pure $ SmallDot
  "shade"                  -> pure $ Shade
  _                        -> pure $ Letters s

