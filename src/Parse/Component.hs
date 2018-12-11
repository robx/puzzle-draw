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
  t   <- o .: "type" :: Parser String
  tag <- parseTag o
  c   <- case t of
    "grid"    -> parseGrid o
    "regions" -> Regions <$> parseRegions o
    "nodes"   -> NodeGrid <$> parseNodeGrid o
    "cells"   -> CellGrid <$> parseCellGrid o
    "edges"   -> EdgeGrid <$> parseEdgeGrid o
    _         -> fail $ "unknown component type: " ++ t
  pure $ TaggedComponent tag c

parseTag :: Object -> Parser (Maybe Tag)
parseTag o = do
  t <- o .:? "tag" :: Parser (Maybe String)
  case t of
    Nothing         -> pure Nothing
    Just "puzzle"   -> pure (Just Puzzle)
    Just "solution" -> pure (Just Solution)
    Just x          -> fail $ "unknown tag: " ++ x

parseGrid :: Object -> Parser Component
parseGrid o = do
  g  <- o .: "grid" >>= Util.parseIrregGrid
  s  <- o .: "style"
  gs <- case s of
    "default"           -> pure GridDefault
    "default-irregular" -> pure GridDefaultIrregular
    "dashed"            -> pure GridDashed
    _                   -> fail $ "unknown grid style: " ++ s
  return $ Grid gs g

parseRegions :: Object -> Parser (Grid C Char)
parseRegions o = do
  g <- o .: "grid"
  Util.parseGrid g

parseNodeGrid :: Object -> Parser (Grid N Decoration)
parseNodeGrid o = do
  g <- o .: "grid"
  Util.parseGridWith parseDecoration g

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
  '/'  -> Diagonal $ PrimeDiag (True, False)
  '\\' -> Diagonal $ PrimeDiag (False, True)
  '#'  -> Shade
  _    -> Letter c

parseExtendedDecoration :: Util.IntString -> Parser Decoration
parseExtendedDecoration (Util.IntString s) = case s of
  "kropki-white"     -> pure $ DecKropkiDot KWhite
  "kropki-black"     -> pure $ DecKropkiDot KBlack
  "blank"            -> pure Blank
  "afternoon-west"   -> pure $ AfternoonWest
  "afternoon-south"  -> pure $ AfternoonSouth
  "diagonal-forward" -> pure $ Diagonal $ PrimeDiag (True, False)
  "diagonal-back"    -> pure $ Diagonal $ PrimeDiag (False, True)
  "diagonal-both"    -> pure $ Diagonal $ PrimeDiag (True, True)
  "dot"              -> pure $ Dot
  "shade"            -> pure $ Shade
  _                  -> pure $ Letters s

