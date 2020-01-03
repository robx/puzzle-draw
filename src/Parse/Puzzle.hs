{-# LANGUAGE OverloadedStrings #-}

module Parse.Puzzle where

import Control.Applicative
import Data.Yaml

data TypedPuzzle
  = TP
      { _tpType :: Maybe String,
        _tpRenderAs :: Maybe String,
        _tpPuzzle :: Value,
        _tpSolution :: Maybe Value,
        _tpCode :: Maybe Value
      }
  deriving (Show)

instance FromJSON TypedPuzzle where
  parseJSON (Object v) =
    TP
      <$> v .:? "type"
      <*> v .:? "render-as"
      <*> v .: "puzzle"
      <*> v .:? "solution"
      <*> v .:? "code"
  parseJSON _ = empty

-- | A pair of parsers for a puzzle type.
-- First parses the puzzle, second the solution.
type ParsePuzzle a b = (Value -> Parser a, Value -> Parser b)
