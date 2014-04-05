{-# LANGUAGE OverloadedStrings #-}

module Puzzles.Parse.Puzzle where

import Data.Yaml
import Control.Applicative

import Puzzles.PuzzleTypes

data TypedPuzzle = TP (Maybe String) Value (Maybe Value)
    deriving Show

instance FromJSON TypedPuzzle where
    parseJSON (Object v) = TP <$>
                           v .:? "type" <*>
                           v .:  "puzzle" <*>
                           v .:? "solution"
    parseJSON _          = empty

-- | A pair of parsers for a puzzle type.
-- First parses the puzzle, second the solution.
type ParsePuzzle a b = (Value -> Parser a, Value -> Parser b)

parseType :: String -> Parser PuzzleType
parseType t = case lookupType t of
                  Nothing -> fail $ "unknown puzzle type: " ++ t
                  Just p  -> return p
