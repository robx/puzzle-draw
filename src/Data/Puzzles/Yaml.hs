{-# LANGUAGE OverloadedStrings #-}

module Data.Puzzles.Yaml where

import Control.Applicative
import Control.Monad
import Data.Yaml

data Component  = C { componentType :: String
                    , grid          :: String
                    }

data Puzzle = P { puzzleType :: String
                , puzzle     :: [Component]
                , solution   :: Maybe [Component]
                }

instance FromJSON Component where
    parseJSON (Object v) = C           <$>
                           v .: "type" <*>
                           v .: "grid"
    parseJSON _          = mzero

instance FromJSON Puzzle where
    parseJSON (Object v) = P                 <$>
                           v .:  "type"      <*>
                           v .:  "puzzle"    <*>
                           v .:? "solution"
    parseJSON _          = mzero
