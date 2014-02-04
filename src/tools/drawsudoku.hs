{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Diagrams.TwoD.Puzzles.Draw
import Data.Puzzles.Grid

import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

gridFromStrings :: [String] -> Grid IntClue
gridFromStrings = fmap charToIntClue . fromListList 

readGrid :: IO (Grid IntClue)
readGrid = liftM (gridFromStrings . filter (not . null) . lines) getContents

d g = (drawClues drawInt (clues g) `atop` sudokugrid) # frame

main = liftM d readGrid >>= defaultMain
