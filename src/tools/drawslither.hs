{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import Diagrams.TwoD.Puzzles.Draw
import Data.Puzzles.Grid

import Control.Monad

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

gridFromStrings :: [String] -> Grid IntClue
gridFromStrings = fmap charToIntClue . fromListList 

readGrid :: IO (Grid IntClue)
readGrid = liftM (gridFromStrings . filter (not . null) . lines) getContents

main = liftM (frame . drawSlitherGrid) readGrid >>= defaultMain
