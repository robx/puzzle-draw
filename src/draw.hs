module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.Yaml

import Data.Puzzles.Grid
import Data.Puzzles.Yaml
import Diagrams.TwoD.Puzzles.Draw

readGrid :: Component -> CharGrid
readGrid (C _ g) = fromListList . lines $ g

renderGrid :: CharGrid -> Diagram B R2
renderGrid = frame . drawClueGrid . fmap charToCharClue

renderPuzzle :: FilePath -> IO (Diagram B R2)
renderPuzzle fp = do
    Just (P t ps ss) <- decodeFile fp
    return . mconcat . map (renderGrid . readGrid) $ ps

main = mainWith renderPuzzle

