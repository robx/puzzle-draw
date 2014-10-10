module Main where

import Diagrams.Puzzles.DSL

missingLabyrinth :: Puzzle
missingLabyrinth = do
    readData
    grid <- parseCharGrid ["puzzle", "grid"]
    clues <- parseOutsideClues ["puzzle", "clues"]
    return $ drawGrid dashed grid <> drawOutsideClues' clues

main :: IO ()
main = mainWith missingLabyrinth
