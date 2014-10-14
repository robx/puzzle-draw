module Main where

import Diagrams.Puzzles.DSL

summon :: Puzzle
summon = do
    readData
    areas <- parseAreaGrid ["puzzle", "grid"]
    clues <- parseOutsideClues ["puzzle", "clues"]
    return $ drawAreaGrid areas <> drawOutsideClues clues

main :: IO ()
main = mainWith summon
