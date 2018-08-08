module Data.CmdLine
    ( checkTypeExit
    , exitErr
    , readPuzzle
    ) where

import Parse.Puzzle
import Data.PuzzleTypes

import qualified Data.Yaml as Y
import System.Exit

checkTypeExit :: Maybe String -> IO PuzzleType
checkTypeExit mt = case checkType mt of
    Left err -> exitErr err
    Right t -> return t

readPuzzle :: FilePath -> IO (Either Y.ParseException TypedPuzzle)
readPuzzle = Y.decodeFileEither

exitErr :: String -> IO a
exitErr e = putStrLn e >> exitFailure
