module Data.Puzzles.CmdLine
    ( checkType
    , exitErr
    , readPuzzle
    ) where

import Text.Puzzles.Puzzle
import Data.Puzzles.PuzzleTypes

import qualified Data.Yaml as Y
import System.Exit

checkType :: Maybe String -> IO PuzzleType
checkType mt = do
    t <- maybe errno return mt
    maybe (errunk t) return (lookupType t)
  where
    errno    = exitErr "no puzzle type given"
    errunk t = exitErr $ "unknown puzzle type: " ++ t

readPuzzle :: FilePath -> IO (Either Y.ParseException TypedPuzzle)
readPuzzle = Y.decodeFileEither

exitErr :: String -> IO a
exitErr e = putStrLn e >> exitFailure


