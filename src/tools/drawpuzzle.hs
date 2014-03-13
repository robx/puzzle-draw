{-# LANGUAGE FlexibleContexts #-}

module Main where

import Diagrams.Prelude hiding (value, option, (<>), Result)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.BoundingBox
import Diagrams.Backend.CmdLine

import Diagrams.TwoD.Puzzles.PuzzleDraw

import Data.Puzzles.Grid
import Data.Puzzles.ReadPuzzle
import Diagrams.TwoD.Puzzles.Puzzle (OutputChoice(..), RenderPuzzle, draw)
import Options.Applicative
import Control.Monad

import System.FilePath
import System.Environment (getProgName)
import System.Exit

import qualified Data.Yaml as Y
import Data.Aeson (Result(..))


data PuzzleOpts = PuzzleOpts
    { _format  :: String
    , _example :: Bool
    , _input   :: FilePath
    }

puzzleOpts :: Parser PuzzleOpts
puzzleOpts = PuzzleOpts
    <$> strOption
            (long "format" <> short 'f'
             <> value "png"
             <> metavar "FMT"
             <> help "Desired output format by file extension")
    <*> switch
            (long "example" <> short 'e'
             <> help "Example formatting (puzzle next to solution)")
    <*> argument str
            (metavar "INPUT"
             <> help "Puzzle file in .pzl format")

instance Parseable PuzzleOpts where
    parser = puzzleOpts

cmtopoint :: Double -> Double
cmtopoint = (* 28.3464567)

outputSuffix DrawPuzzle = ""
outputSuffix DrawSolution = "-sol"
outputSuffix DrawExample = ""

toDiagramOpts :: OutputChoice -> Double -> PuzzleOpts -> DiagramOpts
toDiagramOpts oc w (PuzzleOpts f e i) =
    DiagramOpts (Just w') Nothing out
    where w' = case f of "png" -> round (40 * w)
                         _     -> round . cmtopoint $ (0.8 * w)
          base = takeBaseName i
          out = addExtension (base ++ outputSuffix oc) f

renderPuzzle :: PuzzleOpts -> (OutputChoice -> Diagram B R2) ->
                OutputChoice -> IO ()
renderPuzzle opts r oc = do
    let x = r oc
        w = fst . unr2 . boxExtents . boundingBox $ x
        dopts = toDiagramOpts oc w opts
        lopts = DiagramLoopOpts False Nothing 0
    mainRender (dopts, lopts) x

defaultOpts :: Parser a -> IO a
defaultOpts optsParser = do
    prog <- getProgName
    let p = info (helper <*> optsParser)
                (fullDesc
                 <> progDesc "Command-line diagram generation."
                 <> header prog)
    execParser p

readPuzzle :: FilePath -> IO (Maybe TypedPuzzle)
readPuzzle = Y.decodeFile

main = do
    opts <- defaultOpts puzzleOpts
    mp <- readPuzzle (_input opts)
    p <- case mp of Nothing -> putStrLn "failed to parse yaml"
                               >> exitFailure
                               >> return undefined
                    Just p  -> return p
    let ps = drawPuzzle p
        ocs = if _example opts
              then [DrawExample]
              else [DrawPuzzle, DrawSolution]
    case ps of Success ps' -> mapM_ (renderPuzzle opts (draw ps')) ocs
               Error e -> putStrLn e >> exitFailure
