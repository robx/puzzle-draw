{-# LANGUAGE FlexibleContexts #-}

module Main where

import Diagrams.Prelude hiding (value, option, (<>), Result)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.BoundingBox
import Diagrams.Backend.CmdLine

import Diagrams.TwoD.Puzzles.Draw

import Data.Puzzles.Grid
import qualified Data.Puzzles.ReadPuzzle as R
import Data.Puzzles.ReadPuzzle (
    TypedPuzzle, puzzleType, dropType, RawPuzzle, ReadPuzzle)
import qualified Diagrams.TwoD.Puzzles.Puzzle as D
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

rd :: ReadPuzzle a -> RenderPuzzle b a ->
      RawPuzzle -> Result (Diagram b R2, Diagram b R2)
rd r d x = d <$> r x

rdtype :: (Backend b R2, Renderable (Path R2) b) =>
          String -> RawPuzzle -> Result (Diagram b R2, Diagram b R2)
rdtype "lits"                 = rd R.lits                D.lits
rdtype "litsplus"             = rd R.litsplus            D.litsplus
rdtype "geradeweg"            = rd R.geradeweg           D.geradeweg
rdtype "fillomino"            = rd R.fillomino           D.fillomino
rdtype "masyu"                = rd R.masyu               D.masyu
rdtype "nurikabe"             = rd R.nurikabe            D.nurikabe
rdtype "latintapa"            = rd R.latintapa           D.latintapa
rdtype "sudoku"               = rd R.sudoku              D.sudoku
rdtype "thermosudoku"         = rd R.thermosudoku        D.thermosudoku
rdtype "pyramid"              = rd R.pyramid             D.pyramid
rdtype "rowkropkipyramid"     = rd R.kpyramid            D.kpyramid
rdtype "slitherlink"          = rd R.slither             D.slither
rdtype "slitherlinkliar"      = rd R.liarslither         D.liarslither
rdtype "skyscrapers-tightfit" = rd R.tightfitskyscrapers D.tightfitskyscrapers
rdtype "wordloop"             = rd R.wordloop            D.wordloop
rdtype "wordsearch"           = rd R.wordsearch          D.wordsearch
rdtype "curvedata"            = rd R.curvedata           D.curvedata
rdtype "doubleback"           = rd R.doubleback          D.doubleback
rdtype "slalom"               = rd R.slalom              D.slalom
rdtype "compass"              = rd R.compass             D.compass
rdtype t                      = const . Error $ "unknown puzzle type: " ++ t

drawPuzzle :: TypedPuzzle -> Result (Diagram B R2, Diagram B R2)
drawPuzzle p = rdtype (puzzleType p) (dropType p)

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
