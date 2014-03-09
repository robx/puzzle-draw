module Main where

import Diagrams.Prelude hiding (value, option, (<>), Result)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.BoundingBox
import Diagrams.Backend.CmdLine

import Diagrams.TwoD.Puzzles.Draw

import Data.Puzzles.Grid
import qualified Data.Puzzles.ReadPuzzle as R
import Data.Puzzles.ReadPuzzle (TypedPuzzle, puzzleType, dropType)
import qualified Diagrams.TwoD.Puzzles.Puzzle as D
import Diagrams.TwoD.Puzzles.Puzzle (OutputChoice(..), draw)
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

drawPuzzle :: TypedPuzzle -> Result (OutputChoice -> Diagram B R2)
drawPuzzle p = case puzzleType p of
    "lits" ->      f p R.lits D.lits
    "litsplus" ->  f p R.litsplus D.litsplus
    "geradeweg" -> f p R.geradeweg D.geradeweg
    "fillomino" -> f p R.fillomino D.fillomino
    "masyu" ->     f p R.masyu D.masyu
    "nurikabe" ->  f p R.nurikabe D.nurikabe
    "latintapa" -> f p R.latintapa D.latintapa
    "sudoku" ->    f p R.sudoku D.sudoku
    "thermosudoku" -> f p R.thermosudoku D.thermosudoku
    "pyramid" ->   f p R.pyramid D.pyramid
    "rowkropkipyramid" -> f p R.kpyramid D.kpyramid
    "slitherlink" -> f p R.slither D.slither
    "slitherlinkliar" -> f p R.liarslither D.liarslither
    "skyscrapers-tightfit" -> f p R.tightfitskyscrapers D.tightfitskyscrapers
    "wordloop" -> f p R.wordloop D.wordloop
    "wordsearch" -> f p R.wordsearch D.wordsearch
    "curvedata" -> f p R.curvedata D.curvedata
    "doubleback" -> f p R.doubleback D.doubleback
    "slalom" -> f p R.slalom D.slalom
    "compass" -> f p R.compass D.compass
    t -> Error $ "unknown puzzle type: " ++ t
    where f q parse rp = draw rp <$> parse (dropType q)

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
    case ps of Success ps' -> mapM_ (renderPuzzle opts ps') ocs
               Error e -> putStrLn e >> exitFailure
