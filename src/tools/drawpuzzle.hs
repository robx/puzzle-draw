module Main where

import System.Exit
import Diagrams.Backend.CmdLine

import Diagrams.TwoD.Puzzles.Draw
import Diagrams.TwoD.Puzzles.Puzzle
import Diagrams.BoundingBox

import Data.Puzzles.Grid
import Data.Puzzles.ReadPuzzle
import Options.Applicative

import Control.Monad

import Diagrams.Prelude hiding (value, option, (<>), Result)
import Diagrams.Backend.Cairo.CmdLine

import System.FilePath
import System.Environment (getProgName)

import qualified Data.Yaml as Y
import Data.Aeson (Result(..))

import Data.Puzzles.Grid
import Diagrams.TwoD.Puzzles.Draw

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
    <*> (argument str)
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
          out = addExtension (base ++ (outputSuffix oc)) f

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

drawPuzzle :: Puzzle -> Result (OutputChoice -> Diagram B R2)
drawPuzzle p = case puzzleType p of
    "lits" ->      f p parseLITS lits
    "litsplus" ->  f p parseLITSPlus lits
    "geradeweg" -> f p parseGeradeweg geradeweg
    "fillomino" -> f p parseFillomino fillomino
    "masyu" ->     f p parseMasyu masyu
    "nurikabe" ->  f p parseNurikabe nurikabe
    "latintapa" -> f p parseLatinTapa latintapa
    "sudoku" ->    f p parseSudoku sudoku
    "thermosudoku" -> f p parseThermoSudoku thermosudoku
    "pyramid" ->   f p parsePyramid pyramid
    "rowkropkipyramid" -> f p parseKropkiPyramid kpyramid
    "slitherlink" -> f p parseSlitherLink slither
    "slitherlinkliar" -> f p parseLiarSlitherLink liarslither
    "skyscrapers-tightfit" -> f p parseTightfitSkyscraper tightfitskyscrapers
    "wordloop" -> f p parseWordloop wordloop
    "wordsearch" -> f p parseWordsearch wordsearch
    "curvedata" -> f p parseCurveData curvedata
    "doubleback" -> f p parseDoubleBack doubleback
    "slalom" -> f p parseSlalom slalom
    "compass" -> f p parseCompass compass
    t -> Error $ "unknown puzzle type: " ++ t
    where f q parse rp = draw rp <$> parse q

readPuzzle :: FilePath -> IO (Maybe Puzzle)
readPuzzle = Y.decodeFile

main = do
    opts <- defaultOpts puzzleOpts
    mp <- readPuzzle (_input opts)
    p <- case mp of Nothing -> putStrLn "failed to parse yaml" >> exitFailure >> return undefined
                    Just p  -> return p
    let ps = drawPuzzle p
        ocs = if _example opts
              then [DrawExample]
              else [DrawPuzzle, DrawSolution]
    case ps of Success ps' -> sequence_ . map (renderPuzzle opts ps') $ ocs
               Error e -> putStrLn e >> exitFailure
