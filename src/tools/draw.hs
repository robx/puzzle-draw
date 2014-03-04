module Main where

import Diagrams.Backend.CmdLine

import Diagrams.TwoD.Puzzles.Draw
import Diagrams.TwoD.Puzzles.Puzzle
import Diagrams.BoundingBox

import Data.Puzzles.Grid
import Data.Puzzles.Puzzle
import Options.Applicative

import Control.Monad

import Diagrams.Prelude hiding (value, option, (<>))
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
                         _     -> round . cmtopoint $ w
          base = takeBaseName i
          out = addExtension (base ++ (outputSuffix oc)) f

renderPuzzle :: PuzzleOpts -> PuzzleSol B -> OutputChoice -> IO ()
renderPuzzle opts (p, s) oc = do
    let x = draw (p, s) oc
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

drawPuzzle :: Puzzle -> (Diagram B R2, Diagram B R2)
drawPuzzle p = case puzzleType p of
    "lits" ->      f p parseLITS drawLITS drawLITSsol
    "litsplus" ->  f p parseLITSPlus drawLITS drawLITSsol
    "geradeweg" -> f p parseGeradeweg drawGeradeweg drawGeradewegsol
    "fillomino" -> f p parseFillomino drawFillomino drawFillominosol
    "masyu" ->     f p parseMasyu drawMasyu drawMasyusol
    "nurikabe" ->  f p parseNurikabe drawNurikabe drawNurikabesol
    "latintapa" -> f p parseLatinTapa drawLatinTapa drawLatinTapasol
    "sudoku" ->    f p parseSudoku drawSudoku drawSudokusol
    "thermosudoku" -> f p parseThermoSudoku drawThermoSudoku drawThermoSudokusol
    "pyramid" ->   f p parsePyramid drawPyramid drawPyramidsol
    "rowkropkipyramid" -> f p parseKropkiPyramid drawKropkiPyramid drawKropkiPyramidsol
    "slitherlink" -> f p parseSlitherLink drawSlither drawSlithersol
    "slitherlinkliar" -> f p parseLiarSlitherLink drawLiarSlither drawLiarSlithersol
    "skyscrapers-tightfit" -> f p parseTightfitSkyscraper drawTightfitSkyscraper drawTightfitSkyscrapersol
    "wordloop" -> f p parseWordloop drawWordloop drawWordloopsol
    "wordsearch" -> f p parseWordsearch drawWordsearch drawWordsearchsol
    "curvedata" -> f p parseCurveData drawCurveData drawCurveDatasol
    "doubleback" -> f p parseDoubleBack drawDoubleBack drawDoubleBacksol
    "slalom" -> f p parseSlalom drawSlalom drawSlalomsol
    where f q parse draw drawsol = let Success x = parse q in (draw x, drawsol x)

readPuzzle :: FilePath -> IO Puzzle
readPuzzle fp = do
    Just p <- Y.decodeFile fp
    return p

main = do
    opts <- defaultOpts puzzleOpts
    p <- readPuzzle (_input opts)
    let ps = drawPuzzle p
        ocs = if _example opts
              then [DrawExample]
              else [DrawPuzzle, DrawSolution]
    mapM (renderPuzzle opts ps) ocs
