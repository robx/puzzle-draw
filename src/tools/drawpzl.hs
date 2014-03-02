module Main where

import Diagrams.Prelude hiding (value, option, (<>))
import Diagrams.Backend.CmdLine
import Diagrams.Backend.Cairo.CmdLine

import System.FilePath
import System.Environment (getProgName)

import Options.Applicative

import qualified Data.Yaml as Y

import Data.Puzzles.Grid
import qualified Data.Puzzles.Yaml as YP
import Diagrams.TwoD.Puzzles.Draw

readGrid :: YP.Component -> CharGrid
readGrid (YP.C _ g) = fromListList . lines $ g

drawGrid' :: CharGrid -> Diagram B R2
drawGrid' = frame . drawClueGrid . fmap charToCharClue

type Puzzle = [YP.Component]

readPuzzle :: FilePath -> IO Puzzle
readPuzzle fp = do
    Just (YP.P t ps ss) <- Y.decodeFile fp
    return ps

drawPuzzle :: Puzzle -> Diagram B R2
drawPuzzle = mconcat . map (drawGrid' . readGrid)

data PuzzleOpts = PuzzleOpts
    { _border  :: Maybe Double
    , _scale   :: Maybe Double
    , _format  :: String
    , _example :: Bool
    , _input   :: FilePath
    }

toDiagramOpts :: PuzzleOpts -> DiagramOpts
toDiagramOpts (PuzzleOpts b s f e i) =
    DiagramOpts (Just 200) Nothing (addExtension (takeBaseName i) f)

renderPuzzle :: PuzzleOpts -> IO ()
renderPuzzle opts = do
    p <- readPuzzle (_input opts)
    let dopts = toDiagramOpts opts
        lopts = DiagramLoopOpts False Nothing 0
    mainRender (dopts, lopts) (drawPuzzle p)

defaultOpts :: Parser a -> IO a
defaultOpts optsParser = do
    prog <- getProgName
    let p = info (helper <*> optsParser)
                (fullDesc
                 <> progDesc "Command-line diagram generation."
                 <> header prog)
    execParser p

main = do
    opts <- defaultOpts puzzleOpts
    renderPuzzle opts

puzzleOpts :: Parser PuzzleOpts
puzzleOpts = PuzzleOpts
    <$> (optional . option)
            (long "border" <> short 'b'
             <> metavar "WIDTH"
             <> help "Desired width of outer border")
    <*> (optional . option)
            (long "scale" <> short 's'
             <> metavar "FACTOR"
             <> help "Desired scaling factor relative to default size")
    <*> option
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

--newtype M = M m

--instance Mainable d => Mainable (M d) where
--    type MainOpts _ = PuzzleOpts
--
--    mainRender opts (M d) = mainRender 
