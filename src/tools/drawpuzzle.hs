{-# LANGUAGE FlexibleContexts #-}

module Main where

import Diagrams.Prelude hiding (value, option, (<>), Result, render)

import Data.CmdLine
import Draw.CmdLine

import Parse.Puzzle
import Parse.Code
import Data.Compose
import Data.PuzzleTypes (typeOptions)
import Draw.Draw
import Draw.Code
import Draw.Lib

import Options.Applicative
import Control.Monad
import Data.Maybe
import Data.List (intercalate)

import System.FilePath
import System.Environment (getProgName)

import qualified Data.Yaml as Y

optListTypes :: Parser (a -> a)
optListTypes =
    infoOption
        (unlines' typeOptions)
        (long "list-types"
         <> help "List supported puzzle types")
  where
    unlines' = intercalate "\n"

data PuzzleOpts = PuzzleOpts
    { _format   :: Format
    , _type     :: Maybe String
    , _dir      :: FilePath
    , _puzzle   :: Bool
    , _solution :: Bool
    , _example  :: Bool
    , _code     :: Bool
    , _scale    :: Double
    , _input    :: [FilePath]
    }

config :: PuzzleOpts -> IO Config
config opts =
  do
    var <- fontGenLight
    bit <- fontBit
    let device = case _format opts of
                     PNG -> Screen
                     _   -> Print
    return $ Config device var bit

puzzleOpts :: Parser PuzzleOpts
puzzleOpts = PuzzleOpts
    <$> option parseFormat
            (long "format" <> short 'f'
             <> value (head formats)
             <> metavar "FMT"
             <> help ("Desired output format by file extension " ++ fmts))
    <*> (optional . strOption $
            (long "type" <> short 't'
             <> metavar "TYPE"
             <> help "Puzzle type, overriding type in input file"))
    <*> strOption
            (long "directory" <> short 'd'
             <> value "."
             <> metavar "DIR"
             <> help "Output directory")
    <*> switch
            (long "puzzle" <> short 'p'
             <> help "Render puzzle (to base.ext)")
    <*> switch
            (long "solution" <> short 's'
             <> help "Render solution (to base-sol.ext)")
    <*> switch
            (long "example" <> short 'e'
             <> help "Render example (to base.ext)")
    <*> switch
            (long "code" <> short 'c'
             <> help "Add solution code markers")
    <*> option auto
            (long "scale"
             <> value 1.0
             <> metavar "FACTOR"
             <> help "Scale the size by this factor")
    <*> some (argument str
            (metavar "INPUT..."
             <> help "Puzzle files in .pzl format"))
  where
    parseFormat = eitherReader
        (\s -> case lookupFormat s of
                Just f -> Right f
                Nothing -> Left "unknown format")
    fmts = "(" ++ intercalate ", " (map extension formats) ++ ")"

cmtopoint :: Double -> Double
cmtopoint = (* 28.3464567)

outputSuffix :: OutputChoice -> String
outputSuffix DrawPuzzle = ""
outputSuffix DrawSolution = "-sol"
outputSuffix DrawExample = ""

toRenderOpts :: FilePath -> OutputChoice -> (Double, Double) -> PuzzleOpts -> RenderOpts
toRenderOpts input oc (w, h) opts = RenderOpts out sz
  where
    f = _format opts
    u = case f of PNG -> Pixels
                  _    -> Points
    w' = toOutputWidth u w * (_scale opts)
    h' = toOutputWidth u h * (_scale opts)
    sz = mkSizeSpec2D (Just w') (Just h')
    base = takeBaseName input
    out = _dir opts </> (base ++ outputSuffix oc) <.> extension f

renderPuzzle :: PuzzleOpts -> FilePath -> (OutputChoice -> Maybe (Diagram B)) ->
                (OutputChoice, Bool) -> IO ()
renderPuzzle opts input r (oc, req) = do
    let x = r oc
    when (req && isNothing x) $
        exitErr ("failed to render (no solution?): " ++ show oc)
    when (isJust x) $ do
        let Just x' = x
            ropts = toRenderOpts input oc (diagramSize x') opts
        renderToFile ropts x'

defaultOpts :: Parser a -> IO a
defaultOpts optsParser = do
    prog <- getProgName
    let p = info (helper <*> optListTypes <*> optsParser)
                (fullDesc
                 <> progDesc "Command-line diagram generation."
                 <> header prog)
    execParser p

type OutputChoices = [(OutputChoice, Bool)]

checkOutput :: PuzzleOpts -> IO OutputChoices
checkOutput opts
    | (p || s) && e  = exitErr "example output conflicts with puzzle/solution"
    | e              = return . map req $ [DrawExample]
    | p && s         = return . map req $ [DrawPuzzle, DrawSolution]
    | p              = return . map req $ [DrawPuzzle]
    | s              = return . map req $ [DrawSolution]
    | otherwise      = return [req DrawPuzzle, opt DrawSolution]
  where
    p = _puzzle opts
    s = _solution opts
    e = _example opts
    req x = (x, True)
    opt x = (x, False)

maybeSkipSolution :: OutputChoices -> Maybe Y.Value -> Maybe Y.Value
maybeSkipSolution _ Nothing    = Nothing
maybeSkipSolution ocs (Just v) =
    if any hasSol . map fst $ ocs
        then Just v
        else Nothing
  where
    hasSol DrawSolution = True
    hasSol DrawExample  = True
    hasSol DrawPuzzle   = False

maybeSkipCode :: PuzzleOpts -> Maybe Y.Value -> Maybe Y.Value
maybeSkipCode opts = if _code opts then id else const Nothing

parseAndDrawCode :: Y.Value -> IO (CodeDiagrams (Drawing B))
parseAndDrawCode v = case parsed of
    Left  e -> exitErr $ "solution code parse failure: " ++ e
    Right c -> return $ drawCode c
  where
    parsed = Y.parseEither parseCode v

handleOne :: PuzzleOpts -> OutputChoices -> FilePath -> IO ()
handleOne opts ocs input = do
    mp <- readPuzzle input
    TP mt mrt pv msv mc <- case mp of Left  e -> exitErr $
                                             "parse failure: " ++ show e
                                      Right p -> return p
    let msv' = maybeSkipSolution ocs msv
        mc'  = maybeSkipCode opts mc
    t <- checkType $ _type opts `mplus` mrt `mplus` mt
    let ps = Y.parseEither (handle drawPuzzleMaybeSol t) (pv, msv')
    mcode <- sequenceA $ parseAndDrawCode <$> mc'
    cfg <- config opts
    case ps of Right ps' -> mapM_ (renderPuzzle opts input (render cfg mcode ps')) ocs
               Left    e -> exitErr $ "parse failure: " ++ e

main :: IO ()
main = do
    opts <- defaultOpts puzzleOpts
    ocs <- checkOutput opts
    mapM_ (handleOne opts ocs) (_input opts)
