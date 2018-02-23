{-# LANGUAGE FlexibleContexts #-}

module Main where

import Diagrams.Prelude hiding (value, option, (<>), Result, render)

import Data.CmdLine
import Draw.CmdLine

import Parse.Puzzle
import Parse.Code
import Data.Compose
import Data.PuzzleTypes (typeNames)
import Draw.Draw
import Draw.Code

import Options.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.List (intercalate, sort)

import System.FilePath
import System.Environment (getProgName)

import qualified Data.Yaml as Y

optListTypes :: Parser (a -> a)
optListTypes =
    infoOption
        (unlines' . sort . map snd $ typeNames)
        (long "list-types"
         <> help "List supported puzzle types")
  where
    unlines' = intercalate "\n"

data PuzzleOpts = PuzzleOpts
    { _format   :: String
    , _type     :: Maybe String
    , _puzzle   :: Bool
    , _solution :: Bool
    , _example  :: Bool
    , _code     :: Bool
    , _input    :: FilePath
    }

puzzleOpts :: Parser PuzzleOpts
puzzleOpts = PuzzleOpts
    <$> strOption
            (long "format" <> short 'f'
             <> value (head formats)
             <> metavar "FMT"
             <> help ("Desired output format by file extension " ++ fmts))
    <*> (optional . strOption $
            (long "type" <> short 't'
             <> metavar "TYPE"
             <> help "Puzzle type, overriding type in input file"))
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
    <*> argument str
            (metavar "INPUT"
             <> help "Puzzle file in .pzl format")
  where
    fmts = "(" ++ intercalate ", " formats ++ ")"

cmtopoint :: Double -> Double
cmtopoint = (* 28.3464567)

outputSuffix :: OutputChoice -> String
outputSuffix DrawPuzzle = ""
outputSuffix DrawSolution = "-sol"
outputSuffix DrawExample = ""

toRenderOpts :: OutputChoice -> Double -> PuzzleOpts -> RenderOpts
toRenderOpts oc w opts = RenderOpts out w'
  where
    f = _format opts
    u = case f of "png" -> Pixels
                  _     -> Points
    w' = toOutputWidth u w
    base = takeBaseName (_input opts)
    out = addExtension (base ++ outputSuffix oc) f

renderPuzzle :: PuzzleOpts -> (OutputChoice -> Maybe (Diagram B)) ->
                (OutputChoice, Bool) -> IO ()
renderPuzzle opts r (oc, req) = do
    let x = r oc
    when (req && isNothing x) $
        exitErr ("failed to render (no solution?): " ++ show oc)
    when (isJust x) $ do
        let Just x' = x
            ropts = toRenderOpts oc (diagramWidth x') opts
        renderToFile ropts x'

defaultOpts :: Parser a -> IO a
defaultOpts optsParser = do
    prog <- getProgName
    let p = info (helper <*> optListTypes <*> optsParser)
                (fullDesc
                 <> progDesc "Command-line diagram generation."
                 <> header prog)
    execParser p

checkOutput :: PuzzleOpts -> IO [(OutputChoice, Bool)]
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

maybeSkipSolution :: [(OutputChoice, Bool)] -> Maybe Y.Value -> Maybe Y.Value
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

parseAndDrawCode :: Y.Value -> IO (CodeDiagrams (Diagram B))
parseAndDrawCode v = case parsed of
    Left  e -> exitErr $ "solution code parse failure: " ++ e
    Right c -> return $ drawCode c
  where
    parsed = Y.parseEither parseCode v

main :: IO ()
main = do
    opts <- defaultOpts puzzleOpts
    ocs <- checkOutput opts
    checkFormat (_format opts)
    mp <- readPuzzle (_input opts)
    TP mt pv msv mc <- case mp of Left  e -> exitErr $
                                             "parse failure: " ++ show e
                                  Right p -> return p
    let msv' = maybeSkipSolution ocs msv
        mc'  = maybeSkipCode opts mc
    t <- checkType $ _type opts `mplus` mt
    let ps = Y.parseEither (handle drawPuzzleMaybeSol t) (pv, msv')
    mcode <- sequenceA $ parseAndDrawCode <$> mc'
    case ps of Right ps' -> mapM_ (renderPuzzle opts (render mcode ps')) ocs
               Left    e -> exitErr $ "parse failure: " ++ e
