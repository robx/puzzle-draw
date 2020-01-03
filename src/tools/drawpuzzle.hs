{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Lib
import Data.List (intercalate)
import Data.PuzzleTypes
import qualified Data.Yaml as Y
import Draw.CmdLine
import Draw.Draw
import Draw.Font
import Draw.Render
import Options.Applicative
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.FilePath

optListTypes :: Parser (a -> a)
optListTypes =
  infoOption
    (unlines' typeOptions)
    (long "list-types" <> help "List supported puzzle types")
  where
    unlines' = intercalate "\n"

data PuzzleOpts
  = PuzzleOpts
      { _format :: Format,
        _type :: Maybe String,
        _dir :: FilePath,
        _puzzle :: Bool,
        _solution :: Bool,
        _example :: Bool,
        _code :: Bool,
        _scale :: Double,
        _input :: [FilePath]
      }

config :: PuzzleOpts -> Config
config opts =
  let var = fontAnelizaRegular
      bit = fontBit
      device = case _format opts of
        PDF -> Print
        _ -> Screen
   in Config device var bit

puzzleOpts :: Parser PuzzleOpts
puzzleOpts =
  PuzzleOpts
    <$> option
      parseFormat
      ( long "format"
          <> short 'f'
          <> value (head formats)
          <> metavar "FMT"
          <> help ("Desired output format by file extension " ++ fmts)
      )
    <*> ( optional
            . strOption
            $ ( long "type" <> short 't' <> metavar "TYPE"
                  <> help
                    "Puzzle type, overriding type in input file"
              )
        )
    <*> strOption
      ( long "directory" <> short 'd' <> value "." <> metavar "DIR"
          <> help
            "Output directory"
      )
    <*> switch
      (long "puzzle" <> short 'p' <> help "Render puzzle (to base.ext)")
    <*> switch
      ( long "solution" <> short 's'
          <> help
            "Render solution (to base-sol.ext)"
      )
    <*> switch
      (long "example" <> short 'e' <> help "Render example (to base.ext)")
    <*> switch (long "code" <> short 'c' <> help "Add solution code markers")
    <*> option
      auto
      ( long "scale" <> value 1.0 <> metavar "FACTOR"
          <> help
            "Scale the size by this factor"
      )
    <*> some
      ( argument
          str
          (metavar "INPUT..." <> help "Puzzle files in .pzl or .pzg format")
      )
  where
    parseFormat =
      eitherReader
        ( \s -> case lookupFormat s of
            Just f -> Right f
            Nothing -> Left "unknown format"
        )
    fmts = "(" ++ intercalate ", " (map extension formats) ++ ")"

outputSuffix :: OutputChoice -> String
outputSuffix DrawPuzzle = ""
outputSuffix DrawSolution = "-sol"
outputSuffix DrawExample = ""

outputPath :: PuzzleOpts -> FilePath -> Format -> OutputChoice -> FilePath
outputPath opts input fmt oc = out
  where
    base = takeBaseName input
    out = _dir opts </> (base ++ outputSuffix oc) <.> extension fmt

defaultOpts :: Parser a -> IO a
defaultOpts optsParser = do
  prog <- getProgName
  let p =
        info
          (helper <*> optListTypes <*> optsParser)
          (fullDesc <> progDesc "Command-line diagram generation." <> header prog)
  execParser p

checkOutput :: PuzzleOpts -> Either String OutputChoice
checkOutput opts = case choices of
  [oc] -> Right oc
  [] -> Right DrawPuzzle
  _ -> Left "more than one output flag given"
  where
    p = _puzzle opts
    s = _solution opts
    e = _example opts
    choices =
      map snd
        . filter fst
        $ [(p, DrawPuzzle), (s, DrawSolution), (e, DrawExample)]

checkPuzzleFormat :: FilePath -> Either String PuzzleFormat
checkPuzzleFormat fp = case takeExtension fp of
  ".pzl" -> Right PZL
  ".pzg" -> Right PZG
  ext -> Left $ "unknown format: " ++ ext

maybeSkipCode :: PuzzleOpts -> Maybe Y.Value -> Maybe Y.Value
maybeSkipCode opts = if _code opts then id else const Nothing

handleOne :: PuzzleOpts -> OutputChoice -> FilePath -> IO ()
handleOne opts oc fpin = do
  puzzleFormat <- orExitFile $ checkPuzzleFormat fpin
  let params =
        Params
          (_format opts)
          (config opts)
          oc
          (_scale opts)
          (_code opts)
          puzzleFormat
      fpout = outputPath opts fpin (_format opts) oc
  input <- B.readFile fpin
  output <- orExitFile $ decodeAndDraw params input
  BL.writeFile fpout output
  where
    orExitFile :: Either String a -> IO a
    orExitFile = orExit . mapLeft ((fpin <> ": ") <>)

main :: IO ()
main = do
  opts <- defaultOpts puzzleOpts
  oc <- orExit $ checkOutput opts
  mapM_ (handleOne opts oc) (_input opts)

orExit :: Either String a -> IO a
orExit (Left err) = putStrLn err >> exitFailure
orExit (Right r) = return r
