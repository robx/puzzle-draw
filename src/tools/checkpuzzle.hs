{-# LANGUAGE FlexibleContexts #-}

module Main where

import Text.Puzzles.Puzzle
import Data.Puzzles.PuzzleTypes (typeNames, PuzzleType(..))
import Data.Puzzles.CmdLine (exitErr, readPuzzle, checkType)

import Data.Puzzles.Elements (digitList)
import qualified Text.Puzzles.PuzzleTypes as T

import Options.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.List (intercalate, sort)

import System.Exit
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
    { _type     :: Maybe String
    , _input    :: FilePath
    }

puzzleOpts :: Parser PuzzleOpts
puzzleOpts = PuzzleOpts
    <$> (optional . strOption $
            (long "type" <> short 't'
             <> metavar "TYPE"
             <> help "Puzzle type, overriding type in input file"))
    <*> argument str
            (metavar "INPUT"
             <> help "Puzzle file in .pzl format")

defaultOpts :: Parser a -> IO a
defaultOpts optsParser = do
    prog <- getProgName
    let p = info (helper <*> optListTypes <*> optsParser)
                (fullDesc
                 <> progDesc "Command-line puzzle checking."
                 <> header prog)
    execParser p

main :: IO ()
main = do
    opts <- defaultOpts puzzleOpts
    mp <- readPuzzle (_input opts)
    TP mt pv msv _ <- case mp of Left  e -> exitErr $
                                             "parse failure: " ++ show e
                                 Right p -> return p
    t <- checkType $ _type opts `mplus` mt
    sv <- maybe (exitErr $ "need solution") return msv 
    let es = Y.parseEither (check t) (pv, sv)
    case es of Left err  -> exitErr $ "parse failure: " ++ err
               Right []  -> exitSuccess
               Right es' -> mapM_ putStrLn es' >> exitFailure

check :: PuzzleType -> (Y.Value, Y.Value) -> Y.Parser [String]
check t (pv, sv) =
    case t of
        ABCtje -> checkABCtje (pv, sv)
        _      -> return []

checkABCtje :: (Y.Value, Y.Value) -> Y.Parser [String]
checkABCtje (pv, sv) = do
    p <- fst T.abctje $ pv
    s <- snd T.abctje $ sv
    return . catMaybes . map (\c -> c p s) $
        [ solutionKeys, values ]
  where
    solutionKeys (ds, _) s = let have = sort (digitList ds)
                                 want = sort (map fst (s))
                             in if have /= want then Just "unequal digit lists" else Nothing
    values (_, ws) vs = (\es -> case es of [] -> Nothing
                                           _  -> Just $ intercalate ", " es)
                      . catMaybes
                      . map (\(w, v) -> if val w == v
                                            then Nothing
                                            else Just (w ++ " should be " ++ show (val w)))
                      $ ws
      where
        l c = fromMaybe 0 . lookup c . map (\(x, y) -> (y, x)) $ vs
        val = sum . map l
