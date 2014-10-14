{-# LANGUAGE FlexibleContexts, CPP #-}

module Diagrams.Puzzles.CmdLine 
    ( B
    , renderToFile
    , RenderOpts(..)
    , formats
    , checkFormat
    , checkType
    , exitErr
    , readPuzzle
    )
  where

import Diagrams.Prelude hiding (value, option, (<>), Result)

#ifdef CAIRO
import Diagrams.Backend.Cairo (B, renderCairo)
#else
import Diagrams.Backend.SVG (B, renderSVG)
#endif

import Text.Puzzles.Puzzle
import Data.Puzzles.PuzzleTypes

import System.Exit

import Control.Monad (unless)
import qualified Data.Yaml as Y

data RenderOpts = RenderOpts { _file :: FilePath, _w :: Double }

renderB :: FilePath -> SizeSpec2D -> Diagram B R2 -> IO ()
renderB =
#ifdef CAIRO
    renderCairo
#else
    renderSVG
#endif

renderToFile :: RenderOpts -> Diagram B R2 -> IO ()
renderToFile ropts = renderB (_file ropts) (Width $ _w ropts)

formats :: [String]
#ifdef CAIRO
formats = ["png", "svg", "ps", "pdf"]
#else
formats = ["svg"]
#endif

checkFormat :: String -> IO ()
checkFormat f = unless (f `elem` formats) $
                    exitErr $ "unknown format: " ++ f

checkType :: Maybe String -> IO PuzzleType
checkType mt = do
    t <- maybe errno return mt
    maybe (errunk t) return (lookupType t)
  where
    errno    = exitErr "no puzzle type given"
    errunk t = exitErr $ "unknown puzzle type: " ++ t

readPuzzle :: FilePath -> IO (Either Y.ParseException TypedPuzzle)
readPuzzle = Y.decodeFileEither

exitErr :: String -> IO a
exitErr e = putStrLn e >> exitFailure
