{-# LANGUAGE FlexibleContexts, CPP #-}

module Diagrams.Puzzles.CmdLine 
    ( B
    , renderToFile
    , RenderOpts(..)
    , formats
    , checkFormat
    )
  where

import Data.Puzzles.CmdLine (exitErr)

import Diagrams.Prelude hiding (value, option, (<>), Result)
import Control.Monad (unless)

#ifdef CAIRO
import Diagrams.Backend.Cairo (B, renderCairo)
#else
import Diagrams.Backend.SVG (B, renderSVG)
#endif

data RenderOpts = RenderOpts { _file :: FilePath, _w :: Double }

renderB :: FilePath -> SizeSpec V2 Double -> Diagram B -> IO ()
renderB =
#ifdef CAIRO
    renderCairo
#else
    renderSVG
#endif

renderToFile :: RenderOpts -> Diagram B -> IO ()
renderToFile ropts = renderB (_file ropts) (mkWidth $ _w ropts)

formats :: [String]
#ifdef CAIRO
formats = ["png", "svg", "ps", "pdf"]
#else
formats = ["svg"]
#endif

checkFormat :: String -> IO ()
checkFormat f = unless (f `elem` formats) $
                    exitErr $ "unknown format: " ++ f
