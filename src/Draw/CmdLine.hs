{-# LANGUAGE FlexibleContexts #-}

module Draw.CmdLine 
    ( B
    , renderToFile
    , RenderOpts(..)
    , formats
    , checkFormat
    )
  where

import Data.CmdLine (exitErr)

import Diagrams.Prelude hiding (value, option, (<>), Result)
import Control.Monad (unless)

import Diagrams.Backend.Rasterific (B, renderRasterific)

data RenderOpts = RenderOpts
  { _file :: FilePath
  , _size :: SizeSpec V2 Double
  }

renderToFile :: RenderOpts -> Diagram B -> IO ()
renderToFile ropts = renderRasterific (_file ropts) (_size ropts)

formats :: [String]
formats = ["png", "ps", "pdf"]

checkFormat :: String -> IO ()
checkFormat f = unless (f `elem` formats) $
                    exitErr $ "unknown format: " ++ f
