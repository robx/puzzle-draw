{-# LANGUAGE FlexibleContexts #-}

module Draw.CmdLine 
    ( renderRasterific
    , renderSVG
    , backend
    , BackendType(..)
    , RenderOpts(..)
    , Format(..)
    , lookupFormat
    , extension
    , formats
    )
  where

import Diagrams.Prelude hiding (value, option, (<>), Result)

import qualified Diagrams.Backend.Rasterific as Rasterific
import qualified Diagrams.Backend.SVG as SVG

data RenderOpts = RenderOpts
  { _file :: FilePath
  , _size :: SizeSpec V2 Double
  }

renderRasterific :: RenderOpts -> Diagram Rasterific.B -> IO ()
renderRasterific ropts = Rasterific.renderRasterific (_file ropts) (_size ropts)

renderSVG :: RenderOpts -> Diagram SVG.B -> IO ()
renderSVG ropts = SVG.renderSVG (_file ropts) (_size ropts)

data Format = PNG | PDF | SVG | JPG

data BackendType = BackendRasterific | BackendSVG

backend :: Format -> BackendType
backend b = case b of
  PNG -> BackendRasterific
  PDF -> BackendRasterific
  JPG -> BackendRasterific
  SVG -> BackendSVG

lookupFormat :: String -> Maybe Format
lookupFormat f = case f of
    "png" -> Just PNG
    "pdf" -> Just PDF
    "svg" -> Just SVG
    "jpg" -> Just JPG
    _ -> Nothing

extension :: Format -> String
extension f = case f of
    PNG -> "png"
    PDF -> "pdf"
    SVG -> "svg"
    JPG -> "jpg"

formats :: [Format]
formats = [PNG, JPG, PDF, SVG]
