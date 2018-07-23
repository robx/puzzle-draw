{-# LANGUAGE FlexibleContexts #-}

module Draw.CmdLine 
    ( B
    , renderToFile
    , RenderOpts(..)
    , Format(..)
    , lookupFormat
    , extension
    , formats
    )
  where

import Diagrams.Prelude hiding (value, option, (<>), Result)

import Diagrams.Backend.Rasterific (B, renderRasterific)

data RenderOpts = RenderOpts
  { _file :: FilePath
  , _size :: SizeSpec V2 Double
  }

renderToFile :: RenderOpts -> Diagram B -> IO ()
renderToFile ropts = renderRasterific (_file ropts) (_size ropts)

data Format = PNG | PS | PDF

lookupFormat :: String -> Maybe Format
lookupFormat f = case f of
    "png" -> Just PNG
    "ps" -> Just PS
    "pdf" -> Just PDF
    _ -> Nothing

extension :: Format -> String
extension f = case f of
    PNG -> "png"
    PDF -> "pdf"
    PS -> "ps"

formats :: [Format]
formats = [PNG, PS, PDF]
