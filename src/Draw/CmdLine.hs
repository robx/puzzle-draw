{-# LANGUAGE FlexibleContexts #-}

module Draw.CmdLine
  ( renderBytesRasterific
  , renderBytesSVG
  , backend
  , BackendType(..)
  , Format(..)
  , lookupFormat
  , extension
  , formats
  )
where

import           Diagrams.Prelude        hiding ( value
                                                , option
                                                , (<>)
                                                , Result
                                                )

import           Data.ByteString.Lazy           ( ByteString )
import           Graphics.Svg.Core              ( renderBS )
import qualified Data.Text                     as Text
import           Codec.Picture                  ( pixelMap )
import           Codec.Picture.Types            ( convertPixel
                                                , dropTransparency
                                                )
import           Codec.Picture.Png              ( encodePng )
import           Codec.Picture.Jpg              ( encodeJpeg )

import qualified Diagrams.Backend.Rasterific   as Rasterific
import qualified Diagrams.Backend.SVG          as SVG

renderBytesRasterific
  :: Format -> SizeSpec V2 Double -> Diagram Rasterific.B -> ByteString
renderBytesRasterific fmt sz dia = case fmt of
  PDF -> Rasterific.renderPdfBS (round w) (round h) sz dia
  PNG -> encodePng
    $ renderDia Rasterific.Rasterific (Rasterific.RasterificOptions sz) dia
  JPG -> encodeJpeg . pixelMap (convertPixel . dropTransparency) $ renderDia
    Rasterific.Rasterific
    (Rasterific.RasterificOptions sz)
    dia
  _ -> error "unsupported format"
 where
  V2 w' h'    = boxExtents (boundingBox dia)
  aspectRatio = h' / w'
  (w, h)      = case getSpec sz of
    V2 (Just ww) (Just hh) -> (ww, hh)
    V2 (Just ww) Nothing   -> (ww, aspectRatio * ww)
    V2 Nothing   (Just hh) -> (hh / aspectRatio, hh)
    V2 Nothing   Nothing   -> (100, 100)

renderBytesSVG :: Format -> SizeSpec V2 Double -> Diagram SVG.B -> ByteString
renderBytesSVG fmt sz = case fmt of
  SVG -> renderBS
    . renderDia SVG.SVG (SVG.SVGOptions sz Nothing (Text.pack "") [] True)
  _ -> error "unsupported format"

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
  _     -> Nothing

extension :: Format -> String
extension f = case f of
  PNG -> "png"
  PDF -> "pdf"
  SVG -> "svg"
  JPG -> "jpg"

formats :: [Format]
formats = [PNG, JPG, PDF, SVG]
