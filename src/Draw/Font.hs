module Draw.Font
    ( fontAnelizaRegular
    , fontBit
    , Font
    )
  where

import Graphics.SVGFonts.Fonts (bit)
import Graphics.SVGFonts.ReadFont (PreparedFont, loadFont)

import Paths_puzzle_draw (getDataFileName)

type Font = PreparedFont Double

fontAnelizaRegular :: IO Font
fontAnelizaRegular = getDataFileName "data/fonts/aneliza-regular.svg" >>= loadFont

fontBit :: IO Font
fontBit = getDataFileName "data/fonts/bitstream.svg" >>= loadFont
