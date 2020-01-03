{-# LANGUAGE TemplateHaskell #-}

module Draw.Font
  ( fontAnelizaRegular,
    fontBit,
    Font,
  )
where

import Data.FileEmbed
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Graphics.SVGFonts.ReadFont
  ( PreparedFont,
    loadFont',
  )

type Font = PreparedFont Double

anelizaRegular :: String
anelizaRegular =
  Text.unpack . decodeUtf8 $ $(embedFile "data/fonts/aneliza-regular.svg")

fontAnelizaRegular :: Font
fontAnelizaRegular = snd $ loadFont' "aneliza-regular" anelizaRegular

bit :: String
bit = Text.unpack . decodeUtf8 $ $(embedFile "data/fonts/bitstream.svg")

fontBit :: Font
fontBit = snd $ loadFont' "bitstream" bit
