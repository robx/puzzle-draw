{-# LANGUAGE TemplateHaskell #-}

module Draw.Font
    ( fontAnelizaRegular
    , fontBit
    , Font
    )
  where

import Graphics.SVGFonts.ReadFont (PreparedFont, loadFont')
import Data.FileEmbed
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as Text

type Font = PreparedFont Double

anelizaRegular :: String
anelizaRegular = Text.unpack . decodeUtf8 $ $(embedFile "data/fonts/aneliza-regular.svg")

fontAnelizaRegular :: Font
fontAnelizaRegular = snd $ loadFont' "aneliza-regular" anelizaRegular

bit :: String
bit = Text.unpack . decodeUtf8 $ $(embedFile "data/fonts/bitstream.svg")

fontBit :: Font
fontBit = snd $ loadFont' "bitstream" bit
