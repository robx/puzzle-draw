module Diagrams.Puzzles.Widths where

gridres :: Int
gridres = 40

gridresd :: Double
gridresd = fromIntegral gridres

onepix :: Double
onepix = 1 / fromIntegral gridres

twopix, fourpix :: Double
twopix = 2 * onepix
fourpix = 4 * onepix

gridwidth :: Double
gridwidth = onepix

framewidthfactor :: Double
framewidthfactor = 4

edgewidth, borderwidth :: Double
edgewidth = 3 * onepix
borderwidth = 1 / 2
