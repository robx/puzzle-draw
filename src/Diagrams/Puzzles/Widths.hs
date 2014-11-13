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
gridwidth = onepix / 2

framewidthfactor :: Double
framewidthfactor = 8

edgewidth :: Double
edgewidth = 3 * onepix

borderwidth :: Double
borderwidth = 1 / 2
