module Diagrams.TwoD.Puzzles.Widths where

gridres :: Int
gridres = 40

onepix :: Double
onepix = 1 / fromIntegral gridres

twopix = 2 * onepix
fourpix = 4 * onepix

gridwidth :: Double
gridwidth = onepix

framewidthfactor :: Double
framewidthfactor = 4

edgewidth = 3 * onepix

borderwidth = 1 / 4 + onepix / 2
