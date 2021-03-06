module Draw.Widths where

gridres :: Int
gridres = 40

gridresd :: Double
gridresd = fromIntegral gridres

onepix :: Double
onepix = 1 / fromIntegral gridres

linewidth :: Double
linewidth = onepix

-- width of frame relative to width of thick edge
-- for standard 3 pixel edgewidth this yields a 4 pixel frame
framewidthfactor :: Double
framewidthfactor = 4 / 3

edgewidth :: Double
edgewidth = 3 * onepix

borderwidth :: Double
borderwidth = 1 / 4

outsideScale :: Double
outsideScale = 0.85

noteScale :: Double
noteScale = 0.85
