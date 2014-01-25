module Data.Puzzles.Pyramid where

data Row = R { entries :: [Maybe Int]
             , shaded :: Bool
             }

newtype Pyramid = P {unP :: [Row]}

ex1 = [ "G."
      , "W.."
      , "W.73"
      , "G.1.."
      , "G1...3"
      ]

readClues :: String -> [Maybe Int]
readClues = map readClue
    where readClue '.' = Nothing
          readClue ' ' = Nothing
          readClue c | c >= '1' && c <= '9' = Just (read [c])

showClues :: [Maybe Int] -> String
showClues = map showClue
    where showClue = maybe '.' (head . show)

instance Show Row where
    show (R c True) = 'G' : showClues c
    show (R c False) = 'W' : showClues c

instance Read Row where
    readsPrec d ('G':clues) = [(R (readClues clues) True, "")]
    readsPrec d ('W':clues) = [(R (readClues clues) False, "")]

readPyramid :: [String] -> Pyramid
readPyramid = P . map read

instance Show Pyramid where
    show = unlines . map show . unP
