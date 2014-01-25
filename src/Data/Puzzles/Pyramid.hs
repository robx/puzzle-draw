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

readClue '.' = Nothing
readClue ' ' = Nothing
readClue c | c >= '1' && c <= '9' = Just (read [c])

readClues :: String -> [Maybe Int]
readClues = map readClue

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

ex2 = [ "G2"
      , "G. ."
      , "G.o. ."
      , "W.*. 2o."
      , "G.*.*. .*."
      ]

data KropkiDot = None | Black | White
    deriving Show
data KropkiRow = KRow { entriesk :: [Maybe Int]
                      , shadedk :: Bool
                      , dotsk :: [KropkiDot]
                      }
    deriving Show
newtype RowKropkiPyramid = KP {unKP :: [KropkiRow]}
    deriving Show

readKropkiRow :: String -> KropkiRow
readKropkiRow (s:c:xs) = KRow cs (readShaded s) ks
    where readShaded 'G' = True
          readShaded 'W' = False
          readKropki '*' = Black
          readKropki 'o' = White
          readKropki ' ' = None
          readKC [] = []
          readKC (k:c:xs) = (readKropki k, readClue c) : readKC xs
          (ks, cs) = unzip $ readKC xs

readKropkiPyramid :: [String] -> RowKropkiPyramid
readKropkiPyramid = KP . map readKropkiRow
