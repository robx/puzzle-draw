module Data.Puzzles.Pyramid where

import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec
import Control.Monad (liftM2)

data Row = R { entries :: [Maybe Int]
             , shaded :: Bool
             }

newtype Pyramid = P {unP :: [Row]}

ex1 = [ "G     ."
      , "W    . ."
      , "W   . 7 3"
      , "G  . 1 . ."
      , "G 1 . . . 3"
      ]

prow :: GenParser Char st Row
prow = do s <- pshaded
          spaces
          es <- pclues
          return (R es s)
pshaded = (char 'G' >> return True) <|> (char 'W' >> return False)
pclues = do c <- pclue
            cs <- many (spaces >> pclue)
            return (c:cs)
pclue = fmap (Just . digitToInt) digit
        <|> (char '.' >> return Nothing)

readrow cs = r
    where Right r = parse prow "(unknown)" cs

readPyramid :: [String] -> Pyramid
readPyramid = P . map readrow

showClues :: [Maybe Int] -> String
showClues = map showClue
    where showClue = maybe '.' (head . show)

instance Show Row where
    show (R c True) = 'G' : showClues c
    show (R c False) = 'W' : showClues c

instance Show Pyramid where
    show = unlines . map show . unP

ex2 = [ "G     2"
      , "G    . ."
      , "G   .o. ."
      , "W  .*. 2o."
      , "G .*.*. .*."
      ]

data KropkiDot = None | Black | White
    deriving Show
data KropkiRow = KR { entriesk :: [Maybe Int]
                    , shadedk :: Bool
                    , dotsk :: [KropkiDot]
                    }
    deriving Show
newtype RowKropkiPyramid = KP {unKP :: [KropkiRow]}
    deriving Show

pkropkirow = do s <- pshaded
                spaces
                (ks, cs) <- pkropkiclues
                return (KR cs s ks)
pkropkiclues = do c <- pclue
                  kcs <- many (liftM2 (,) pkropki pclue)
                  let (ks, cs) = unzip kcs in return (ks, c:cs)
pkropki = (char '*' >> return Black)
          <|> (char 'o' >> return White)
          <|> (char ' ' >> return None)

readkropkirow cs = r
    where Right r = parse pkropkirow "(unknown)" cs

readKropkiPyramid :: [String] -> RowKropkiPyramid
readKropkiPyramid = KP . map readkropkirow
