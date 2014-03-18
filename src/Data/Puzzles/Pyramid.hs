module Data.Puzzles.Pyramid where

import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec
import Control.Monad (liftM2, mplus)

data Row = R { entries :: [Maybe Int]
             , shaded  :: Bool
             }

newtype Pyramid = Pyr {unPyr :: [Row]}

psize :: Pyramid -> Int
psize (Pyr rows) = length rows

ex1 :: [String]
ex1 = [ "G     ."
      , "W    . ."
      , "W   . 7 3"
      , "G  . 1 . ."
      , "G 1 . . . 3"
      ]

mergepyramids :: Pyramid -> Pyramid -> Pyramid
mergepyramids (Pyr rs) (Pyr qs)
    | length rs /= length qs  = error "can't merge differently sized pyramids"
    | otherwise               = Pyr (zipWith mergerow rs qs)
    where mergerow (R es s) (R es' s') = R (zipWith mplus es es') (s || s')

mergekpyramids :: RowKropkiPyramid -> Pyramid -> RowKropkiPyramid
mergekpyramids (KP rs) (Pyr qs)
    | length rs /= length qs  = error "can't merge differently sized pyramids"
    | otherwise               = KP (zipWith mergerow rs qs)
    where mergerow (KR es s ds) (R es' s') =
              KR (zipWith mplus es es') (s || s') ds

prow :: GenParser Char st Row
prow = do s <- pshaded
          spaces
          es <- pclues
          return (R es s)

pshaded :: GenParser Char st Bool
pshaded = (char 'G' >> return True) <|> (char 'W' >> return False)

pclues :: GenParser Char st [Maybe Int]
pclues = do c <- pclue
            cs <- many (spaces >> pclue)
            return (c:cs)

pclue :: GenParser Char st (Maybe Int)
pclue = fmap (Just . digitToInt) digit
        <|> (char '.' >> return Nothing)

readrow :: String -> Row
readrow cs = r
    where Right r = parse prow "(unknown)" cs

readPyramid :: [String] -> Pyramid
readPyramid = Pyr . map readrow

readPlainPyramid :: [String] -> Pyramid
readPlainPyramid = readPyramid . map ('W':)

showClues :: [Maybe Int] -> String
showClues = map showClue
    where showClue = maybe '.' (head . show)

instance Show Row where
    show (R c True) = 'G' : showClues c
    show (R c False) = 'W' : showClues c

instance Show Pyramid where
    show = unlines . map show . unPyr

ex2 :: [String]
ex2 = [ "G     2"
      , "G    . ."
      , "G   .o. ."
      , "W  .*. 2o."
      , "G .*.*. .*."
      ]

data KropkiDot = None | Black | White
    deriving Show
data KropkiRow = KR { entriesk :: [Maybe Int]
                    , shadedk  :: Bool
                    , dotsk    :: [KropkiDot]
                    }
    deriving Show
newtype RowKropkiPyramid = KP {unKP :: [KropkiRow]}
    deriving Show

plainpyramid :: RowKropkiPyramid -> Pyramid
plainpyramid (KP rows) = Pyr (map r rows)
    where r x = R (entriesk x) (shadedk x)

pkropkirow :: GenParser Char st KropkiRow
pkropkirow = do s <- pshaded
                spaces
                (ks, cs) <- pkropkiclues
                return (KR cs s ks)

pkropkiclues :: GenParser Char st ([KropkiDot], [Maybe Int])
pkropkiclues = do c <- pclue
                  kcs <- many (liftM2 (,) pkropki pclue)
                  let (ks, cs) = unzip kcs in return (ks, c:cs)

pkropki :: GenParser Char st KropkiDot
pkropki = (char '*' >> return Black)
          <|> (char 'o' >> return White)
          <|> (char ' ' >> return None)

readkropkirow :: String -> KropkiRow
readkropkirow cs = r
    where Right r = parse pkropkirow "(unknown)" cs

readKropkiPyramid :: [String] -> RowKropkiPyramid
readKropkiPyramid = KP . map readkropkirow
