module Data.Puzzles.Read where

import Data.Puzzles.Things

import Text.Read (readMaybe)

charToIntClue c
    | '0' <= c && c <= '9'  = Just $ fromEnum c - fromEnum '0'
    | otherwise             = Nothing

strToIntClue :: String -> IntClue
strToIntClue = readMaybe

charToCharClue c
    | c == ' ' || c == '.' || c == '-'  = Nothing
    | otherwise                         = Just c

charToMasyuClue :: Char -> MasyuClue
charToMasyuClue '*' = Just MBlack
charToMasyuClue 'o' = Just MWhite
charToMasyuClue c
    | c == ' ' || c == '.'  = Nothing

