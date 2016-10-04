{-# LANGUAGE CPP #-}

module Main where

import Test.Tasty
import Test.Tasty.Golden

import System.Process
import System.FilePath

main :: IO ()
main = tests >>= defaultMain

render :: FilePath -> IO ()
render fp = callProcess "stack" ["exec", "drawpuzzle", "--", "-e", "-c", "-f",  "png", fp ]

testFile :: FilePath -> TestTree
testFile fp = goldenVsFile ("comparing " ++ fp)
                           fppng
                           (takeFileName fppng)
                           (render fp)
  where
    fppng = replaceExtension fp ".png"

tests :: IO TestTree
tests = testGroup "compare to old output" . map testFile
        <$> findByExtension [".pzl"] "tests/examples"
