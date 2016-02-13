{-# LANGUAGE CPP #-}

module Main where

import Test.Tasty
import Test.Tasty.Golden

import System.Process
import System.FilePath

main :: IO ()
main = tests >>= defaultMain

drawpuzzle :: FilePath
drawpuzzle = ".stack-work/install/x86_64-osx/lts-5.2/7.10.3/bin/drawpuzzle"

render :: FilePath -> IO ()
render fp = callProcess drawpuzzle [ "-e", "-c", "-f",  "png", fp ]

testFile :: FilePath -> TestTree
testFile fp = goldenVsFile ("comparing " ++ fp)
                           fppng
                           (takeFileName fppng)
                           (render fp)
  where
    fppng = replaceExtension fp ".png"

tests :: IO TestTree
#ifdef CAIRO
tests = testGroup "compare to old output" . map testFile
        <$> findByExtension [".pzl"] "tests/examples"
#else
tests = return $ testGroup "comparison disabled, requires cairo" []
#endif
