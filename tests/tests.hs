import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text, pack)
import Data.Yaml

import Control.DeepSeq

import Data.Puzzles.ReadPuzzle (geradeweg', tightfitskyscrapers', slalom')
import Data.Puzzles.Read (parseChar)

import Diagrams.TwoD.Puzzles.Draw
import Diagrams.Prelude
import Diagrams.Backend.SVG

import Text.Blaze.Svg.Renderer.Text (renderSvg)

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

geradeweg_1 :: Text
geradeweg_1 = pack . unlines $
    [ ".....    "
    , ".211."
    , "..2..  "
    , "3...4"
    , "....."
    ]

geradeweg_1_sol = pack . unlines $
    [ "┌┐┌─┐"
    , "││└┐│  "
    , "│└─┘│"
    , "└──┐│"
    , "  .└┘ "
    ]

tightfit_1 :: Text
tightfit_1 = pack . unlines $
    [ "3/\\.-"
    , "-\\.\\4"
    , "-.\\/-"
    , " 35-"
    ]

tightfit_1_sol :: Text
tightfit_1_sol = pack . unlines $
    [ "2/1 4\\5  3"
    , "  4\\5  3  2\\1 "
    , "   3  1\\2 5/4"
    ]

tightfit_sol_broken :: Text
tightfit_sol_broken = pack "2/1 4 /5"

tightfit_sol_broken_2 :: Text
tightfit_sol_broken_2 = pack "2/x 4 3/5"

slalom_sol_broken :: Text
slalom_sol_broken = pack "//\\ /\\x5 "

justShow :: Show a => Maybe a -> Bool
justShow Nothing = False
justShow (Just x) = show x `deepseq` True

testParse :: Show a => (Value -> Parser a) -> Text -> Assertion
testParse p t = (justShow . parseMaybe p . String $ t) @? "bad parse"

testNonparse :: Show a => (Value -> Parser a) -> Text -> Assertion
testNonparse p t = (not . justShow . parseMaybe p . String $ t)
                   @? "parsed but shouldn't"

testBreakSlalom :: Bool
testBreakSlalom =
    case parseMaybe (snd slalom') (String slalom_sol_broken) of
        Nothing -> True
        Just s  -> let d = drawSlalomDiags s
                       svg = renderDia SVG (SVGOptions (Width 100) Nothing) d
                       svgt = renderSvg svg
                   in (show svgt) `deepseq` True

unitTests = testGroup "Unit tests"
    [ testCase "parse geradeweg" $ testParse (fst geradeweg') geradeweg_1
    , testCase "parse geradeweg solution" $ testParse (snd geradeweg') geradeweg_1_sol
    , testCase "parse tightfit" $ testParse (fst tightfitskyscrapers') tightfit_1
    , testCase "parse tightfit solution" $ testParse (snd tightfitskyscrapers') tightfit_1_sol
    , testCase "don't parse broken tightfit solution" $
        testNonparse (snd tightfitskyscrapers') tightfit_sol_broken
    , testCase "don't parse broken tightfit solution" $
        testNonparse (snd tightfitskyscrapers') tightfit_sol_broken_2
    , testCase "parse digit" $ (parseMaybe parseChar '5' :: Maybe Int) @=? Just 5
    , testCase "don't parse hex chars" $ (parseMaybe parseChar 'a' :: Maybe Int) @=? Nothing
    , testCase "don't break on non-digits" $ (parseMaybe parseChar ' ' :: Maybe Int) @=? Nothing
    , testCase "don't parse invalid slalom solution" $ testNonparse (snd slalom') slalom_sol_broken
    , testCase "don't break rendering invalid slalom solution"
         $ testBreakSlalom @? "just testing against errors"
    ]
