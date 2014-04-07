import Test.Tasty
import Test.Tasty.HUnit

import Data.Yaml
import Data.List (sort)

import Control.DeepSeq

import Data.Puzzles.Elements (Thermometer)
import Text.Puzzles.Util (parseChar)
import Text.Puzzles.PuzzleTypes
import qualified Data.Puzzles.Grid as Grid
import Data.Puzzles.Pyramid (PyramidSol(..))

import Diagrams.Puzzles.PuzzleGrids
import Diagrams.Prelude
import Diagrams.Backend.SVG

import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Data
import Util

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

test_thermo_1 :: [Thermometer]
test_thermo_1 = either (const []) snd res
  where
    res = parseEither (fst thermosudoku) thermo_1

-- two neighbouring a's, should be fine
test_thermo_2 :: [Thermometer]
test_thermo_2 = either (const []) snd res
  where
    res = parseEither (fst thermosudoku) thermo_2

testBreakSlalom :: Bool
testBreakSlalom =
    case parseMaybe (snd slalom) slalom_sol_broken of
        Nothing -> True
        Just s  -> let d = drawSlalomDiags s
                       svg = renderDia SVG (SVGOptions (Width 100) Nothing) d
                       svgt = renderSvg svg
                   in (show svgt) `deepseq` True

test_tightfit_1 :: Bool
test_tightfit_1 = either (const False) test_both res
  where
    res = parseEither (fst tightfitskyscrapers) tightfit_1
    test_both (o, g) = test_size g && test_clues o
    test_size g = Grid.size g == (3, 3)
    test_clues (Grid.OC l r b t) = l == [Nothing, Nothing, Just 3] &&
                                   r == [Nothing, Just 4, Nothing] &&
                                   b == [Just 3, Just 5, Nothing] &&
                                   t == [Nothing, Nothing, Nothing]

test_pyramid_sol :: Bool
test_pyramid_sol = either (const False) test_content res
  where
    res = parseEither (snd kpyramid) kpyramid_1_sol
    test_content (PyramidSol rs) = rs == [[3], [8,5], [1,9,4], [3,2,7,3], [1,2,4,3,6]]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "parse geradeweg" $ testParse (fst geradeweg) geradeweg_1
    , testCase "parse geradeweg solution" $ testParse (snd geradeweg) geradeweg_1_sol
    , testCase "parse tightfit" $ testParse (fst tightfitskyscrapers) tightfit_1
    , testCase "parse tightfit, correct size" $ test_tightfit_1 @? "error in puzzle"
    , testCase "parse tightfit solution" $ testParse (snd tightfitskyscrapers) tightfit_1_sol
    , testCase "don't parse broken tighfit" $ testNonparse (fst tightfitskyscrapers) tightfit_broken_1
    , testCase "don't parse broken tighfit" $ testNonparse (fst tightfitskyscrapers) tightfit_broken_2
    , testCase "don't parse broken tightfit solution" $
        testNonparse (snd tightfitskyscrapers) tightfit_sol_broken
    , testCase "don't parse broken tightfit solution" $
        testNonparse (snd tightfitskyscrapers) tightfit_sol_broken_2
    , testCase "parse digit" $ (parseMaybe parseChar '5' :: Maybe Int) @=? Just 5
    , testCase "don't parse hex chars" $ (parseMaybe parseChar 'a' :: Maybe Int) @=? Nothing
    , testCase "don't break on non-digits" $ (parseMaybe parseChar ' ' :: Maybe Int) @=? Nothing
    , testCase "don't parse invalid slalom solution" $ testNonparse (snd slalom) slalom_sol_broken
    , testCase "don't break rendering invalid slalom solution"
         $ testBreakSlalom @? "just testing against errors"
    , testCase "parse kpyramid" $ testParse (fst kpyramid) kpyramid_1
    , testCase "parse kpyramid sol" $ testParse (snd kpyramid) kpyramid_1_sol
    , testCase "parse kpyramid sol properly" $ test_pyramid_sol @? "wrong solution"
    , testCase "don't parse broken kpyramid" $ testNonparse (fst kpyramid) kpyramid_broken_1
    , testCase "don't parse broken kpyramid" $ testNonparse (fst kpyramid) kpyramid_broken_2
    , testCase "don't parse broken kpyramid" $ testNonparse (fst kpyramid) kpyramid_broken_3
    , testCase "parse compass" $ testParse (fst compass) compass_1
    , testCase "don't parse borken compass" $ testNonparse (fst compass) compass_broken_1
    , testCase "don't parse borken compass" $ testNonparse (fst compass) compass_broken_2
    , testCase "don't parse borken compass" $ testNonparse (fst compass) compass_broken_3
    , testCase "don't parse borken compass" $ testNonparse (fst compass) compass_broken_4
    , testCase "don't parse borken compass" $ testNonparse (fst compass) compass_broken_5
    , testCase "parse thermo" $ testParse (fst thermosudoku) thermo_1
    , testCase "parse thermo" $ testParse (fst thermosudoku) thermo_2
    , testCase "parse thermo, thermometers" $ sort test_thermo_1 @?= [ [(0, 4), (1, 5), (2, 4), (1, 3)]
                                                                     , [(4, 0), (3, 1), (4, 2), (5, 1)] ]
    , testCase "parse thermo, thermometers" $ sort test_thermo_2 @?= [ [(0, 1), (1, 0), (2, 0)]
                                                                     , [(0, 2), (1, 3), (2, 4)]
                                                                     , [(4, 0), (4, 1), (3, 2)] ]
    , testCase "don't parse broken thermo" $ testNonparse (fst thermosudoku) thermo_broken_1
    , testCase "don't parse broken thermo" $ testNonparse (fst thermosudoku) thermo_broken_2
    ]
