import Test.Tasty
import Test.Tasty.HUnit

import Data.Yaml
import Data.List (sort)
import qualified Data.Map as Map
import Control.DeepSeq

import Text.Puzzles.Puzzle
import Data.Puzzles.Elements (Thermometer, MasyuPearl(..))
import Text.Puzzles.Util
    (parseChar, parseMultiOutsideClues, parseEdgeGrid)
import Text.Puzzles.PuzzleTypes
import qualified Data.Puzzles.Grid as Grid
import Data.Puzzles.Pyramid (PyramidSol(..))
import Data.Puzzles.Grid (multiOutsideClues, OutsideClues(..))
import Data.Puzzles.GridShape (Edge, Square(..))

import Diagrams.Puzzles.PuzzleGrids
import Diagrams.Prelude
import Diagrams.Backend.SVG

import Text.Blaze.Svg.Renderer.Text (renderSvg)

import Data
import Util

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ parseUtilTests, parseTests, parseDataTests, renderTests, dataTests ]

testParsePzl, testParseSol, testNonparsePzl, testNonparseSol ::
    (Show a, Show b) => String -> ParsePuzzle a b -> Value -> TestTree
testParsePzl name parser yaml =
    testCase ("parse " ++ name) $ testParse (fst parser) yaml
testParseSol name parser yaml =
    testCase ("parse " ++ name ++ " (sol)") $ testParse (snd parser) yaml
testNonparsePzl name parser yaml =
    testCase ("don't parse broken " ++ name) $ testNonparse (fst parser) yaml
testNonparseSol name parser yaml =
    testCase ("don't parse broken " ++ name ++ " (sol)") $
        testNonparse (snd parser) yaml

parseUtilTests :: TestTree
parseUtilTests = testGroup "Parsing infrastructure tests (parseChar)"
    [ testCase "parse digit" $
        (parseMaybe parseChar '5' :: Maybe Int) @=? Just 5
    , testCase "don't parse hex chars" $
        (parseMaybe parseChar 'a' :: Maybe Int) @=? Nothing
    , testCase "don't break on non-digits" $
        (parseMaybe parseChar ' ' :: Maybe Int) @=? Nothing
    ]

parseTests :: TestTree
parseTests = testGroup "Parsing tests (full puzzles, no details)"
    [ testParsePzl "geradeweg" geradeweg geradeweg_1
    , testParseSol "geradeweg" geradeweg geradeweg_1_sol
    , testParsePzl "tightfit"  tightfitskyscrapers tightfit_1
    , testParseSol "tightfit"  tightfitskyscrapers tightfit_1_sol
    , testNonparsePzl "tightfit" tightfitskyscrapers tightfit_broken_1
    , testNonparsePzl "tightfit" tightfitskyscrapers tightfit_broken_2
    , testNonparseSol "tightfit" tightfitskyscrapers tightfit_sol_broken
    , testNonparseSol "tightfit" tightfitskyscrapers tightfit_sol_broken_2
    , testNonparseSol "slalom" slalom slalom_sol_broken
    , testParsePzl "kpyramid" kpyramid kpyramid_1
    , testParseSol "kpyramid" kpyramid kpyramid_1_sol
    , testNonparsePzl "kpyramid" kpyramid kpyramid_broken_1
    , testNonparsePzl "kpyramid" kpyramid kpyramid_broken_2
    , testNonparsePzl "kpyramid" kpyramid kpyramid_broken_3
    , testParsePzl "compass" compass compass_1
    , testNonparsePzl "compass" compass compass_broken_1
    , testNonparsePzl "compass" compass compass_broken_2
    , testNonparsePzl "compass" compass compass_broken_3
    , testNonparsePzl "compass" compass compass_broken_4
    , testNonparsePzl "compass" compass compass_broken_5
    , testParsePzl "thermosudoku" thermosudoku thermo_1
    , testParsePzl "thermosudoku" thermosudoku thermo_2
    , testNonparsePzl "thermosudoku" thermosudoku thermo_broken_1
    , testNonparsePzl "thermosudoku" thermosudoku thermo_broken_2
    , testParsePzl "boxof2or3" boxof2or3 boxof2or3_1
    ]

test_thermo_1 :: [Thermometer]
test_thermo_1 = either (const []) snd $
                parseEither (fst thermosudoku) thermo_1

-- two neighbouring a's, should be fine
test_thermo_2 :: [Thermometer]
test_thermo_2 = either (const []) snd $
                parseEither (fst thermosudoku) thermo_2

testThermo :: [Thermometer] -> [Thermometer] -> Assertion
testThermo t expect = sort t @?= expect

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
    test_content (PyramidSol rs) =
        rs == [[3], [8,5], [1,9,4], [3,2,7,3], [1,2,4,3,6]]

test_multioutside :: Assertion
test_multioutside = Right oc @=? res
  where
    res = parseEither parseMultiOutsideClues multioutside
    oc = OC [[3], [1, 2]] [[1, 0], []] [[0, 0, 1]] [[1, -1]]
         :: OutsideClues [Int]

test_edge_grid :: Assertion
test_edge_grid = Right (gn, gc, es) @=? res
  where
    res = parseEither parseEdgeGrid edgeGrid_1
    gn = Grid.Grid (Square 0 0) Map.empty :: Grid.SGrid MasyuPearl
    gc = Grid.Grid (Square 0 0) Map.empty :: Grid.SGrid Int
    es = [] :: [Edge]

parseDataTests :: TestTree
parseDataTests = testGroup "Parsing tests (full puzzles, result checks)"
    [ testCase "parse tightfit, correct size" $ test_tightfit_1 @? "error in puzzle"
    , testCase "parse kpyramid sol properly" $ test_pyramid_sol @? "wrong solution"
    , testCase "parse thermos" $ testThermo test_thermo_1
                                            [ [(0, 4), (1, 5), (2, 4), (1, 3)]
                                            , [(4, 0), (3, 1), (4, 2), (5, 1)] ]
    , testCase "parse thermos" $ testThermo test_thermo_2
                                            [ [(0, 1), (1, 0), (2, 0)]
                                            , [(0, 2), (1, 3), (2, 4)]
                                            , [(4, 0), (4, 1), (3, 2)] ]
    , testCase "parse multioutsideclues" $ test_multioutside
    , testCase "parse edge grid" $ test_edge_grid
    ]

-- this used to cause a rendering crash, though it's
-- caught by parsing by now
testBreakSlalom :: Bool
testBreakSlalom =
    case parseMaybe (snd slalom) slalom_sol_broken of
        Nothing -> True
        Just s  -> let d = drawSlalomDiags s
                       svg = renderDia SVG (SVGOptions (Width 100) Nothing) d
                       svgt = renderSvg svg
                   in (show svgt) `deepseq` True

renderTests :: TestTree
renderTests = testGroup "Rendering tests"
    [ testCase "don't break rendering invalid slalom solution"
         $ testBreakSlalom @? "just testing against errors"
    ]

testMultiOutsideClues :: Assertion
testMultiOutsideClues = multiOutsideClues (OC l r b t) `sorteq` res
  where
    sorteq xs ys = sort xs @?= sort ys
    l = [[1, 2], [3]]
    r = [[], [1, 0]]
    b = [[0, 0, 1]]
    t = [[1, -1]]
    res = [ ((-1,0), 1), ((-2,0), 2), ((-1,1), 3),
            ((1,1), 1), ((2,1), 0), ((0,-1), 0), ((0,-2), 0), ((0,-3), 1),
            ((0,2), 1), ((0,3), -1) ] :: [((Int, Int), Int)]

dataTests :: TestTree
dataTests = testGroup "Generic tests for the Data modules"
    [ testCase "multiOutsideClues" testMultiOutsideClues
    ]
