import Data
import Data.Elements
  ( MasyuPearl (..),
    Thermometer,
  )
import qualified Data.Grid as Grid
import Data.Grid
import Data.GridShape
import qualified Data.GridShapeSpec
import qualified Data.GridSpec
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Pyramid (PyramidSol (..))
import Data.Util
import Data.Yaml
import qualified Draw.GridSpec
import qualified Draw.PuzzleTypesSpec
import Parse.Puzzle
import Parse.PuzzleTypes
import qualified Parse.PuzzleTypesSpec
import Parse.Util
  ( parseChar,
    parseMultiOutsideClues,
    parsePlainEdgeGrid,
  )
import qualified Parse.UtilSpec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Util

main :: IO ()
main = do
  ss <- specs
  defaultMain . testGroup "Tests" $ tests ++ ss

specs :: IO [TestTree]
specs =
  mapM
    (uncurry testSpec)
    [ ("Data.Grid", Data.GridSpec.spec),
      ("Data.GridShape", Data.GridShapeSpec.spec),
      ("Draw.Grid", Draw.GridSpec.spec),
      ("Draw.PuzzleTypes", Draw.PuzzleTypesSpec.spec),
      ("Parse.PuzzleTypes", Parse.PuzzleTypesSpec.spec),
      ("Parse.Util", Parse.UtilSpec.spec)
    ]

tests :: [TestTree]
tests = [parseUtilTests, parseTests, parseDataTests, dataTests]

testParsePzl :: Show a => String -> ParsePuzzle a b -> Value -> TestTree
testParsePzl name parser yaml =
  testCase ("parse " ++ name) $ testParse (fst parser) yaml

testParseSol :: Show b => String -> ParsePuzzle a b -> Value -> TestTree
testParseSol name parser yaml =
  testCase ("parse " ++ name ++ " (sol)") $ testParse (snd parser) yaml

testNonparsePzl :: Show a => String -> ParsePuzzle a b -> Value -> TestTree
testNonparsePzl name parser yaml =
  testCase ("don't parse broken " ++ name) $ testNonparse (fst parser) yaml

testNonparseSol :: Show b => String -> ParsePuzzle a b -> Value -> TestTree
testNonparseSol name parser yaml =
  testCase ("don't parse broken " ++ name ++ " (sol)") $
    testNonparse (snd parser) yaml

parseUtilTests :: TestTree
parseUtilTests =
  testGroup
    "Parsing infrastructure tests (parseChar)"
    [ testCase "parse digit" $ (parseMaybe parseChar '5' :: Maybe Int) @=? Just 5,
      testCase "don't parse hex chars" $
        (parseMaybe parseChar 'a' :: Maybe Int)
          @=? Nothing,
      testCase "don't break on non-digits" $
        (parseMaybe parseChar ' ' :: Maybe Int)
          @=? Nothing
    ]

parseTests :: TestTree
parseTests =
  testGroup
    "Parsing tests (full puzzles, no details)"
    [ testParsePzl "geradeweg" geradeweg geradeweg_1,
      testParseSol "geradeweg" geradeweg geradeweg_1_sol,
      testParsePzl "tightfit" tightfitskyscrapers tightfit_1,
      testParseSol "tightfit" tightfitskyscrapers tightfit_1_sol,
      testNonparsePzl "tightfit" tightfitskyscrapers tightfit_broken_1,
      testNonparsePzl "tightfit" tightfitskyscrapers tightfit_broken_2,
      testNonparseSol "tightfit" tightfitskyscrapers tightfit_sol_broken,
      testNonparseSol "tightfit" tightfitskyscrapers tightfit_sol_broken_2,
      testNonparseSol "slalom" slalom slalom_sol_broken,
      testParsePzl "kpyramid" kpyramid kpyramid_1,
      testParseSol "kpyramid" kpyramid kpyramid_1_sol,
      testNonparsePzl "kpyramid" kpyramid kpyramid_broken_1,
      testNonparsePzl "kpyramid" kpyramid kpyramid_broken_2,
      testNonparsePzl "kpyramid" kpyramid kpyramid_broken_3,
      testParsePzl "compass" compass compass_1,
      testNonparsePzl "compass" compass compass_broken_1,
      testNonparsePzl "compass" compass compass_broken_2,
      testNonparsePzl "compass" compass compass_broken_3,
      testNonparsePzl "compass" compass compass_broken_4,
      testNonparsePzl "compass" compass compass_broken_5,
      testParsePzl "thermosudoku" thermosudoku thermo_1,
      testParsePzl "thermosudoku" thermosudoku thermo_2,
      testNonparsePzl "thermosudoku" thermosudoku thermo_broken_1,
      testNonparsePzl "thermosudoku" thermosudoku thermo_broken_2
    ]

test_thermo_1 :: [Thermometer]
test_thermo_1 = either (const []) snd $ parseEither (fst thermosudoku) thermo_1

-- two neighbouring a's, should be fine
test_thermo_2 :: [Thermometer]
test_thermo_2 = either (const []) snd $ parseEither (fst thermosudoku) thermo_2

testThermo :: [Thermometer] -> [[Coord]] -> Assertion
testThermo t expect = sort t @?= map (map fromCoord) expect

test_tightfit_1 :: Bool
test_tightfit_1 = either (const False) test_both res
  where
    res = parseEither (fst tightfitskyscrapers) tightfit_1
    test_both (o, g) = test_size g && test_clues o
    test_size g = Grid.size (Map.mapKeys toCoord g) == (3, 3)
    test_clues (Grid.OC l r b t) =
      l
        == [Nothing, Nothing, Just 3]
        && r
        == [Nothing, Just 4, Nothing]
        && b
        == [Just 3, Just 5, Nothing]
        && t
        == [Nothing, Nothing, Nothing]

test_pyramid_sol :: Bool
test_pyramid_sol = either (const False) test_content res
  where
    res = parseEither (snd kpyramid) kpyramid_1_sol
    test_content (PyramidSol rs) =
      rs == [[3], [8, 5], [1, 9, 4], [3, 2, 7, 3], [1, 2, 4, 3, 6]]

test_multioutside :: Assertion
test_multioutside = Right oc @=? res
  where
    res = parseEither parseMultiOutsideClues multioutside
    oc =
      OC [[3], [1, 2]] [[1, 0], []] [[0, 0, 1]] [[1, -1]] ::
        OutsideClues
          Coord
          [Int]

test_plain_edge_grid :: Assertion
test_plain_edge_grid = Right (gn, gc, sort es) @=? res'
  where
    res = parseEither parsePlainEdgeGrid edgeGrid_1
    res' = fmap (\(x, y, e) -> (x, y, sort e)) res
    gn :: Grid.Grid N (Maybe MasyuPearl)
    gn =
      Map.mapKeys fromCoord
        . Map.fromList
        $ [ ((0, 0), Just MBlack),
            ((0, 1), Just MWhite),
            ((1, 0), Just MWhite),
            ((1, 1), Just MBlack),
            ((2, 0), Nothing),
            ((2, 1), Just MBlack),
            ((3, 0), Nothing),
            ((3, 1), Just MWhite)
          ]
    gc :: Grid.Grid C Int
    gc =
      Map.mapKeys fromCoord
        . Map.fromList
        $ [((0, 0), 1), ((1, 0), 2), ((2, 0), 3)]
    es =
      map
        (\(E c d) -> E (fromCoord c) d)
        [ E (0, 0) Horiz,
          E (0, 1) Horiz,
          E (1, 1) Horiz,
          E (2, 1) Horiz,
          E (0, 0) Vert,
          E (1, 0) Vert
        ]

parseDataTests :: TestTree
parseDataTests =
  testGroup
    "Parsing tests (full puzzles, result checks)"
    [ testCase "parse tightfit, correct size" $
        test_tightfit_1
          @? "error in puzzle",
      testCase "parse kpyramid sol properly" $
        test_pyramid_sol
          @? "wrong solution",
      testCase "parse thermos" $
        testThermo
          test_thermo_1
          [[(0, 4), (1, 5), (2, 4), (1, 3)], [(4, 0), (3, 1), (4, 2), (5, 1)]],
      testCase "parse thermos" $
        testThermo
          test_thermo_2
          [ [(0, 1), (1, 0), (2, 0)],
            [(0, 2), (1, 3), (2, 4)],
            [(4, 0), (4, 1), (3, 2)]
          ],
      testCase "parse multioutsideclues" $ test_multioutside,
      testCase "parse edge grid" $ test_plain_edge_grid
    ]

sorteq :: (Show a, Ord a) => [a] -> [a] -> Assertion
sorteq xs ys = sort xs @?= sort ys

testEdges :: Assertion
testEdges = do
  inner `sorteq` expinner'
  outer `sorteq` expouter'
  where
    (outer, inner) = edges cs (`elem` cs)
    {-
        ###
        # #
        ###
      -}
    cs :: [C]
    cs =
      map
        fromCoord
        [(0, 0), (1, 0), (2, 0), (0, 1), (2, 1), (0, 2), (1, 2), (2, 2)]
    expouter =
      [ ((0, 0), (0, 1)),
        ((0, 1), (0, 2)),
        ((0, 2), (0, 3)),
        ((0, 3), (1, 3)),
        ((1, 3), (2, 3)),
        ((2, 3), (3, 3)),
        ((3, 3), (3, 2)),
        ((3, 2), (3, 1)),
        ((3, 1), (3, 0)),
        ((3, 0), (2, 0)),
        ((2, 0), (1, 0)),
        ((1, 0), (0, 0)),
        ((1, 1), (2, 1)),
        ((2, 1), (2, 2)),
        ((2, 2), (1, 2)),
        ((1, 2), (1, 1))
      ]
    expouter' = map (uncurry edge' . fromCoord2) expouter
    expinner =
      [ ((0, 1), (1, 1)),
        ((0, 2), (1, 2)),
        ((1, 0), (1, 1)),
        ((2, 0), (2, 1)),
        ((1, 3), (1, 2)),
        ((2, 3), (2, 2)),
        ((3, 1), (2, 1)),
        ((3, 2), (2, 2))
      ]
    expinner' = map (uncurry edge . fromCoord2) expinner
    fromCoord2 (p, q) = (fromCoord p, fromCoord q)

testLoops :: Assertion
testLoops = loops es @=? Just loopsexp
  where
    -- rotations of the loops would be fine
    -- inside and outside of:
    --   xxx
    --   x x
    --   xxx

    es =
      [ ((0, 0), (0, 1)),
        ((0, 1), (0, 2)),
        ((0, 2), (0, 3)),
        ((0, 3), (1, 3)),
        ((1, 3), (2, 3)),
        ((2, 3), (3, 3)),
        ((3, 3), (3, 2)),
        ((3, 2), (3, 1)),
        ((3, 1), (3, 0)),
        ((3, 0), (2, 0)),
        ((2, 0), (1, 0)),
        ((1, 0), (0, 0)),
        ((1, 1), (2, 1)),
        ((2, 1), (2, 2)),
        ((2, 2), (1, 2)),
        ((1, 2), (1, 1))
      ]
    loopsexp :: [[(Int, Int)]]
    loopsexp =
      [ [ (0, 0),
          (0, 1),
          (0, 2),
          (0, 3),
          (1, 3),
          (2, 3),
          (3, 3),
          (3, 2),
          (3, 1),
          (3, 0),
          (2, 0),
          (1, 0),
          (0, 0)
        ],
        [(1, 1), (2, 1), (2, 2), (1, 2), (1, 1)]
      ]

dataTests :: TestTree
dataTests =
  testGroup
    "Generic tests for the Data modules"
    [testCase "edges" testEdges, testCase "loops" testLoops]
