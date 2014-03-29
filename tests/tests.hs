import Test.Tasty
import Test.Tasty.HUnit

import Data.Yaml
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)

import Control.DeepSeq

import Data.Puzzles.Read
import qualified Data.Puzzles.Grid as Grid

import Diagrams.TwoD.Puzzles.Draw
import Diagrams.Prelude
import Diagrams.Backend.SVG

import Text.Blaze.Svg.Renderer.Text (renderSvg)
import qualified Data.Text as T

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

decodeLines :: [String] -> Value
decodeLines = fromJust . decode . B.pack . unlines

packLine :: String -> Value
packLine = String . T.pack

packLines :: [String] -> Value
packLines = String . T.pack . unlines

geradeweg_1 :: Value
geradeweg_1 = packLines $
    [ ".....    "
    , ".211."
    , "..2..  "
    , "3...4"
    , "....."
    ]

geradeweg_1_sol :: Value
geradeweg_1_sol = packLines $
    [ "┌┐┌─┐"
    , "││└┐│  "
    , "│└─┘│"
    , "└──┐│"
    , "  .└┘ "
    ]

tightfit_1 :: Value
tightfit_1 = packLines $
    [ " --- "
    , "3/\\.-"
    , "-\\.\\4"
    , "-.\\/-"
    , " 35-"
    ]

tightfit_broken_1 :: Value
tightfit_broken_1 = packLines $
    [ " --- "
    , "3/\\.-"
    , "-\\x\\4"
    , "-.\\/-"
    , " 35-"
    ]

tightfit_broken_2 :: Value
tightfit_broken_2 = packLines $
    [ "3/\\.-"
    , "-\\x\\4"
    , "-.\\/-"
    , " 35-"
    ]

tightfit_1_sol :: Value
tightfit_1_sol = packLines $
    [ "2/1 4\\5  3"
    , "  4\\5  3  2\\1 "
    , "   3  1\\2 5/4"
    ]

tightfit_sol_broken :: Value
tightfit_sol_broken = packLine "2/1 4 /5"

tightfit_sol_broken_2 :: Value
tightfit_sol_broken_2 = packLine "2/x 4 3/5"

slalom_sol_broken :: Value
slalom_sol_broken = packLine "//\\ /\\x5 "

kpyramid_1 :: Value
kpyramid_1 = packLines $
    [ "G     3"
    , "G    . ."
    , "G   . . ."
    , "W  .o. . ."
    , "G 1*.*.o.*6"
    ]

kpyramid_1_sol :: Value
kpyramid_1_sol = packLines $
    [ "    3"
    , "   8 5"
    , "  1 9 4"
    , " 3 2 7 3"
    , "1 2 4 3 6"
    ]

kpyramid_broken_1 :: Value
kpyramid_broken_1 = packLines $
    [ "  G     3"
    , "  G    . 22"
    , "  H   . aa ."
    , "  W  .o. .|. "
    , "  G 1*.*.o.*6"
    ]

kpyramid_broken_2 :: Value
kpyramid_broken_2 = packLines $
    [ "G     3"
    , "G    . 22"
    , "H   . aa ."
    , "W  .o. .|. "
    , "G 1*.*.o.*6"
    ]

kpyramid_broken_3 :: Value
kpyramid_broken_3 = packLines $
    [ "G     3"
    , "G    . ."
    , "G   . . ."
    , "W  .o. . ."
    , "G a*.*.o.*6"
    ]

compass_1 :: Value
compass_1 = decodeLines $
    [ "grid: |"
    , "  ..."
    , "  a.b"
    , "clues:"
    , "  a: 2 1 . 2"
    , "  b: 21 . . 0"
    ]

compass_broken_1 :: Value
compass_broken_1 = decodeLines $
    [ "grid: |"
    , "  a.b"
    , "clues:"
    , "  b: 21 . . 0"
    ]

compass_broken_2 :: Value
compass_broken_2 = decodeLines $
    [ "grid: |"
    , "  a.b"
    , "clues:"
    , "  a: x . . 0"
    , "  b: 21 . . 0"
    ]

compass_broken_3 :: Value
compass_broken_3 = decodeLines $
    [ "grid: |"
    , "  a.b"
    , "clues:"
    , "  a: 1 . ."
    , "  b: 21 . . 0"
    ]

compass_broken_4 :: Value
compass_broken_4 = decodeLines $
    [ "grid: |"
    , "  a.b"
    , "clues:"
    , "  a: 1 . . 2 3"
    , "  b: 21 . . 0"
    ]

compass_broken_5 :: Value
compass_broken_5 = decodeLines $
    [ "grid: |"
    , "  a3b"
    , "clues:"
    , "  a: 1 . . 2 3"
    , "  b: 21 . . 0"
    ]

thermo_1 :: Value
thermo_1 = packLines $
    [ ".b..2."
    , "a.c5.1"
    , ".d..6."
    , ".4..c."
    , "5.3b.d"
    , ".2..a."
    ]

thermo_broken_1 :: Value
thermo_broken_1 = packLines $
    [ ".."
    , "a."
    ]

thermo_broken_2 :: Value
thermo_broken_2 = packLines $
    [ "bb"
    , "a."
    ]

justShow :: Show a => Maybe a -> Bool
justShow Nothing = False
justShow (Just x) = show x `deepseq` True

eitherShow :: Show a => Either e a -> Bool
eitherShow (Left _) = False
eitherShow (Right x) = show x `deepseq` True

testParse :: Show a => (Value -> Parser a) -> Value -> Assertion
testParse p t = eitherShow res @? "bad parse: " ++ err
  where
    res = parseEither p t
    err = either id (const "no error") res

testNonparse :: Show a => (Value -> Parser a) -> Value -> Assertion
testNonparse p t = (not . justShow . parseMaybe p $ t)
                   @? "parsed but shouldn't"

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
    , testCase "don't parse broken thermo" $ testNonparse (fst thermosudoku) thermo_broken_1
    , testCase "don't parse broken thermo" $ testNonparse (fst thermosudoku) thermo_broken_2
    ]
