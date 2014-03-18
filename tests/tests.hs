import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text, pack)
import Data.Yaml

import Data.Puzzles.ReadPuzzle (geradeweg', tightfitskyscrapers')

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

justShowLength :: Show a => Maybe a -> Int
justShowLength = maybe (-1) (length . show)

checkParse :: Show a => Int -> (Value -> Parser a) -> Text -> (Bool, String)
checkParse n p t = (l >= n, "bad parse, show length " ++ show l ++ " < " ++ show n)
  where
    l = justShowLength (parseMaybe p (String t))
testParse n p t = b @? e
  where
    (b, e) = checkParse n p t

testNonparse p t = l == (-1) @? "parsed but shouldn't, length " ++ show l
  where
    l = justShowLength (parseMaybe p (String t))

unitTests = testGroup "Unit tests"
    [ testCase "parse geradeweg" $ testParse 442 (fst geradeweg') geradeweg_1
    , testCase "parse geradeweg solution" $ testParse 221 (snd geradeweg') geradeweg_1_sol
    , testCase "parse tightfit" $ testParse 263 (fst tightfitskyscrapers') tightfit_1
    , testCase "parse tightfit solution" $ testParse 169 (snd tightfitskyscrapers') tightfit_1_sol
    , testCase "don't parse broken tightfit solution" $
        testNonparse (snd tightfitskyscrapers') tightfit_sol_broken
    , testCase "don't parse broken tightfit solution" $
        testNonparse (snd tightfitskyscrapers') tightfit_sol_broken_2
    ]
