module Util where

import Control.DeepSeq
import Data.Yaml

import Test.Tasty.HUnit

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
