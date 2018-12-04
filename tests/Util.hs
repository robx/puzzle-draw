module Util where

import           Control.DeepSeq
import           Data.Yaml

import           Test.Tasty.HUnit

-- | Force full evaluation of a showable value.
justShow :: Show a => Maybe a -> Bool
justShow Nothing  = False
justShow (Just x) = show x `deepseq` True

-- | Force full evaluation of a showable value.
eitherShow :: Show a => Either e a -> Bool
eitherShow (Left  _) = False
eitherShow (Right x) = show x `deepseq` True

-- | Test that a value is parsed correctly, by forcing the
--   parse result to be fully evaluated.
testParse :: Show a => (Value -> Parser a) -> Value -> Assertion
testParse p t = eitherShow res @? "bad parse: " ++ err
 where
  res = parseEither p t
  err = either id (const "no error") res

-- | Test that a value is not parsed.
testNonparse :: Show a => (Value -> Parser a) -> Value -> Assertion
testNonparse p t =
  (not . justShow . parseMaybe p $ t) @? "parsed but shouldn't"
