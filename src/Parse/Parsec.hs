-- | Parsec helper for puzzle file parsing.
module Parse.Parsec (
    toParser
  , toStringParser
  , fraction
  ) where

import Text.ParserCombinators.Parsec hiding ((<|>))
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import Control.Applicative

import Data.Elements

toParser :: GenParser a () b -> [a] -> Yaml.Parser b
toParser p v = case parse p "(unknown)" v of Left e  -> fail (show e)
                                             Right x -> pure x

toStringParser :: GenParser Char () b -> Yaml.Value -> Yaml.Parser b
toStringParser p v = case v of
    Yaml.String t -> toParser p (T.unpack t)
    _             -> fail "expected string"

-- | fraction is meant to parse things like "1 1/2", "3/10", "7".
fraction :: GenParser Char st Fraction
fraction = do
    a <- many1 digit <* spaces
    FComp a <$> many1 digit <*>  (char '/' *> many1 digit)
      <|> FFrac a <$> (char '/' *> many1 digit)
      <|> pure (FInt a)
