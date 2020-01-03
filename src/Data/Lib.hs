module Data.Lib
  ( mapLeft,
    impossible,
    invertMap,
  )
where

import Data.List
  ( groupBy,
    sortOn,
  )
import qualified Data.Map.Strict as Map

mapLeft :: (a -> b) -> Either a x -> Either b x
mapLeft f e = case e of
  Left l -> Left $ f l
  Right r -> Right r

invertMap :: (Eq a, Ord a) => Map.Map k a -> Map.Map a [k]
invertMap =
  Map.fromList
    . map (\l -> (fst (head l), map snd l))
    . groupBy (\x y -> fst x == fst y)
    . sortOn fst
    . map (\(x, y) -> (y, x))
    . Map.toList

impossible :: a
impossible = error "impossible"
