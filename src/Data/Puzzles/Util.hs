module Data.Puzzles.Util
    ( paths
    , loops
    ) where

import Data.List (partition)
import Control.Monad (guard)

paths :: Eq a => [(a, a)] -> Maybe [[a]]
paths [] = Just []
paths ((a,b):segs) = do
    (p, segs') <- collect b segs
    (p', segs'') <- collect a segs'
    ps <- paths segs''
    return ((reverse p' ++ p) : ps)
  where
    collect x sgs = case withx of
        []      -> Just ([x], withoutx)
        [(y,z)] -> do
            (xs, sgs') <- collect (if x == y then z else y) withoutx
            Just (x:xs, sgs')
        _       -> Nothing
      where
        (withx, withoutx) = partition (\s -> fst s == x || snd s == x) sgs

loops :: Eq a => [(a, a)] -> Maybe [[a]]
loops segs = do
    ps <- paths segs
    mapM_ (guard . isLoop) ps
    return ps
  where
    isLoop p = head p == last p
