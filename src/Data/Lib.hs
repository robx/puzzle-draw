module Data.Lib
  ( mapLeft
  )
where

mapLeft :: (a -> b) -> Either a x -> Either b x
mapLeft f e = case e of
  Left  l -> Left $ f l
  Right r -> Right r

