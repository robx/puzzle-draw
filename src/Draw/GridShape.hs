module Draw.GridShape where

import qualified Data.AffineSpace as AS
import Data.GridShape
import Diagrams.Prelude hiding
  ( E,
    N,
    dot,
    offset,
    outer,
    size,
    unit,
  )
import Draw.Lib

(.--.) :: AS.AffineSpace p => p -> p -> AS.Diff p
(.--.) = (AS..-.)

class ToPoint a where
  toPoint :: a -> P2 Double

instance ToPoint C where
  toPoint c = p2 (1 / 2, 1 / 2) .+^ r2i (c .--. C 0 0)

instance ToPoint N where
  toPoint c = origin .+^ r2i (c .--. N 0 0)

instance ToPoint ShiftC where
  toPoint (ShiftC c@(C _x y)) = toPoint c .+^ 0.5 * fromIntegral y *^ unitX
