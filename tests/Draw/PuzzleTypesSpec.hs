module Draw.PuzzleTypesSpec where

import qualified Data.ByteString.Lazy as LBS
import Data.GridShape
import qualified Data.Map.Strict as Map
import Diagrams.Prelude (mkSizeSpec2D)
import Draw.CmdLine
  ( Format (SVG),
    renderBytesSVG,
  )
import qualified Draw.Draw as Draw
import qualified Draw.Font as Font
import Draw.PuzzleTypes (colorakari)
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldSatisfy,
  )

spec :: Spec
spec = do
  describe "colorakari" $ do
    it "doesn't crash on blanks in the grid" $ do
      let sz = mkSizeSpec2D (Just 1.0) (Just 1.0)
          cfg = Draw.Config Draw.Screen Font.fontAnelizaRegular Font.fontBit
          g =
            Map.fromList
              [((C 1 1), Just 'R'), ((C 1 2), Just ' '), ((C 2 2), Nothing)]
          d = Draw.diagram cfg $ Draw.puzzle colorakari $ g
          bs = renderBytesSVG SVG sz d
      LBS.length bs `shouldSatisfy` (\l -> l > 0)
