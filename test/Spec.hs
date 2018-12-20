import Prelude

import Test.Hspec (hspec)

import qualified Data.InterpolationSpec
import qualified Data.Interpolation.THSpec

main :: IO ()
main = hspec $ do
  Data.InterpolationSpec.spec
  Data.Interpolation.THSpec.spec
