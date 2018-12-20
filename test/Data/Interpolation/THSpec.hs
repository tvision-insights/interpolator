module Data.Interpolation.THSpec where

import Prelude

import Control.Monad.Reader (runReader)
import Data.Containers (mapFromList)
import Data.Either.Validation (validationToEither)
import Data.Profunctor.Product.Default (def)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Data.Text as T
import Test.Hspec (Spec, describe, it, shouldBe)

-- the modules being tested
import Data.Interpolation
import Data.Interpolation.TH

data Bar' a b = Bar
  { barA :: a
  , barB :: b
  } deriving (Eq, Ord, Show)
type UninterpolatedBar = Bar' (Uninterpolated Int) (Uninterpolated T.Text)
type Bar = Bar' Int T.Text

data Foo' a b c
  = Foo1 a
  | Foo2 b
  | Foo3 c
  | Foo4
  deriving (Eq, Ord, Show)
type UninterpolatedFoo = Foo' UninterpolatedBar (Uninterpolated Int) (Uninterpolated Bool)
type Foo = Foo' Bar Int Bool

makeAdaptorAndInstance "pBar" ''Bar'
makeInterpolatorSumInstance ''Foo'

key1, key2, key3, key4 :: TemplateKey
key1 = TemplateKey "key1"
key2 = TemplateKey "key2"
key3 = TemplateKey "key3"
key4 = TemplateKey "key4"

run :: UninterpolatedFoo -> Either [InterpolationFailure] Foo
run = validationToEither . flip runReader defaultContext . runInterpolator fooInterpolator
  where
    fooInterpolator :: Interpolator UninterpolatedFoo Foo
    fooInterpolator = def

    defaultContext :: InterpolationContext
    defaultContext = InterpolationContext . mapFromList $
      [ (key1, (TemplateValue "1"))
      , (key2, (TemplateValue "asdf"))
      , (key3, (TemplateValue "2"))
      , (key4, (TemplateValue "true"))
      ]

spec :: Spec
spec = describe "Shared.Interpolation.THSpec" $ do
  it "if it compiles, it worked" True

  it "interpolates over all branches" $ do
    run (Foo1 (Bar (Templated $ Template key1 Nothing) (Templated $ Template key2 Nothing))) `shouldBe` Right (Foo1 $ Bar 1 "asdf")
    run (Foo2 (Templated $ Template key3 Nothing)) `shouldBe` Right (Foo2 2)
    run (Foo3 (Templated $ Template key4 Nothing)) `shouldBe` Right (Foo3 True)
    run Foo4 `shouldBe` Right Foo4
