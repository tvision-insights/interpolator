module Data.Interpolation.THSpec where

import Prelude

import Control.Monad.Reader (runReader)
import Data.Containers (mapFromList)
import Data.Either.Validation (validationToEither)
import Data.Profunctor.Product.Default (def)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text, toUpper)
import Test.Hspec (Spec, describe, it, shouldBe)

-- the modules being tested
import Data.Interpolation
import Data.Interpolation.TH

-- |A simple newtype with a derived instance for 'FromTemplateValue'; that instance will be in scope
-- when the type is referred to later in this file.
withUninterpolated [d|
  newtype BarName = BarName { unBarName :: Text }
    deriving (Eq, Ord, Show, FromTemplateValue, ToTemplateValue)
  |]

-- |A simple non-interpolatable type (no 'FromTemplateValue' instance.)
data Quux
  = QuuxFuzzy
  | QuuxSmooth
  deriving (Eq, Ord, Show)

withUninterpolated [d|
  data Bar = Bar
    { barA :: BarName
      -- ^Because BarName has 'FromTemplateValue', it will be 'Uninterpolated BarName' in the generated type.
    , barB :: Maybe Int
      -- ^No instance for 'Maybe Int', so this becomes 'Maybe (Uninterpolated Int)'.
    , barC :: [Bool]
      -- ^Similarly, this becomes '[Uninterpolated Bool]'.
    , barD :: Quux
      -- ^No 'FromTemplateValue' instance at all, so this is left as just 'Quux'.
    } deriving (Eq, Ord, Show)
  |]

-- |A type that will have its FromTemplateValue instance defined manually. For demonstration
-- purposes, this is just a string that is upper-cased when it is interpolated from a template.
newtype Upcased = Upcased { unUpcased :: Text }
  deriving (Eq, Show)

-- |Note: want the uninterpolated type for this sum to account for the late-defined
-- `FromTemplateValue Upcased` instance, so derive only the polymorphic type now:
withPolymorphic [d|
  data Foo
    = FooBar Bar
    | FooInt Int
    | FooBool Bool
    | FooUpcased Upcased
    | FooNone
    deriving (Eq, Ord, Show)
  |]

makeAdaptorAndInstance "pBar" ''Bar'
makeInterpolatorSumInstance ''Foo'

instance FromTemplateValue Upcased where
  parseTemplateValue = Just . Upcased . toUpper . unTemplateValue

-- Finally, derive the uninterpolated type for Foo, with the instance for Upcased defined.
deriveUninterpolated ''Foo

-- --
-- -- Another newtype, this time with a custom FromTemplateValue instance which is defined after the
-- -- type, and a record type that uses it.
-- --

-- newtype BazName = BazName { unBazName :: Text }
--   deriving (Eq, Ord, Show)

-- withPolymorphic [d|
--   data Baz = Baz
--     { bazName :: BazName
--     } deriving (Eq, Ord, Show)
--   |]

key1, key2, key3, key4, key5 :: TemplateKey
key1 = TemplateKey "key1"
key2 = TemplateKey "key2"
key3 = TemplateKey "key3"
key4 = TemplateKey "key4"
key5 = TemplateKey "key5"

run :: UninterpolatedFoo -> Either [InterpolationFailure] Foo
run = validationToEither . flip runReader defaultContext . runInterpolator fooInterpolator
  where
    fooInterpolator :: Interpolator UninterpolatedFoo Foo
    fooInterpolator = def

    defaultContext :: InterpolationContext
    defaultContext = InterpolationContext . mapFromList $
      [ (key1, TemplateValue "asdf")
      , (key2, TemplateValue "1")
      , (key3, TemplateValue "2")
      , (key4, TemplateValue "true")
      , (key5, TemplateValue "Wow")
      ]

spec :: Spec
spec = describe "Shared.Interpolation.THSpec" $ do
  it "if it compiles, it worked" True

  it "interpolates over all branches" $ do
    run (FooBar (Bar (Templated $ Template key1 Nothing) (Just . Templated $ Template key2 Nothing) [] QuuxFuzzy))
      `shouldBe` Right (FooBar $ Bar (BarName "asdf") (Just 1) [] QuuxFuzzy)
    run (FooInt (Templated $ Template key3 Nothing)) `shouldBe` Right (FooInt 2)
    run (FooBool (Templated $ Template key4 Nothing)) `shouldBe` Right (FooBool True)
    run (FooUpcased (Templated $ Template key5 Nothing)) `shouldBe` Right (FooUpcased (Upcased "WOW"))
    run FooNone `shouldBe` Right FooNone
