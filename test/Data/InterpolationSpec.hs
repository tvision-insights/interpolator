module Data.InterpolationSpec where

import Prelude

import Control.Monad.Reader (runReader)
import Data.Aeson (FromJSON, ToJSON, decode, eitherDecodeStrict', encode)
import Data.Containers (mapFromList, singletonMap)
import Data.Either (isLeft)
import Data.Either.Validation (validationToEither)
import Data.Map (Map)
import Data.Profunctor.Product.Default (Default, def)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (Gen, arbitrary, forAll, listOf, resize, suchThat, (===))

-- the module being tested
import Data.Interpolation

upToNCharacters :: Int -> Gen T.Text
upToNCharacters n = fmap T.pack . resize n . listOf . suchThat arbitrary $ \ x -> x /= '\0'

codecLaws :: (ToJSON a, FromJSON a, Eq a, Show a) => Gen a -> Spec
codecLaws gen = do
  it "decode . encode == Just" $
    forAll gen $ \x -> (decode . encode) x === Just x
  it "encode . decode == Just (for any JSON resulting from encoding)" $
    forAll gen $ \(y :: b) ->
      let json = encode y
      in (encode <$> (decode json :: Maybe b)) === Just json

newtype Bar = Bar { unBar :: Int }
  deriving (Eq, Ord, Show, FromTemplateValue, ToTemplateValue)

data Foo' a b c d e = Foo
  { fooA :: a
  , fooB :: b
  , fooC :: c
  , fooD :: d
  , fooE :: e
  } deriving (Eq, Ord, Show)
type UninterpolatedFoo = Foo' (Uninterpolated T.Text) (Uninterpolated Int) (Uninterpolated Bool) [Uninterpolated Bar] (Map Char (Uninterpolated T.Text))
type Foo = Foo' T.Text Int Bool [Bar] (Map Char T.Text)

makeAdaptorAndInstance "pFoo" ''Foo'

fooInterpolator :: Interpolator UninterpolatedFoo Foo
fooInterpolator = def

key0, key1, key2, key3, key4, key5 :: TemplateKey
key0 = TemplateKey "key0"
key1 = TemplateKey "key1"
key2 = TemplateKey "key2"
key3 = TemplateKey "key3"
key4 = TemplateKey "key4"
key5 = TemplateKey "key5"

emptyContext, defaultContext :: InterpolationContext
emptyContext = InterpolationContext mempty
defaultContext = InterpolationContext . mapFromList $
  [ (key1, (TemplateValue "asdf"))
  , (key2, (TemplateValue "1"))
  , (key3, (TemplateValue "true"))
  , (key4, (TemplateValue "2"))
  , (key5, (TemplateValue "fdsa"))
  ]

singletonContext :: ToTemplateValue a => TemplateKey -> a -> InterpolationContext
singletonContext k v = InterpolationContext . singletonMap k . toTemplateValue $ v

run :: InterpolationContext -> Interpolator a b -> a -> Either [InterpolationFailure] b
run ctx x = validationToEither . flip runReader ctx . runInterpolator x

identityLaw :: (HasCallStack, Eq a, Show a) => Interpolator (Uninterpolated a) a -> Gen a -> Spec
identityLaw interp gen = describe "Identity Law" $ do
  it "always succeeds on a literal" $
    forAll gen $ \ x ->
      run emptyContext interp (Literal x) `shouldBe` Right x

roundtripLaw :: (HasCallStack, Eq a, Show a, ToTemplateValue a) => Interpolator (Uninterpolated a) a -> Gen a -> Spec
roundtripLaw interp gen = describe "Round Trip Law" $ do
  it "can always interpolate a rendered value" $
    forAll ((,) <$> gen <*> arbitrary) $ \ (x, k) ->
      run (singletonContext k x) interp (Templated (Template k Nothing)) `shouldBe` Right x

defaultLaws :: (HasCallStack, Eq a, Show a) => Interpolator (Uninterpolated a) a -> Gen a -> Spec
defaultLaws interp gen = describe "Default Laws" $ do
  it "defaults if the value is not found" $
    forAll ((,) <$> gen <*> arbitrary) $ \ (x, k) ->
      run emptyContext interp (Templated (Template k (Just x))) `shouldBe` Right x
  it "fails if the value is not found and no default" $
    forAll arbitrary $ \ k ->
      run emptyContext interp (Templated (Template k Nothing)) `shouldBe` Left [InterpolationFailureKeyNotFound k]

interpolationLaws ::
  ( HasCallStack, Eq a, Show a
  , Default Interpolator (Uninterpolated a) a, ToTemplateValue a )
  => Gen a -> Spec
interpolationLaws gen = do
  identityLaw def gen
  roundtripLaw def gen
  defaultLaws def gen

spec :: Spec
spec = describe "InterpolationSpec" $ do
  let isLeftInt :: Either String (Uninterpolated Int) -> Bool
      isLeftInt = isLeft

      noDefaultText :: TemplateKey -> Uninterpolated T.Text
      noDefaultText k = Templated (Template k Nothing)

  describe "Foo Unit Tests" $ do
    it "interpolates variables from context" $ do
      let template = Foo
            { fooA = Templated $ Template key1 Nothing
            , fooB = Templated $ Template key2 Nothing
            , fooC = Templated $ Template key3 Nothing
            , fooD = [Templated $ Template key4 Nothing]
            , fooE = singletonMap 'a' (Templated $ Template key5 Nothing)
            }
          expected = Foo "asdf" 1 True [Bar 2] (singletonMap 'a' "fdsa")
      run defaultContext fooInterpolator template `shouldBe` Right expected
    it "fails with multiple errors" $ do
      let template = Foo
            { fooA = Templated $ Template key0 Nothing
            , fooB = Templated $ Template key1 Nothing
            , fooC = Templated $ Template key3 Nothing
            , fooD = [Templated $ Template key4 Nothing]
            , fooE = singletonMap 'a' (Templated $ Template key5 Nothing)
            }
          expected =
            [ InterpolationFailureKeyNotFound key0
            , InterpolationFailureValueNotReadable key1 (TemplateValue "asdf")
            ]
      run defaultContext fooInterpolator template `shouldBe` Left expected

  describe "Uninterpolated JSON Unit Tests" $ do
    it "parses base case" $ do
      eitherDecodeStrict' "\"_env:key1:foo\"" `shouldBe` Right (Templated (Template key1 (Just ("foo" :: T.Text))))
      eitherDecodeStrict' "\"foo\"" `shouldBe` Right (Literal ("foo" :: T.Text))
      eitherDecodeStrict' "\"_env:key1:1\"" `shouldBe` Right (Templated (Template key1 (Just (1 :: Int))))
      eitherDecodeStrict' "1" `shouldBe` Right (Literal (1 :: Int))
      eitherDecodeStrict' "\"_env:key1:true\"" `shouldBe` Right (Templated (Template key1 (Just True)))
      eitherDecodeStrict' "true" `shouldBe` Right (Literal True)

    it "parses no default" $ do
      eitherDecodeStrict' "\"_env:key1\"" `shouldBe` Right (noDefaultText key1)
      eitherDecodeStrict' "\"_env:key1:\"" `shouldBe` Right (noDefaultText key1)

    it "fails with no key" $ do
      eitherDecodeStrict' "\"_env:\"" `shouldSatisfy` isLeftInt
      eitherDecodeStrict' "\"_env::\"" `shouldSatisfy` isLeftInt
      eitherDecodeStrict' "\"_env::1\"" `shouldSatisfy` isLeftInt

    it "succeeds with colons in default value" $ do
      eitherDecodeStrict' "\"_env:key1:foo:bar\"" `shouldBe` Right (Templated (Template key1 (Just ("foo:bar" :: T.Text))))

    it "succeeds even if you are a bad person and do something wrong sometimes" $ do
      eitherDecodeStrict' "\"_env:a space\"" `shouldBe` Right (noDefaultText $ TemplateKey "a space")

  describe "Text Primitive" $ do
    describe "JSON Laws" $ do
      codecLaws (arbitrary :: Gen (Uninterpolated T.Text))
    describe "Interpolation Laws" $ do
      interpolationLaws $ upToNCharacters 50

  describe "Int Primitive" $ do
    describe "JSON Laws" $ do
      codecLaws (arbitrary :: Gen (Uninterpolated Int))
    describe "Interpolation Laws" $ do
      interpolationLaws (arbitrary :: Gen Int)

  describe "Bool Primitive" $ do
    describe "JSON Laws" $ do
      codecLaws (arbitrary :: Gen (Uninterpolated Bool))
    describe "Interpolation Laws" $ do
      interpolationLaws (arbitrary :: Gen Bool)
