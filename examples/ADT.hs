module ADT where

import Prelude

import Data.Aeson (FromJSON, Value (Object), eitherDecodeStrict', parseJSON, withObject, (.:))
import Data.ByteString (ByteString)
import Data.Profunctor.Product.Default (def)
import Data.Semigroup ((<>))
import qualified Data.Text as T

import Basic (Foo, UninterpolatedFoo)
import Data.Interpolation
import Data.Interpolation.TH

withUninterpolated [d|
  data Bar
    = Bar1 Foo
    | Bar2 Int
    | Bar3
    deriving (Eq, Ord, Show)
  |]
-- Note, these are generated:
-- type UninterpolatedBar = Bar' UninterpolatedFoo (Uninterpolated Int)
-- type Bar = Bar' Foo Int

makeInterpolatorSumInstance ''Bar'

instance (FromJSON a, FromJSON b) => FromJSON (Bar' a b) where
  parseJSON = withObject "Bar" $ \ obj ->
    obj .: "type" >>= \ case
      "1" -> Bar1 <$> parseJSON (Object obj)
      "2" -> Bar2 <$> parseJSON (Object obj)
      "3" -> pure Bar3
      other -> fail $ "Not a valid bar: " <> T.unpack other

barInterpolator :: Interpolator UninterpolatedBar Bar
barInterpolator = def

uninterpolatedStr :: ByteString
uninterpolatedStr = "{\"type\": \"1\", \"a\": \"_env:foo\", \"b\": \"_env:bar\", \"c\": \"_env:baz\" }"

uninterpolated :: IO UninterpolatedBar
uninterpolated = either fail pure $ eitherDecodeStrict' uninterpolatedStr

interpolated :: IO Bar
interpolated = either (fail . show) pure =<< interpolateWithContextExplicit barInterpolator =<< uninterpolated
