module Basic where

import Prelude

import Data.Aeson (eitherDecodeStrict')
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.ByteString (ByteString)
import Data.Profunctor.Product.Default (def)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Data.Text as T

import Data.Interpolation
import Data.Interpolation.TH

withUninterpolated [d|
  data Foo = Foo
    { a :: T.Text
    , b :: Int
    , c :: Bool
    } deriving (Eq, Ord, Show)
  |]

-- Note: these two synonyms are generated, along with the polymorphic type Foo'
-- type UninterpolatedFoo = Foo' (Uninterpolated T.Text) (Uninterpolated Int) (Uninterpolated Bool)
-- type Foo = Foo' T.Text Int Bool

deriveJSON defaultOptions ''Foo'
makeAdaptorAndInstance "pFoo" ''Foo'

fooInterpolator :: Interpolator UninterpolatedFoo Foo
fooInterpolator = def

uninterpolatedStr :: ByteString
uninterpolatedStr = "{\"a\": \"_env:foo\", \"b\": \"_env:bar\", \"c\": \"_env:baz\" }"

uninterpolated :: IO UninterpolatedFoo
uninterpolated = either fail pure $ eitherDecodeStrict' uninterpolatedStr

interpolated :: IO Foo
interpolated = either (fail . show) pure =<< interpolateWithContextExplicit fooInterpolator =<< uninterpolated
