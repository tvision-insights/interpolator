module Data.Interpolation where

import Prelude hiding (lookup)

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (Reader, asks, runReader)
import Data.Aeson (FromJSON, ToJSON, Value (String), parseJSON, toJSON, withText)
import Data.Char (isAlphaNum)
import Data.Containers (mapFromList, setFromList, setToList)
import Data.Either.Validation (Validation (Failure, Success), validationToEither)
import Data.Map (Map, lookup)
import Data.Profunctor (Profunctor, dimap)
import Data.Profunctor.Product (ProductProfunctor, SumProfunctor, purePP, (****), (+++!))
import Data.Profunctor.Product.Default (Default, def)
import Data.Semigroup ((<>))
import Data.Sequences (isPrefixOf)
import Data.Set (Set)
import qualified Data.Text as T
import System.Environment (getEnvironment)
<<<<<<< HEAD
import Test.QuickCheck
  (Arbitrary, Arbitrary1, Gen, arbitrary, frequency, liftArbitrary, listOf1, oneof, suchThat)
=======
import Test.QuickCheck (Arbitrary, Arbitrary1, arbitrary, arbitrary1, liftArbitrary, listOf1, oneof, suchThat)
>>>>>>> upstream/master
import Text.Read (readMaybe)

-- |Newtype wrapper for an environment variable key.
newtype TemplateKey = TemplateKey { unTemplateKey :: T.Text }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- |Newtype wrapper for an environment variable value.
newtype TemplateValue = TemplateValue { unTemplateValue :: T.Text }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- |Type for a value that is described by '_env:ENVIRONMENT_VARIABLE:default' in JSON.
data Template a = Template
  { _templateKey     :: TemplateKey
  , _templateDefault :: Maybe a
  }
  deriving (Eq, Ord, Show)

-- |Type for a value that can be described either with '_env...' or as just a literal value in JSON.
data Uninterpolated a
  = Templated (Template a)
  | Literal a
  deriving (Eq, Ord, Show)

data InterpolationFailure
  = InterpolationFailureKeyNotFound TemplateKey
  | InterpolationFailureValueNotReadable TemplateKey TemplateValue
  deriving (Eq, Ord)

instance Show InterpolationFailure where
  show = \ case
    InterpolationFailureKeyNotFound (TemplateKey k) -> "Interpolation key " <> show k <> " not found"
    InterpolationFailureValueNotReadable (TemplateKey k) (TemplateValue v) -> "Value " <> show v <> " for interpolation key " <> show k <> " not readable"

newtype InterpolationContext = InterpolationContext { unInterpolationContext :: Map TemplateKey TemplateValue }

-- |A class for parsing environment variable values, which should only be defined on primitives.
-- Similar to 'Read' except that for text-type values it should parse using identity.
class FromTemplateValue a where
  parseTemplateValue :: TemplateValue -> Maybe a

instance FromTemplateValue T.Text where
  parseTemplateValue = Just . unTemplateValue

instance FromTemplateValue String where
  parseTemplateValue = Just . T.unpack . unTemplateValue

instance FromTemplateValue Int where
  parseTemplateValue = readMaybe . T.unpack . unTemplateValue

instance FromTemplateValue Bool where
  parseTemplateValue x = case T.toLower (unTemplateValue x) of
    "true"  -> Just True
    "false" -> Just False
    _       -> Nothing

-- |A class for showing environment variable values, which should only be defined on primitives.
-- Similar to 'Show' except that for text-type values it should use identity.
class ToTemplateValue a where
  toTemplateValue :: a -> TemplateValue

instance ToTemplateValue T.Text where
  toTemplateValue = TemplateValue

instance ToTemplateValue String where
  toTemplateValue = TemplateValue . T.pack

instance ToTemplateValue Int where
  toTemplateValue = TemplateValue . T.pack . show

instance ToTemplateValue Bool where
  toTemplateValue = TemplateValue . T.toLower . T.pack . show

newtype Interpolator templates identities = Interpolator
  { runInterpolator :: templates -> Reader InterpolationContext (Validation [InterpolationFailure] identities)
  }

instance Functor (Interpolator templates) where
  fmap f (Interpolator g) = Interpolator $ fmap (fmap (fmap f)) g

instance Applicative (Interpolator templates) where
  pure x = Interpolator $ \ _ -> pure $ Success x
  Interpolator f <*> Interpolator g = Interpolator $ \ x -> do
    f' <- f x
    y <- g x
    pure $ f' <*> y

instance Profunctor Interpolator where
  dimap f g (Interpolator h) = Interpolator (dimap f (fmap (fmap g)) h)

instance ProductProfunctor Interpolator where
  purePP = pure
  (****) = (<*>)

instance SumProfunctor Interpolator where
  Interpolator f +++! Interpolator g = Interpolator $ \ case
    Left x -> fmap Left <$> f x
    Right y -> fmap Right <$> g y

-- |Run a template using the interpolation context and failing if the value is not found or not readable.
runTemplate :: FromTemplateValue a => Interpolator (Uninterpolated a) a
runTemplate = Interpolator $ \ case
  Literal d -> pure $ Success d
  Templated (Template k dMay) -> asks (lookup k . unInterpolationContext) >>= pure . \ case
    Just v -> maybe (Failure [InterpolationFailureValueNotReadable k v]) Success $ parseTemplateValue v
    Nothing -> maybe (Failure [InterpolationFailureKeyNotFound k]) Success dMay

mkInterpolationContext :: MonadIO m => m InterpolationContext
mkInterpolationContext = InterpolationContext . mapFromList . map toTuple <$> liftIO getEnvironment
  where
    toTuple (x, y) = (TemplateKey $ T.pack x, TemplateValue $ T.pack y)

interpolateWithContext :: (Default Interpolator templates identities, MonadIO m)
  => templates -> m (Either [InterpolationFailure] identities)
interpolateWithContext = interpolateWithContextExplicit def

interpolateWithContextExplicit :: MonadIO m
  => Interpolator templates identities -> templates -> m (Either [InterpolationFailure] identities)
interpolateWithContextExplicit interpolator x = do
  ctx <- mkInterpolationContext
  pure . validationToEither . flip runReader ctx . runInterpolator interpolator $ x

-- |Pure transformation for the identity interpolation. FIXME this is too clunky for overlapping
-- instances, define an auxiliary class (or type) for IdentityInterpolation.
instance {-# OVERLAPPABLE #-} Default Interpolator a a where
  def = Interpolator $ pure . Success

-- |When we can parse template values, we can interpolate from the template.
instance FromTemplateValue a => Default Interpolator (Uninterpolated a) a where
  def = runTemplate

instance Default Interpolator a b => Default Interpolator (Map k a) (Map k b) where
  def = Interpolator $ fmap sequenceA . traverse (runInterpolator def)

instance (Default Interpolator a b, Ord a, Ord b) => Default Interpolator (Set a) (Set b) where
  def = Interpolator $ fmap (fmap setFromList . sequenceA) . traverse (runInterpolator def) . setToList

instance Default Interpolator a b => Default Interpolator [a] [b] where
  def = Interpolator $ fmap sequenceA . traverse (runInterpolator def)

instance Default Interpolator a b => Default Interpolator (Maybe a) (Maybe b) where
  def = Interpolator $ fmap sequenceA . traverse (runInterpolator def)

instance FromTemplateValue a => FromJSON (Template a) where
  parseJSON jv = flip (withText "Template") jv $ \ t ->
    case T.splitOn ":" t of
      "_env":k:vs -> do
        let v = T.intercalate ":" vs
            defaultV = parseTemplateValue =<< if T.null v then Nothing else Just (TemplateValue v)
        case T.null k of
          False -> pure $ Template (TemplateKey k) defaultV
          True  -> fail $ "Not a template: " <> T.unpack t
      _ -> fail $ "Not a template: " <> T.unpack t

instance ToTemplateValue a => ToJSON (Template a) where
  toJSON (Template (TemplateKey k) (Just x)) = String $ "_env:" <> k <> ":" <> unTemplateValue (toTemplateValue x)
  toJSON (Template (TemplateKey k) Nothing) = String $ "_env:" <> k

instance (FromTemplateValue a, FromJSON a) => FromJSON (Uninterpolated a) where
  parseJSON jv = (Templated <$> parseJSON jv) <|> (Literal <$> parseJSON jv)

instance (ToTemplateValue a, ToJSON a) => ToJSON (Uninterpolated a) where
  toJSON = \ case
    Templated x -> toJSON x
    Literal x -> toJSON x

instance Arbitrary TemplateKey where
  arbitrary = TemplateKey <$> varNameAllowed
    where varNameAllowed = fmap T.pack . listOf1 $ arbitrary `suchThat` (\c -> isAlphaNum c || c == '_')

instance Arbitrary1 Uninterpolated where
  liftArbitrary g = oneof
    [ Literal <$> g
    , Templated <$> (Template <$> arbitrary <*> liftArbitrary g)
    ]

instance {-# OVERLAPPABLE #-} Arbitrary a => Arbitrary (Uninterpolated a) where
  arbitrary = arbitrary1

instance {-# OVERLAPPING #-} Arbitrary (Uninterpolated T.Text) where
  arbitrary = liftArbitrary noEnv
    where noEnv = fmap T.pack $ arbitrary `suchThat` (\ s -> not ("_env:" `isPrefixOf` s) && not (null s))

