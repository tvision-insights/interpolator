module Data.Interpolation.TH where

import Prelude

import Data.Either.Validation (Validation (Success))
import Data.Profunctor.Product.Default (Default, def)
import Data.Semigroup ((<>))
import Data.Sequences (replicateM, singleton)
import Data.Traversable (for)
import Language.Haskell.TH
  ( Con (NormalC, RecC)
  , Dec (DataD, NewtypeD)
  , Info (TyConI)
  , Name
  , Q
  , Type (AppT, ConT, VarT)
  , lookupTypeName
  , mkName
  , nameBase
  , newName
  , reify
  )
import qualified Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Syntax (returnQ)

import Data.Interpolation (Interpolator (Interpolator), runInterpolator)

extractSumConstructorsAndNumFields :: Name -> Q [(Name, Int)]
extractSumConstructorsAndNumFields ty = do
  reify ty >>= \ case
    TyConI (NewtypeD _ _ _ _ c _) -> singleton <$> extractConstructor c
    TyConI (DataD _ _ _ _ cs _) -> traverse extractConstructor cs
    other -> fail $ "can't extract constructors: " <> show other
  where
    extractConstructor = \ case
      NormalC n fs -> pure (n, length fs)
      other -> fail $ "won't extract constructors: " <> show other <> " - sum types only"

-- |Make an instance of 'Default' for 'Interpolator' of an ADT. Can't do it for an arbitrary
-- Profunctor p because of partial functions. This splice is meant to be used in conjunction with
-- 'makeAdaptorAndInstance' for records as a way to project 'Default' instances down to all leaves.
--
-- @
--
--  data Foo' a b = Foo1 a | Foo2 b
--  makeInterpolatorSumInstance ''Foo'
--
-- @
--
-- @
--
--  instance (Default Interpolator a1 b1, Default Interpolator a2 b2) => Default Interpolator (Foo' a1 a2) (Foo' b1 b2) where
--    def = Interpolator $ \ case
--      Foo1 x -> Foo1 <$> runInterpolator def x
--      Foo2 x -> Foo2 <$> runInterpolator def x
--
-- @
makeInterpolatorSumInstance :: Name -> Q [Dec]
makeInterpolatorSumInstance tyName = do
  cs <- extractSumConstructorsAndNumFields tyName
  (contextConstraints, templateVars, identityVars) <- fmap (unzip3 . mconcat) $ for cs $ \ (_, i) -> replicateM i $ do
    a <- newName "a"
    b <- newName "b"
    pure ([t| Default Interpolator $(TH.varT a) $(TH.varT b) |], a, b)
  let appConstructor x y = TH.appT y (TH.varT x)
      templateType = foldr appConstructor (TH.conT tyName) templateVars
      identityType = foldr appConstructor (TH.conT tyName) identityVars
      matches = flip fmap cs $ \ (c, i) -> case i of
        0 -> TH.match (TH.conP c []) (TH.normalB [| pure $ Success $(TH.conE c) |]) []
        1 -> do
          x <- newName "x"
          TH.match (TH.conP c [TH.varP x]) (TH.normalB [| fmap $(TH.conE c) <$> runInterpolator def $(TH.varE x) |]) []
        _ -> fail $ "can only match sum constructors up to 1 argument"
  sequence $
    [ TH.instanceD
        (TH.cxt contextConstraints)
        [t| Default Interpolator $(templateType) $(identityType) |]
        [ TH.funD
            'def
            [TH.clause [] (TH.normalB [| Interpolator $(TH.lamCaseE matches) |]) []]
        ]
    ]

-- |Make a fully-polymorphic data type and type aliases for "normal" and "uninterpolated" variants.
--
-- For example:
--
-- @
--   withUninterpolatedRecord [d|
--     data Foo = Foo
--       { fooBar :: String
--       , fooBaz :: Maybe Int
--       } deriving (Eq, Show)
--     |]
-- @
--
-- Is equivalent to:
--
-- @
--   data Foo' bar baz = Foo
--     { fooBar :: bar
--     , fooBaz :: baz
--     } deriving (Eq, Show)
--   type Foo = Foo' String (Maybe Int)
--   type UninterpolatedFoo = Foo' (Uninterpolated String) (Maybe (Uninterpolated Int))
-- @
--
-- __Note:__ the trailing @|]@ of the quasi quote bracket has to be indented or a parse error will occur.
--
-- TODO: pattern match on certain Functors (e.g. Maybe) and interpolate the inner type, as seen in
-- the example above.
withUninterpolatedRecord :: Q [Dec] -> Q [Dec]
withUninterpolatedRecord qDecs = do
  decs <- qDecs
  let [DataD [] tName [] Nothing constrs deriv] = decs
      [RecC cName vars] = constrs
      primedName = mkName (nameBase tName <> "'")
      simpleName = mkName . nameBase
      toTV (n, _, _) = TH.plainTV (simpleName n)  -- TODO: strip prefix?
      tvs = toTV <$> vars
      toField (n, s, _) = (simpleName n, s, VarT (simpleName n))
      fs = toField <$> vars
      con = TH.recC (simpleName cName) (returnQ <$> fs)
      toSimpleType (_, _, t) = t
  primed <- TH.dataD (pure []) primedName tvs Nothing [con] (returnQ <$> deriv)
  normalSyn <- TH.tySynD (simpleName tName) [] $
    returnQ $ foldl (\ t v -> AppT t (toSimpleType v)) (ConT primedName) vars
  uninterp <- lookupTypeName "Uninterpolated" >>= maybe (fail "Uninterpolated not in scope") returnQ
  uninterpolatedSyn <- TH.tySynD (mkName $ "Uninterpolated" <> nameBase tName) [] $
    returnQ $ foldl (\ t v -> AppT t (AppT (ConT uninterp) (toSimpleType v))) (ConT primedName) vars
  pure [primed, normalSyn, uninterpolatedSyn]

-- TODO:
-- withUninterpolatedSum?
