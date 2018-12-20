module Data.Interpolation.TH where

import Prelude

import Data.Either.Validation (Validation (Success))
import Data.Profunctor.Product.Default (Default, def)
import Data.Semigroup ((<>))
import Data.Sequences (replicateM, singleton)
import Data.Traversable (for)
import Language.Haskell.TH
  (Con (NormalC), Dec (DataD, NewtypeD), Info (TyConI), Name, Q, newName, reify)
import qualified Language.Haskell.TH.Lib as TH

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
