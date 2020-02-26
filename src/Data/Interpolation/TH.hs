module Data.Interpolation.TH where

import Control.Monad (foldM)
import Data.Char (toLower)
import Data.Either.Validation (Validation (Success))
import Data.Profunctor.Product.Default (Default, def)
import Data.Semigroup ((<>))
import Data.Sequences (catMaybes, replicateM, singleton, stripPrefix)
import Data.Traversable (for)
import Language.Haskell.TH
  ( Con (NormalC, RecC)
  , Dec (DataD, NewtypeD, TySynD)
  , Info (TyConI)
  , Name
  , Q
  , Type (AppT, ConT, ListT, VarT)
  , lookupTypeName
  , mkName
  , nameBase
  , newName
  , pprint
  , reify
  , reportError
  )
import qualified Language.Haskell.TH.Lib as TH
import Language.Haskell.TH.Syntax (returnQ)
import Prelude

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

-- |When applied to a simple data type declaration, substitute a fully-polymorphic data type
-- and type aliases for "normal" and "uninterpolated" variants.
--
-- For example, a record or newtype (using record syntax):
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
-- Whenever the type of a field is itself an application, 'Data.Interpolation.Uninterpolated' is
-- applied to the /inner/ type, which is probably what you want for 'Maybe' (as in the example),
-- @[]@, and so on, but won't work in other cases where there's a
-- 'Data.Interpolation.FromTemplateValue' instance for some non-trivial user type.
withUninterpolated :: Q [Dec] -> Q [Dec]
withUninterpolated qDecs = do
  decs <- qDecs
  uninterp <- lookupTypeName "Uninterpolated" >>= maybe (fail "Uninterpolated not in scope") returnQ
  case decs of
    -- "data" with a single record constructor:
    [DataD [] tName [] Nothing [RecC cName fields] deriv] -> do
      let con = TH.recC (simpleName cName) (returnQ <$> (fieldToPolyField <$> fields))
      primedDecl <- TH.dataD (pure []) (primedName tName) (fieldToTypeVar <$> fields) Nothing [con] (returnQ <$> deriv)
      normalSyn <- TH.tySynD (simpleName tName) [] $
        returnQ $ foldl (\ t v -> AppT t (fieldToSimpleType v)) (ConT (primedName tName)) fields
      uninterpolatedSyn <- TH.tySynD (mkName $ "Uninterpolated" <> nameBase tName) [] $
        foldM (\ t v -> AppT t <$> (mapAppT (AppT (ConT uninterp)) (fieldToSimpleType v))) (ConT (primedName tName)) fields
      pure [primedDecl, normalSyn, uninterpolatedSyn]

    -- "newtype" with a single record constructor:
    [NewtypeD [] tName [] Nothing (RecC cName [field]) deriv] -> do
      -- TODO: use the type name, lower-cased, instead of the field name, for the type var?
      let con = TH.recC (simpleName cName) (returnQ <$> [fieldToPolyField field])
      primedDecl <- TH.newtypeD (pure []) (primedName tName) [fieldToTypeVar field] Nothing con (returnQ <$> deriv)
      normalSyn <- TH.tySynD (simpleName tName) [] $
        returnQ $ AppT (ConT (primedName tName)) (fieldToSimpleType field)
      uninterpolatedSyn <- TH.tySynD (mkName $ "Uninterpolated" <> nameBase tName) [] $
        (AppT (ConT (primedName tName)) <$> (mapAppT (AppT (ConT uninterp)) (fieldToSimpleType field)))
      pure [primedDecl, normalSyn, uninterpolatedSyn]

    -- "data" with multiple simple constructors:
    [DataD [] tName [] Nothing constrs deriv] -> do
      let mapConstr = \ case
            NormalC cName [(s, t)] ->
              let vName = niceName tName cName
              in pure (Just (TH.plainTV vName), NormalC cName [(s, VarT vName)], Just t)
            NormalC cName [] ->
              pure (Nothing, NormalC cName [], Nothing)
            other -> fail $ "Can't handle constructor: " <> pprint other

      (vars, constrs', ts) <- unzip3 <$> traverse mapConstr constrs
      primedDecl <- TH.dataD (pure []) (primedName tName) (catMaybes vars) Nothing (returnQ <$> constrs') (returnQ <$> deriv)
      normalSyn <- TH.tySynD (simpleName tName) [] $
        returnQ $ foldl AppT (ConT (primedName tName)) (catMaybes ts)
      uninterpolatedSyn <- TH.tySynD (mkName $ "Uninterpolated" <> nameBase tName) [] $
        foldM (\ t v -> AppT t <$> (mapAppT (AppT (ConT uninterp)) v)) (ConT (primedName tName)) (catMaybes ts)

      -- reportError $ pprint uninterpolatedSyn
      pure [primedDecl, normalSyn, uninterpolatedSyn]

    _ -> do
      reportError $ "Can't handle declaration: " <> pprint decs
      reportError $ "AST: " <> show decs -- HACK
      pure []

  where
    primedName n = mkName (nameBase n <> "'")
    simpleName = mkName . nameBase

    fieldToTypeVar (n, _, _) = TH.plainTV (simpleName n)  -- TODO: strip prefix?
    fieldToPolyField (n, s, _) = (simpleName n, s, VarT (simpleName n))
    fieldToSimpleType (_, _, t) = t

    niceName prefix = mkName . unCap . (\ s -> maybe s id (stripPrefix (nameBase prefix) s)) . nameBase
    unCap = \ case
      c : cs -> toLower c : cs
      other -> other

    -- Apply a type constructor, pushing it inside an outer type application, and into every type
    -- parameter, even in the presence of type synonyms (such as those generated here.)
    -- Note: we might want map only under certain constructors, (e.g. Maybe, []).
    mapAppT :: (Type -> Type) -> Type -> Q Type
    mapAppT f = \ case
      ConT n -> do
        info <- reify n
        case info of
          TyConI (DataD _ _ _ _ _ _) -> pure (f (ConT n))
          TyConI (NewtypeD _ _ _ _ _ _) -> pure (f (ConT n))
          TyConI (TySynD _ [] t) -> mapAppT f t
          other -> do
            reportError $ "Can't handle constructor: " <> pprint other
            pure $ ConT n
      AppT (ConT n1) t2 -> AppT (ConT n1) <$> mapAppT f t2
      AppT t1 t2 -> AppT <$> mapAppT f t1 <*> mapAppT f t2
      ListT -> pure ListT
      other -> do
        reportError $ "Can't handle type: " <> pprint other
        pure other
