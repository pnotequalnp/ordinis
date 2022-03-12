{-# LANGUAGE OverloadedLists #-}

module Language.Ordinis.Typechecker where

import Control.Monad
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map, (\\))
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.Writer.Static.Local
import Language.Ordinis.Syntax
import Language.Ordinis.Typechecker.Names

data SourceLocated a = SourceLocated
  { loc :: {-# UNPACK #-} !SourceLoc,
    val :: a
  }
  deriving stock (Show)

instance Eq a => Eq (SourceLocated a) where
  x == y = x.val == y.val

instance Ord a => Ord (SourceLocated a) where
  compare x y = compare x.val y.val

data SourceLoc
  = Explicit (Type Located)
  | Literal (Located Literal)
  | Inferred (Expression Located)
  | ImplicitTopLevel (Located Name)
  | ExplicitParameter (Expression Located)
  | InferredParameter (Located Name)
  deriving stock (Show, Eq, Ord)

explicitType :: Type Located -> Type SourceLocated
explicitType t = mapType (\(Located _ x) -> SourceLocated (Explicit t) x) t

sym :: Expression Located -> SourceLocated ()
sym x = SourceLocated (Inferred x) ()

inferredTypeVar :: Names :> es => Expression Located -> Eff es (Type SourceLocated)
inferredTypeVar x = TVar . SourceLocated (Inferred x) <$> newName

data TypeError
  = UnificationError (Type SourceLocated) (Type SourceLocated)
  | UnboundVariable (Located Name)
  | DuplicateTypeSignature (Located Name)
  | DuplicateTypeSynonym (Located Name)
  | ExtraParameters (Located Name)
  | MismatchedParamCounts (Located Name)
  | LoneTypeSignature (Located Name)

data Constraint
  = Type SourceLocated :=: Type SourceLocated
  deriving stock (Eq, Ord)

type Constraints = Set Constraint

type Env = Map Name (Type SourceLocated)

newtype Substitution = Substitute (Map Name (Type Identity))
  deriving newtype (Monoid)

instance Semigroup Substitution where
  s@(Substitute x) <> Substitute y = Substitute ((apply s <$> y) <> x)

class Substitutable a where
  apply :: Substitution -> a -> a

type Equations = NonEmpty ([Located Name], Expression Located)

runTypechecker :: Error TypeError :> es => Module Located -> Eff es ()
runTypechecker decls = runNames sequentialNames do
  typeSigs <- moduleTypeSigs decls
  equations <- moduleEquations decls
  definitions <- groupEquations typeSigs equations
  (_, constraints) <-
    runWriter @Constraints
      . runReader @Env Map.empty
      $ traverse (uncurry4 inferDefinition) definitions
  _ <- solve mempty constraints
  pure ()

-- | Collect type signatures from a module, ensuring there aren't duplicates.
moduleTypeSigs :: Error TypeError :> es => Module Located -> Eff es (Map (Located Name) (Type Located))
moduleTypeSigs = \case
  [] -> pure Map.empty
  TypeSig name _ t : decls -> do
    sigs <- moduleTypeSigs decls
    when (name `Map.member` sigs) (throwError (DuplicateTypeSignature name))
    pure (Map.insert name t sigs)
  _ : decls -> moduleTypeSigs decls

-- | Collect equations from a module, grouped by name.
moduleEquations :: Error TypeError :> es => Module Located -> Eff es (Map (Located Name) Equations)
moduleEquations = \case
  [] -> pure Map.empty
  Equation name params _ body : decls -> do
    eqs <- moduleEquations decls
    case Map.lookup name eqs of
      Just ((params', _) :| _) | length params /= length params' -> throwError (MismatchedParamCounts name)
      _ -> pure ()
    pure (Map.insertWith (<>) name [(params, body)] eqs)
  _ : decls -> moduleEquations decls

-- | Group equations together with their type signature, if it exists.
groupEquations ::
  '[Error TypeError, Names] :>> es =>
  Map (Located Name) (Type Located) ->
  Map (Located Name) Equations ->
  Eff es (Map (Located Name) ([Located Name] -> Type SourceLocated, [Located Name] -> [Type SourceLocated], Type SourceLocated, Equations))
groupEquations sigs eqs = do
  defs <- Map.traverseWithKey go eqs
  case Map.toList (sigs \\ defs) of
    [] -> pure ()
    (unmatched, _) : _ -> throwError (LoneTypeSignature unmatched)
  pure defs
  where
    go name eqs' = do
      let (params, _) :| _ = eqs' -- Previously checked that all parameter lists are the same length
      (defType, paramTypes, bodyType) <- case Map.lookup name sigs of
        Nothing -> do
          bodyType <- TVar . SourceLocated (ImplicitTopLevel name) <$> newName
          let paramType n = TVar . SourceLocated (InferredParameter n) <$> newName
          paramTypes <- zipWith ($) <$> replicateM (length params) (sequenceA paramType)
          let sym' = SourceLocated (ImplicitTopLevel name) ()
              defType = foldr (\t -> TFun t sym') bodyType
          pure (defType . paramTypes, paramTypes, bodyType)
        Just defType -> do
          (paramTypes, bodyType) <- explicitParamTypes name (length params) defType
          pure (const (explicitType defType), const paramTypes, bodyType)
      pure (defType, paramTypes, bodyType, eqs')

explicitParamTypes ::
  Error TypeError :> es =>
  Located Name ->
  Int ->
  Type Located ->
  Eff es ([Type SourceLocated], Type SourceLocated)
explicitParamTypes name n (TFun paramType _ sig) = do
  (paramTypes, bodyType) <- explicitParamTypes name (n - 1) sig
  pure (explicitType paramType : paramTypes, bodyType)
explicitParamTypes _ 0 bodyType = pure ([], explicitType bodyType)
explicitParamTypes name _ _ = throwError (ExtraParameters name)

{- CONSTRAINT GENERATION -}

inferDefinition ::
  '[Error TypeError, Reader Env, Writer Constraints, Names] :>> es =>
  ([Located Name] -> Type SourceLocated) ->
  ([Located Name] -> [Type SourceLocated]) ->
  Type SourceLocated ->
  Equations ->
  Eff es (Type SourceLocated)
inferDefinition defType paramTypes bodyType eqs = do
  eqType :| eqTypes <- traverse (inferEquation defType paramTypes bodyType) eqs
  tell (foldr (Set.insert . (:=: eqType)) Set.empty eqTypes)
  pure eqType

inferEquation ::
  '[Error TypeError, Reader Env, Writer Constraints, Names] :>> es =>
  ([Located Name] -> Type SourceLocated) ->
  ([Located Name] -> [Type SourceLocated]) ->
  Type SourceLocated ->
  ([Located Name], Expression Located) ->
  Eff es (Type SourceLocated)
inferEquation defTypeF paramTypesF bodyType (params, body) = do
  bodyType' <- local @Env (<> paramEnv) (inferExpr body)
  tell @Constraints [bodyType :=: bodyType']
  pure defType
  where
    defType = defTypeF params
    paramTypes = paramTypesF params
    paramEnv = Map.fromList (zip ((.val) <$> params) paramTypes)

inferExpr ::
  '[Error TypeError, Reader Env, Writer Constraints, Names] :>> es =>
  Expression Located ->
  Eff es (Type SourceLocated)
inferExpr = \case
  EVar name -> asks (Map.lookup name.val) >>= note (UnboundVariable name)
  fx@(EApp f x) -> do
    fType <- inferExpr f
    xType <- inferExpr x
    fxType <- inferredTypeVar fx
    tell @Constraints [fType :=: TFun xType (sym fx) fxType]
    pure fxType
  ELam _x _ _y -> error "Lambdas"
  ELet _ _n _ _x _ _y -> error "Let"
  ELit x -> pure case x.val of
    LString _ -> string x
    LIntegral _ -> int64 x
    LFractional _ -> float64 x
  EArray _ _x _ _ -> error "Arrays"
  EList _ _x _ _ -> error "Lists"
  EMap _ _xs _ _ -> error "Maps"
  ERecord _ _xs _ _ -> error "Records"
  EVariant _ _n _ _x _ -> error "Variants"
  EDeref _n -> error "Deref"

{- CONSTRAINT SOLVING -}

solve :: Error TypeError :> es => Substitution -> Constraints -> Eff es Substitution
solve z = go z . Set.toList
  where
    go sub = \case
      [] -> pure sub
      x :=: y : constraints -> do
        sub' <- unify x y
        go (sub' <> sub) (apply sub' <$> constraints)

unify :: Error TypeError :> es => Type SourceLocated -> Type SourceLocated -> Eff es Substitution
unify = _

{- LITERALS -}

string :: Located Literal -> Type SourceLocated
string lit = TVar (SourceLocated (Literal lit) "String")

int64 :: Located Literal -> Type SourceLocated
int64 lit = TVar (SourceLocated (Literal lit) "Int64")

float64 :: Located Literal -> Type SourceLocated
float64 lit = TVar (SourceLocated (Literal lit) "Float64")

{- UTILITIES -}

note :: Error e :> es => e -> Maybe a -> Eff es a
note err = \case
  Nothing -> throwError err
  Just x -> pure x

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (w, x, y, z) = f w x y z
