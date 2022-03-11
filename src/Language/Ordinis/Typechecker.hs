module Language.Ordinis.Typechecker where

import Control.Monad
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable
import Effectful
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.Writer.Static.Local
import Language.Ordinis.Syntax
import Language.Ordinis.Typechecker.Fresh

data MaybeLocated a = MaybeLocated
  { loc :: {-# UNPACK #-} !SourceLoc,
    val :: a
  }
  deriving stock (Show)

instance Eq a => Eq (MaybeLocated a) where
  x == y = x.val == y.val

instance Ord a => Ord (MaybeLocated a) where
  compare x y = compare x.val y.val

data SourceLoc
  = Explicit {-# UNPACK #-} !Loc
  | Primitive
  | Inferred
  deriving stock (Show, Eq, Ord)

explicitType :: Type Located -> Type MaybeLocated
explicitType = mapType \(Located l x) -> MaybeLocated (Explicit l) x

sym :: MaybeLocated ()
sym = MaybeLocated Inferred ()

freshTypeVar :: Fresh :> es => Eff es (Type MaybeLocated)
freshTypeVar = TVar . MaybeLocated Inferred <$> fresh

data TypeError
  = UnificationError
  | UnboundVariable (Located Name)
  | DuplicateTypeSignature (Located Name)
  | DuplicateTypeSynonym (Located Name)

data Constraint
  = Type MaybeLocated :=: Type MaybeLocated
  deriving stock (Eq, Ord)

type Constraints = Set Constraint

(!) :: Constraint -> Constraints
(!) = Set.singleton

type Env = Map Name (Type MaybeLocated)

type Substitution = Map Name (Type Identity)

runTypechecker :: '[Error TypeError, IOE] :>> es => Module Located -> Eff es ()
runTypechecker m = do
  typeSyns <- moduleTypeSyns m
  typeSigs <- moduleTypeSigs m
  equations <- moduleEquations m
  (_, constraints) <- runFresh
    . runWriter @Constraints
    . runReader @Env _
    $ do
      _ <- for equations \eq -> do
        inferEquation _ _
      pure _
  sub <- solve Map.empty constraints
  pure ()

moduleTypeSigs :: Error TypeError :> es => Module Located -> Eff es (Map (Located Name) (Type Located))
moduleTypeSigs = \case
  [] -> pure Map.empty
  TypeSig n _ x : xs -> do
    m <- moduleTypeSigs xs
    when (n `Map.member` m) (throwError (DuplicateTypeSignature n))
    pure (Map.insert n x m)
  _ : xs -> moduleTypeSigs xs

moduleEquations :: Error TypeError :> es => Module Located -> Eff es (Map (Located Name) (Type Located))
moduleEquations = \case
  [] -> pure Map.empty
  TypeSig n _ x : xs -> do
    m <- moduleTypeSigs xs
    when (n `Map.member` m) (throwError (DuplicateTypeSignature n))
    pure (Map.insert n x m)
  _ : xs -> moduleTypeSigs xs

moduleTypeSyns :: Error TypeError :> es => Module Located -> Eff es (Map (Located Name) (Type Located))
moduleTypeSyns = \case
  [] -> pure Map.empty
  TypeSyn _ n params _ x : xs -> do
    m <- moduleTypeSigs xs
    when (n `Map.member` m) (throwError (DuplicateTypeSynonym n))
    pure (Map.insert n x m)
  _ : xs -> moduleTypeSigs xs

{- CONSTRAINT GENERATION -}

inferEquation ::
  '[Error TypeError, Reader Env, Writer Constraints, Fresh] :>> es =>
  Maybe (Type Located) ->
  Expression Located ->
  Eff es (Type MaybeLocated)
inferEquation = _

inferExpr ::
  '[Error TypeError, Reader Env, Writer Constraints, Fresh] :>> es =>
  Maybe (Type Located) ->
  Expression Located ->
  Eff es (Type MaybeLocated)
inferExpr _ = \case
  EVar n -> asks (Map.lookup n.val) >>= note (UnboundVariable n)
  EApp f x -> do
    f' <- inferExpr Nothing f
    x' <- inferExpr Nothing x
    fx <- freshTypeVar
    tell (f' :=: TFun x' sym fx !)
    pure fx
  ELam _x _ _y -> error "Lambdas"
  ELet _ _n _ _x _ _y -> error "Let"
  ELit x -> pure case x.val of
    LString _ -> string
    LIntegral _ -> int64
    LFractional _ -> float64
  EArray _ _x _ _ -> error "Arrays"
  EList _ _x _ _ -> error "Lists"
  EMap _ _xs _ _ -> error "Maps"
  ERecord _ _xs _ _ -> error "Records"
  EVariant _ _n _ _x _ -> error "Variants"

{- CONSTRAINT SOLVING -}

solve :: Error TypeError :> es => Substitution -> Constraints -> Eff es Substitution
solve z = go z . Set.toList
  where
    go sub = \case
      [] -> pure sub
      x :=: y : constraints -> do
        sub' <- unify x y
        go (sub' <> sub) (_apply sub' <$> constraints)

unify :: Error TypeError :> es => Type MaybeLocated -> Type MaybeLocated -> Eff es Substitution
unify = _

{- PRIMITIVES -}

string :: Type MaybeLocated
string = TVar (MaybeLocated Primitive "String")

int64 :: Type MaybeLocated
int64 = TVar (MaybeLocated Primitive "Int64")

float64 :: Type MaybeLocated
float64 = TVar (MaybeLocated Primitive "Float64")

{- UTILITIES -}

note :: Error e :> es => e -> Maybe a -> Eff es a
note err = \case
  Nothing -> throwError err
  Just x -> pure x
