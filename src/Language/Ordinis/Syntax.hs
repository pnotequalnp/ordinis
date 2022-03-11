{-# LANGUAGE UndecidableInstances #-}

module Language.Ordinis.Syntax where

import Data.Kind qualified as HS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

data Token
  = TkEOF
  | TkNewLine
  | TkParenOpen
  | TkParenClose
  | TkSquareBracketOpen
  | TkSquareBracketClose
  | TkAngleBracketOpen
  | TkAngleBracketClose
  | TkBraceOpen
  | TkBraceClose
  | TkMapOpen
  | TkMapClose
  | TkListOpen
  | TkListClose
  | TkLet
  | TkIn
  | TkWhere
  | TkEquals
  | TkTypeAnnotation
  | TkForall
  | TkExists
  | TkComma
  | TkDot
  | TkArrow
  | TkType
  | TkIdentifier {id :: {-# UNPACK #-} !Text}
  | TkOperator {op :: {-# UNPACK #-} !Text}
  | TkIntegral {int :: !Integer}
  | TkFractional {frac :: {-# UNPACK #-} !Rational}
  | TkString {string :: {-# UNPACK #-} !Text}
  deriving stock (Show)

renderToken :: Token -> Text
renderToken = \case
  TkEOF -> "<EOF>"
  TkNewLine -> "\\n"
  TkParenOpen -> "("
  TkParenClose -> ")"
  TkSquareBracketOpen -> "["
  TkSquareBracketClose -> "]"
  TkAngleBracketOpen -> "〈"
  TkAngleBracketClose -> "〉"
  TkBraceOpen -> "{"
  TkBraceClose -> "}"
  TkMapOpen -> "⦃"
  TkMapClose -> "⦄"
  TkListOpen -> "〚"
  TkListClose -> "〛"
  TkLet -> "let"
  TkIn -> "in"
  TkWhere -> "where"
  TkEquals -> "="
  TkTypeAnnotation -> ":"
  TkForall -> "∀"
  TkExists -> "∃"
  TkComma -> ","
  TkDot -> "."
  TkArrow -> "->"
  TkType -> "type"
  TkIdentifier x -> x
  TkOperator x -> x
  TkIntegral x -> (T.pack . show) x
  TkFractional x -> (T.pack . show . fromRational @Double) x
  TkString x -> "\"" <> x <> "\""

type Name = Text

data Literal
  = LString Text
  | LIntegral Integer
  | LFractional Rational
  deriving stock (Show)

data Expression (f :: HS.Type -> HS.Type)
  = EVar (f Name)
  | EApp (Expression f) (Expression f)
  | ELam (f Name) (f ()) (Expression f)
  | ELet (f ()) (f Name) (f ()) (Expression f) (f ()) (Expression f)
  | ELit (f Literal)
  | EArray (f ()) [Expression f] [f ()] (f ())
  | EList (f ()) [Expression f] [f ()] (f ())
  | EMap (f ()) (Map (f Name) (f Name, f (), Expression f)) [f ()] (f ())
  | ERecord (f ()) (Map (f Name) (f Name, f (), Expression f)) [f ()] (f ())
  | EVariant (f ()) (f Name) (f ()) (Expression f) (f ())

deriving stock instance (forall a. Show a => Show (f a)) => Show (Expression f)

data Type (f :: HS.Type -> HS.Type)
  = TVar (f Name)
  | TCon (f Name)
  | TApp (Type f) (Type f)
  | TFun (Type f) (f ()) (Type f)
  | TArray (f ()) (Type f) (f ())
  | TList (f ()) (Type f) (f ())
  | TMap (f ()) (Type f) (f ()) (Type f) (f ())
  | TRow (f ()) (Map (f Name) (f Name, f (), Type f)) [f ()] (f ())
  | TRecord (f ()) (Map (f Name) (f Name, f (), Type f)) [f ()] (f ())
  | TVariant (f ()) (Map (f Name) (f Name, f (), Type f)) [f ()] (f ())
  | TForall (f ()) [f Name] (f ()) (Type f)
  | TExists (f ()) [f Name] (f ()) (Type f)

deriving stock instance (forall a. Show a => Show (f a)) => Show (Type f)

-- NOTE: This has an `Ord` constraint even though it seems like it should only need `Eq`
-- This is a GHC bug in 9.2.1, fixed in 9.2.2
deriving stock instance (forall a. Ord a => Eq (f a)) => Eq (Type f)

deriving stock instance (forall a. Ord a => Ord (f a)) => Ord (Type f)

data Declaration (f :: HS.Type -> HS.Type)
  = TypeSig (f Name) (f ()) (Type f)
  | Equation (f Name) [f Name] (f ()) (Expression f)
  | TypeSyn (f ()) (f Name) [f Name] (f ()) (Type f)

deriving stock instance (forall a. Show a => Show (f a)) => Show (Declaration f)

type Module f = [Declaration f]

data Loc = Loc
  { startLine :: {-# UNPACK #-} !Word,
    endLine :: {-# UNPACK #-} !Word,
    startCol :: {-# UNPACK #-} !Word,
    endCol :: {-# UNPACK #-} !Word
  }
  deriving stock (Show, Eq, Ord)

instance Semigroup Loc where
  Loc {startCol, startLine} <> Loc {endCol, endLine} = Loc {startCol, endCol, startLine, endLine}

data Located a = Located
  { loc :: {-# UNPACK #-} !Loc,
    val :: a
  }
  deriving stock (Functor, Show)

instance Eq a => Eq (Located a) where
  x == y = x.val == y.val

instance Ord a => Ord (Located a) where
  compare x y = compare x.val y.val

type (~>) f g = forall a. f a -> g a

mapType :: (forall a. Ord a => Ord (g a)) => f ~> g -> Type f -> Type g
mapType f = \case
  TVar n -> TVar (f n)
  TCon n -> TVar (f n)
  TApp x y -> TApp (mapType f x) (mapType f y)
  TFun x arrow y -> TFun (mapType f x) (f arrow) (mapType f y)
  TArray lb x rb -> TArray (f lb) (mapType f x) (f rb)
  TList lb x rb -> TList (f lb) (mapType f x) (f rb)
  TMap lb k comma v rb -> TMap (f lb) (mapType f k) (f comma) (mapType f v) (f rb)
  TRow lb xs commas rb -> TRow (f lb) (go xs) (f <$> commas) (f rb)
  TRecord lb xs commas rb -> TRecord (f lb) (go xs) (f <$> commas) (f rb)
  TVariant lb xs commas rb -> TVariant (f lb) (go xs) (f <$> commas) (f rb)
  TForall fa params dot x -> TForall (f fa) (f <$> params) (f dot) (mapType f x)
  TExists te params dot x -> TExists (f te) (f <$> params) (f dot) (mapType f x)
  where
    go = Map.mapKeys f . fmap \(x, y, z) -> (f x, f y, mapType f z)
