{-# LANGUAGE UndecidableInstances #-}

module Language.Ordinis.Syntax where

import Data.Kind qualified as HS
import Data.Map (Map)
import Data.Text (Text)
import Data.Text qualified as T

data Token
  = TEOF
  | TNewLine
  | TParenOpen
  | TParenClose
  | TSquareBracketOpen
  | TSquareBracketClose
  | TAngleBracketOpen
  | TAngleBracketClose
  | TBraceOpen
  | TBraceClose
  | TLet
  | TIn
  | TWhere
  | TEquals
  | TTypeAnnotation
  | TForall
  | TComma
  | TDot
  | TIdentifier {id :: {-# UNPACK #-} !Text}
  | TOperator {op :: {-# UNPACK #-} !Text}
  | TIntegral {int :: !Integer}
  | TFractional {frac :: {-# UNPACK #-} !Rational}
  | TString {string :: {-# UNPACK #-} !Text}
  deriving stock (Show)

renderToken :: Token -> Text
renderToken = \case
  TEOF -> "<EOF>"
  TNewLine -> "\\n"
  TParenOpen -> "("
  TParenClose -> ")"
  TSquareBracketOpen -> "["
  TSquareBracketClose -> "]"
  TAngleBracketOpen -> "〈"
  TAngleBracketClose -> "〉"
  TBraceOpen -> "{"
  TBraceClose -> "}"
  TLet -> "let"
  TIn -> "in"
  TWhere -> "where"
  TEquals -> "="
  TTypeAnnotation -> ":"
  TForall -> "∀"
  TComma -> ","
  TDot -> "."
  TIdentifier x -> x
  TOperator x -> x
  TIntegral x -> (T.pack . show) x
  TFractional x -> (T.pack . show . fromRational @Double) x
  TString x -> "\"" <> x <> "\""

type Name = Text

data Literal (f :: HS.Type -> HS.Type)
  = LString Text
  | LIntegral Integer
  | LFractional Rational
  | LSequence [f (Expression f)]
  | LRecord (Map (f Name) (f (Expression f)))
  | LVariant (f Name) (f (Expression f))

deriving stock instance (Show (f Name), Show (f (Expression f))) => Show (Literal f)

data Expression (f :: HS.Type -> HS.Type)
  = EVar Name
  | EApp (f (Expression f)) (f (Expression f))
  | ELam (f Name) (f (Expression f))
  | ELet (f Name) (f (Expression f)) (f (Expression f))
  | ELit (Literal f)

deriving stock instance (Show (f Name), Show (f (Expression f))) => Show (Expression f)

data Type (f :: HS.Type -> HS.Type)
  = TVar Name
  | TCon Name
  | TApp (f (Type f)) (f (Type f))
  | TFun (f (Type f)) (f (Type f))
  | TRow (Map (f Name) (f (Type f)))
  | TRecord (Map (f Name) (f (Type f)))
  | TVariant (Map (f Name) (f (Type f)))

deriving stock instance (Show (f Name), Show (f (Type f))) => Show (Type f)

data Declaration (f :: HS.Type -> HS.Type)
  = Binding (f Name) (f (Expression f))
  | TypeSig (f Name) (f (Type f))

deriving stock instance (Show (f Name), Show (f (Expression f)), Show (f (Type f))) => Show (Declaration f)

type Module f = [f (Declaration f)]

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

{-# ANN mergeLocations ("HLint: ignore Redundant lambda" :: String) #-}
{-# INLINE mergeLocations #-}
mergeLocations :: (Located a -> Located b -> c) -> Located a -> Located b -> Located c
mergeLocations f = \x y -> Located (x.loc <> y.loc) (f x y)

locationAp :: (Located a -> Located b -> c) -> Located a -> Located b -> Located c
locationAp f x y = Located (x.loc <> y.loc) (f x y)
