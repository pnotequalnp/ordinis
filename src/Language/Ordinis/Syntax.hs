{-# LANGUAGE UndecidableInstances #-}

module Language.Ordinis.Syntax where

import Data.Kind qualified as HS
import Data.Map (Map)
import Data.Text (Text)

type family I a where
  I a = a

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
  | TIdentifier {getIdentifier :: {-# UNPACK #-} !Text}
  | TOperator {getOperator :: {-# UNPACK #-} !Text}
  | TIntegral {getIntegral :: !Integer}
  | TFractional {getFractional :: {-# UNPACK #-} !Rational}
  | TString {getString :: {-# UNPACK #-} !Text}
  deriving stock (Show)

type Name = Text

data Literal (f :: HS.Type -> HS.Type)
  = LString Text
  | LIntegral Integer
  | LFractional Rational
  | LSequence [f (Expression f)]
  | LRecord (Map (f Name) (f (Expression f)))

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
  | TFun (f (Type f)) (f (Type f))

deriving stock instance (Show (f Name), Show (f (Type f))) => Show (Type f)

data Declaration (f :: HS.Type -> HS.Type)
  = Binding (f Name) (f (Expression f))
  | Type (f Name) (f (Type f))

deriving stock instance (Show (f Name), Show (f (Expression f)), Show (f (Type f))) => Show (Declaration f)

type Module f = [f (Declaration f)]

data Span a = Span
  { startCol :: {-# UNPACK #-} !Word,
    endCol :: {-# UNPACK #-} !Word,
    startLine :: {-# UNPACK #-} !Word,
    endLine :: {-# UNPACK #-} !Word,
    value :: a
  }
  deriving stock (Show, Functor)

type Spanned f = Span (f Span)

{-# ANN liftSpan2 ("HLint: ignore Redundant lambda" :: String) #-}
{-# INLINE liftSpan2 #-}
liftSpan2 :: (a -> b -> c) -> Span a -> Span b -> Span c
liftSpan2 f = \Span {startCol, startLine, value = x} Span {endLine, endCol, value = y} ->
  Span {startCol, startLine, endCol, endLine, value = f x y}

{-# ANN mergeSpans ("HLint: ignore Redundant lambda" :: String) #-}
{-# INLINE mergeSpans #-}
mergeSpans :: (Span a -> Span b -> c) -> Span a -> Span b -> Span c
mergeSpans f = \s1@Span {startCol, startLine} s2@Span {endLine, endCol} ->
  Span {startCol, startLine, endCol, endLine, value = f s1 s2}

{-# ANN onSpans ("HLint: ignore Redundant lambda" :: String) #-}
{-# INLINE onSpans #-}
onSpans :: (a -> b -> c) -> Span a -> Span b -> c
onSpans f = \Span {value = x} Span {value = y} -> f x y
