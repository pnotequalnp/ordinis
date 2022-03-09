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
  | TIdentifier {getIdentifier :: {-# UNPACK #-} !Text}
  | TOperator {getOperator :: {-# UNPACK #-} !Text}
  | TIntegral {getIntegral :: !Integer}
  | TFractional {getFractional :: {-# UNPACK #-} !Rational}
  | TString {getString :: {-# UNPACK #-} !Text}
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

data Loc = Loc
  { startLine :: {-# UNPACK #-} !Word,
    endLine :: {-# UNPACK #-} !Word,
    startCol :: {-# UNPACK #-} !Word,
    endCol :: {-# UNPACK #-} !Word
  }
  deriving stock (Show)

instance Semigroup Loc where
  Loc {startCol, startLine} <> Loc {endCol, endLine} = Loc {startCol, endCol, startLine, endLine}

data Located a = Located
  { loc :: {-# UNPACK #-} !Loc,
    val :: a
  }
  deriving stock (Functor, Show)

-- {-# ANN liftSpan2 ("HLint: ignore Redundant lambda" :: String) #-}
-- {-# INLINE liftSpan2 #-}
-- liftSpan2 :: (a -> b -> c) -> Span a -> Span b -> Span c
-- liftSpan2 f = \Span {startCol, startLine, value = x} Span {endLine, endCol, value = y} ->
--   Span {startCol, startLine, endCol, endLine, value = f x y}

-- {-# ANN mergeSpans ("HLint: ignore Redundant lambda" :: String) #-}
-- {-# INLINE mergeSpans #-}
-- mergeSpans :: (Span a -> Span b -> c) -> Span a -> Span b -> Span c
-- mergeSpans f = \s1@Span {startCol, startLine} s2@Span {endLine, endCol} ->
--   Span {startCol, startLine, endCol, endLine, value = f s1 s2}

-- {-# ANN onSpans ("HLint: ignore Redundant lambda" :: String) #-}
-- {-# INLINE onSpans #-}
-- onSpans :: (a -> b -> c) -> Span a -> Span b -> c
-- onSpans f = \Span {value = x} Span {value = y} -> f x y
