{
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Language.Ordinis.Parser where

import Data.Text.Lazy qualified as L
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Language.Ordinis.Lexer (LexError, LexState, initialLexState, lexCont)
import Language.Ordinis.Syntax
}

%name parse
%tokentype { Span Token }
%monad { '[Error ParseError, Error LexError, State LexState] :>> es } { Eff es }
%lexer { lexCont } { Span _ _ _ _ TEOF }
%error { (throwError . UnexpectedToken) }

%token
      id              { Span _ _ _ _ (TIdentifier _) }
      let             { Span _ _ _ _ TLet }
      in              { Span _ _ _ _ TIn }
      int             { Span _ _ _ _ (TIntegral _) }
      float           { Span _ _ _ _ (TFractional _) }
      ':'             { Span _ _ _ _ TTypeAnnotation }
      '='             { Span _ _ _ _ TEquals }
      '\n'            { Span _ _ _ _ TNewLine }
      '('             { Span _ _ _ _ TParenOpen }
      ')'             { Span _ _ _ _ TParenClose }
      '['             { Span _ _ _ _ TSquareBracketOpen }
      ']'             { Span _ _ _ _ TSquareBracketClose }
      '〈'             { Span _ _ _ _ TAngleBracketOpen }
      '〉'             { Span _ _ _ _ TAngleBracketClose }
      '{'             { Span _ _ _ _ TBraceOpen }
      '}'             { Span _ _ _ _ TBraceClose }

%%

Module :: { Module Span }
  : Decl                        { [$1] }
  | Module '\n' Decl            { $3 : $1 }
  | Decl '\n'                   { [$1] }

Decl :: { Spanned Declaration }
  : id '=' Expr                 { mergeSpans Binding (getIdentifier `fmap` $1) $3 }
  | id ':' Type                 { mergeSpans Type (getIdentifier `fmap` $1) $3 }

Type :: { Spanned Type }
  : id                          { (TVar . getIdentifier) `fmap` $1 }

Expr :: { Spanned Expression }
  : Atom                        { $1 }

Atom :: { Spanned Expression }
  : '(' Expr ')'                { $2 }
  | id                          { (EVar . getIdentifier) `fmap` $1 }
  | Literal                     { ELit `fmap` $1 }

Literal :: { Spanned Literal }
  : int                         { (LIntegral . getIntegral) `fmap` $1 }
  | float                       { (LFractional . getFractional) `fmap` $1 }

{
{-# ANN module "HLint: ignore" #-}

data ParseError
  = UnexpectedToken (Span Token)
  deriving stock (Show)

runParser :: '[Error ParseError, Error LexError] :>> es => L.Text -> Eff es (Module Span)
runParser source = evalState (initialLexState source) parse
}
