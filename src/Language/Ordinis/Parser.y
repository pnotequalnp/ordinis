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
%tokentype { Located Token }
%monad { '[Error ParseError, Error LexError, State LexState] :>> es } { Eff es }
%lexer { lexCont } { Located _ TEOF }
%error { (throwError . UnexpectedToken) }

%token
      id              { Located _ (TIdentifier _) }
      let             { Located _ TLet }
      in              { Located _ TIn }
      int             { Located _ (TIntegral _) }
      float           { Located _ (TFractional _) }
      ':'             { Located _ TTypeAnnotation }
      '='             { Located _ TEquals }
      '\n'            { Located _ TNewLine }
      '('             { Located _ TParenOpen }
      ')'             { Located _ TParenClose }
      '['             { Located _ TSquareBracketOpen }
      ']'             { Located _ TSquareBracketClose }
      '〈'             { Located _ TAngleBracketOpen }
      '〉'             { Located _ TAngleBracketClose }
      '{'             { Located _ TBraceOpen }
      '}'             { Located _ TBraceClose }

%%

File :: { Module Located }
  : Module                      { reverse $1 }
  | Module '\n'                 { reverse $1 }
  | '\n' Module                 { reverse $2 }
  | '\n' Module '\n'            { reverse $2 }

Module :: { Module Located }
  : Decl                        { [$1] }
  | Module '\n' Decl            { $3 : $1 }

Decl :: { Located (Declaration Located) }
  : id '=' Expr                 { mergeLocations Binding ((.id) `fmap` $1) $3 }
  | id ':' Type                 { mergeLocations TypeSig ((.id) `fmap` $1) $3 }

Type :: { Located (Type Located) }
  : id                          { (TVar . (.id)) `fmap` $1 }

Expr :: { Located (Expression Located) }
  : Atom                        { $1 }

Atom :: { Located (Expression Located) }
  : '(' Expr ')'                { $2 }
  | id                          { (EVar . (.id)) `fmap` $1 }
  | Literal                     { error "lit" }

Literal :: { Located (Literal Located) }
  : int                         { error "int" }
  | float                       { error "float" }

{
{-# ANN module "HLint: ignore" #-}

data ParseError
  = UnexpectedToken (Located Token)
  deriving stock (Show)

runParser :: '[Error ParseError, Error LexError] :>> es => L.Text -> Eff es (Module Located)
runParser source = evalState (initialLexState source) parse
}
