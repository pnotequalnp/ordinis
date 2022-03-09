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

Module :: { Module Located }
  : Decl                        { [$1] }
  | Module '\n' Decl            { $3 : $1 }
  | Decl '\n'                   { [$1] }

Decl :: { Located (Declaration Located) }
  : id '=' Expr                 { undefined }
  | id ':' Type                 { undefined }

Type :: { Located (Type Located) }
  : id                          { undefined }

Expr :: { Located (Expression Located) }
  : Atom                        { $1 }

Atom :: { Located (Expression Located) }
  : '(' Expr ')'                { $2 }
  | id                          { undefined }
  | Literal                     { undefined }

Literal :: { Located (Literal Located) }
  : int                         { undefined }
  | float                       { undefined }

{
{-# ANN module "HLint: ignore" #-}

data ParseError
  = UnexpectedToken (Located Token)
  deriving stock (Show)

runParser :: '[Error ParseError, Error LexError] :>> es => L.Text -> Eff es (Module Located)
runParser source = evalState (initialLexState source) parse
}
