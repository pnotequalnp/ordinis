{
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoOverloadedStrings #-}

module Language.Ordinis.Parser where

import Data.Map (Map)
import Data.Map qualified as Map
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
      let             { Located _ TLet }
      in              { Located _ TIn }
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
      ','             { Located _ TComma }
      id              { Located _ (TIdentifier _) }
      int             { Located _ (TIntegral _) }
      float           { Located _ (TFractional _) }
      str             { Located _ (TString _) }

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
  : TAtom                       { $1 }
  | TAtom TAtom                 { mergeLocations TApp $1 $2 }

TAtom :: { Located (Type Located) }
  : id                          { (TVar . (.id)) `fmap` $1 }
  | '(' Row ')'                 { TRow `fmap` $2 }
  | '{' Row '}'                 { TRecord `fmap` $2 }
  | '〈' Row '〉'                 { TVariant `fmap` $2 }
  | '(' Type ')'                { $2 }

Row :: { Located (Map (Located Name) (Located (Type Located))) }
  : {- empty -}                 { error "empty row" }
  | id ':' Type                 { mergeLocations Map.singleton ((.id) `fmap` $1) $3 }
  | Row ',' id ':' Type         {% let l = (.id) `fmap` $3
                                    in case Map.insertLookupWithKey (\_ _ -> id) l $5 $1.val of
                                        (Just _, _) -> throwError (DuplicateLabel l)
                                        (Nothing, m) -> pure (Located ($5.loc <> $1.loc <> $3.loc) m) }

Expr :: { Located (Expression Located) }
  : Atom                        { $1 }

Atom :: { Located (Expression Located) }
  : '(' Expr ')'                { $2 }
  | id                          { (EVar . (.id)) `fmap` $1 }
  | Literal                     { ELit `fmap` $1 }

Entry :: { Located (Map (Located Name) (Located (Expression Located))) }
  : {- empty -}                 { error "empty entry" }
  | id '=' Expr                 { mergeLocations Map.singleton ((.id) `fmap` $1) $3 }
  | Entry ',' id '=' Expr       {% let l = (.id) `fmap` $3
                                    in case Map.insertLookupWithKey (\_ _ -> id) l $5 $1.val of
                                        (Just _, _) -> throwError (DuplicateLabel l)
                                        (Nothing, m) -> pure (Located ($5.loc <> $1.loc <> $3.loc) m) }

Literal :: { Located (Literal Located) }
  : int                         { (LIntegral . (.int)) `fmap` $1 }
  | float                       { error "float" }
  | str                         { (LString . (.string)) `fmap` $1 }
  | '{' Entry '}'               { LRecord `fmap` $2 }
  | '〈' id '=' Expr '〉'         { mergeLocations LVariant ((.id) `fmap` $2) $4 }

{
{-# ANN module "HLint: ignore" #-}

data ParseError
  = UnexpectedToken (Located Token)
  | DuplicateLabel (Located Name)
  deriving stock (Show)

runParser :: '[Error ParseError, Error LexError] :>> es => L.Text -> Eff es (Module Located)
runParser source = evalState (initialLexState source) parse
}
