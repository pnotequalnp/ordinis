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
      '.'             { Located _ TDot }
      '->'            { Located _ TArrow }
      '∀'             { Located _ TForall }
      '∃'             { Located _ TExists }
      type            { Located _ TType }
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
  | type id '=' Type            { Located (sconcat [$1.loc, $2.loc, $3.loc, $4.loc])
                                    (TypeSyn ((.id) `fmap` $2) $4) }

Type :: { Located (Type Located) }
  : TAtom                       { $1 }
  | TAtom TAtom                 { mergeLocations TApp $1 $2 }

TAtom :: { Located (Type Located) }
  : id                          { (TVar . (.id)) `fmap` $1 }
  | '(' Row ')'                 { TRow `fmap` $2 }
  | '{' Row '}'                 { TRecord `fmap` $2 }
  | '〈' Row '〉'                 { TVariant `fmap` $2 }
  | '(' Type ')'                { $2 }
  | '∀' id '.' Type             { error "forall" }

Row :: { Located (Map (Located Name) (Located (Type Located))) }
  : id ':' Type                 { mergeLocations Map.singleton ((.id) `fmap` $1) $3 }
  | Row ',' id ':' Type         {% fmap (Located (sconcat [$1.loc, $2.loc, $3.loc, $4.loc, $5.loc]))
                                     (insertMapUnique $1.val ((.id) `fmap` $3) $5) }

Expr :: { Located (Expression Located) }
  : Form                        { $1 }
  | let id '=' Expr in Expr     { Located (sconcat [$1.loc, $2.loc, $3.loc, $4.loc, $5.loc, $6.loc])
                                    (ELet ((.id) `fmap` $2) $4 $6) }

Form :: { Located (Expression Located) }
  : Fact                        { $1 }

Fact :: { Located (Expression Located) }
  : Fact Atom                   { mergeLocations EApp $1 $2 }
  | Atom                        { $1 }

Atom :: { Located (Expression Located) }
  : '(' Expr ')'                { $2 }
  | id                          { (EVar . (.id)) `fmap` $1 }
  | Literal                     { ELit `fmap` $1 }

Literal :: { Located (Literal Located) }
  : int                         { (LIntegral . (.int)) `fmap` $1 }
  | float                       { (LFractional . (.frac)) `fmap` $1 }
  | str                         { (LString . (.string)) `fmap` $1 }
  | '{' Entry '}'               { LRecord `fmap` $2 }
  | '〈' id '=' Expr '〉'         { mergeLocations LVariant ((.id) `fmap` $2) $4 }

Entry :: { Located (Map (Located Name) (Located (Expression Located))) }
  : id '=' Expr                 { mergeLocations Map.singleton ((.id) `fmap` $1) $3 }
  | Entry ',' id '=' Expr       {% fmap (Located (sconcat [$1.loc, $2.loc, $3.loc, $4.loc, $5.loc]))
                                     (insertMapUnique $1.val ((.id) `fmap` $3) $5) }

{
{-# ANN module "HLint: ignore" #-}

data ParseError
  = UnexpectedToken (Located Token)
  | DuplicateLabel (Located Name)
  deriving stock (Show)

sconcat :: Semigroup a => [a] -> a
sconcat = \case
  [] -> error "sconcat: empty list"
  [x] -> x
  x : xs -> x <> sconcat xs

insertMapUnique :: Error ParseError :> es
                => Map (Located Name) (Located (f Located))
                -> Located Name
                -> Located (f Located)
                -> Eff es (Map (Located Name) (Located (f Located)))
insertMapUnique m l v = case Map.insertLookupWithKey (\_ _ -> id) l v m of
  (Just _, _) -> throwError (DuplicateLabel l)
  (Nothing, m') -> pure m'

runParser :: '[Error ParseError, Error LexError] :>> es => L.Text -> Eff es (Module Located)
runParser source = evalState (initialLexState source) parse
}
