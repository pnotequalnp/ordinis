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
%lexer { lexCont } { Located _ TkEOF }
%error { (throwError . UnexpectedToken) }

%token
      let             { Located $$ TkLet }
      in              { Located $$ TkIn }
      ':'             { Located $$ TkTypeAnnotation }
      '='             { Located $$ TkEquals }
      '\n'            { Located $$ TkNewLine }
      '('             { Located $$ TkParenOpen }
      ')'             { Located $$ TkParenClose }
      '['             { Located $$ TkSquareBracketOpen }
      ']'             { Located $$ TkSquareBracketClose }
      '〈'             { Located $$ TkAngleBracketOpen }
      '〉'             { Located $$ TkAngleBracketClose }
      '{'             { Located $$ TkBraceOpen }
      '}'             { Located $$ TkBraceClose }
      '{|'            { Located $$ TkMapOpen }
      '|}'            { Located $$ TkMapClose }
      '[|'            { Located $$ TkListOpen }
      '|]'            { Located $$ TkListClose }
      ','             { Located $$ TkComma }
      '.'             { Located $$ TkDot }
      '->'            { Located $$ TkArrow }
      '∀'             { Located $$ TkForall }
      '∃'             { Located $$ TkExists }
      ':='            { Located $$ TkAssign }
      type            { Located $$ TkType }
      id              { Located _ (TkIdentifier _) }
      deref           { Located _ (TkDeref _) }
      int             { Located _ (TkIntegral _) }
      float           { Located _ (TkFractional _) }
      string          { Located _ (TkString _) }

%%

File :: { Module Located }
  : Module                      { reverse $1 }
  | Module '\n'                 { reverse $1 }
  | '\n' Module                 { reverse $2 }
  | '\n' Module '\n'            { reverse $2 }

Module :: { Module Located }
  : Decl                        { [$1] }
  | Module '\n' Decl            { $3 : $1 }

Decl :: { Declaration Located }
  : id ':' Type                 { TypeSig (getId $1) (loc $2) $3 }
  | id Params '=' Expr          { Equation (getId $1) $2 (loc $3) $4 }
  | type id Params '=' Type     { TypeSyn (loc $1) (getId $2) $3 (loc $4) $5 }

Params :: { [Located Name] }
  : {- Empty -}                 { [] }
  | id                          { [getId $1] }
  | Params id                   { getId $2 : $1 }

Type :: { Type Located }
  : TForm                       { $1 }
  | Type '->' Type              { TFun $1 (loc $2) $3 }
  | '∀' Params '.' Type         { TForall (loc $1) $2 (loc $3) $4 }
  | '∃' Params '.' Type         { TExists (loc $1) $2 (loc $3) $4 }

TForm :: { Type Located }
  : TFact                       { $1 }

TFact :: { Type Located }
  : TFact TAtom                 { TApp $1 $2 }
  | TAtom                       { $1 }

TAtom :: { Type Located }
  : id                          { TVar (getId $1) }
  | '(' Type ')'                { $2 }
  | '[' Type ']'                { TArray (loc $1) $2 (loc $3) }
  | '[|' Type '|]'              { TList (loc $1) $2 (loc $3) }
  | '{|' Type ',' Type '|}'     { TMap (loc $1) $2 (loc $3) $4 (loc $5) }
  | '(' TAssoc ')'              { TRow (loc $1) (fst $2) (snd $2) (loc $3) }
  | '{' TAssoc '}'              { TRecord (loc $1) (fst $2) (snd $2) (loc $3) }
  | '〈' TAssoc '〉'              { TVariant (loc $1) (fst $2) (snd $2) (loc $3) }

TAssoc :: { (Map (Located Name) (Located Name, Located (), Type Located), [Located ()]) }
  : {- Empty -}                 { (Map.empty, []) }
  | id ':' Type                 { let l = getId $1 in (Map.singleton l (l, loc $2, $3), []) }
  | TAssoc ',' id ':' Type      {% let l = getId $3; (m, commas) = $1
                                    in fmap (, loc $2 : commas) (insertAssoc m l (l, loc $4, $5)) }

Expr :: { Expression Located }
  : Form                        { $1 }
  | let id '=' Expr in Expr     { ELet (loc $1) (getId $2) (loc $3) $4 (loc $5) $6 }

Form :: { Expression Located }
  : Fact                        { $1 }

Fact :: { Expression Located }
  : Fact Atom                   { EApp $1 $2 }
  | Atom                        { $1 }

Atom :: { Expression Located }
  : id                          { EVar (getId $1) }
  | deref                       { EDeref (getId $1) }
  | Literal                     { ELit $1 }
  | '(' Expr ')'                { $2 }
  | '[' Exprs ']'               { EArray (loc $1) (fst $2) (snd $2) (loc $3) }
  | '[|' Exprs '|]'             { EList (loc $1) (fst $2) (snd $2) (loc $3) }
  | '{|' Assoc '|}'             { EMap (loc $1) (fst $2) (snd $2) (loc $3) }
  | '{' Assoc '}'               { ERecord (loc $1) (fst $2) (snd $2) (loc $3) }
  | '〈' Assoc '〉'               {% case Map.toList (fst $2) of
                                     [] -> throwError (EmptyVariant ($1 <> $3))
                                     [(_, (l, eq, x))] -> pure (EVariant (loc $1) l eq x (loc $3))
                                     _ -> throwError (MultipleVariant ($1 <> $3)) }

Assoc :: { (Map (Located Name) (Located Name, Located (), Expression Located), [Located ()]) }
  : {- Empty -}                 { (Map.empty, []) }
  | id '=' Expr                 { let l = getId $1 in (Map.singleton l (l, loc $2, $3), []) }
  | Assoc ',' id '=' Expr       {% let l = getId $3; (m, commas) = $1
                                    in fmap (, loc $2 : commas) (insertAssoc m l (l, loc $4, $5)) }

Exprs :: { ([Expression Located], [Located ()]) }
  : {- Empty -}                 { ([], []) }
  | Expr                        { ([$1], []) }
  | Exprs ',' Expr              { let (xs, commas) = $1 in ($3 : xs, loc $2 : commas) }

Literal :: { Located Literal }
  : int                         { (LIntegral . (.int)) `fmap` $1 }
  | float                       { (LFractional . (.frac)) `fmap` $1 }
  | string                      { (LString . (.string)) `fmap` $1 }

{
{-# ANN module "HLint: ignore" #-}

data ParseError
  = UnexpectedToken (Located Token)
  | DuplicateLabel (Located Name)
  | EmptyVariant Loc
  | MultipleVariant Loc
  deriving stock (Show, Eq)

getId :: Located Token -> Located Name
getId = fmap (.id)

loc :: Loc -> Located ()
loc l = Located l ()

insertAssoc :: Error ParseError :> es
            => Map (Located Name) a
            -> Located Name
            -> a
            -> Eff es (Map (Located Name) a)
insertAssoc m l x = case Map.insertLookupWithKey (\_ _ -> id) l x m of
  (Just _, _) -> throwError (DuplicateLabel l)
  (Nothing, m') -> pure m'

runParser :: '[Error ParseError, Error LexError] :>> es => L.Text -> Eff es (Module Located)
runParser source = evalState (initialLexState source) parse
}
