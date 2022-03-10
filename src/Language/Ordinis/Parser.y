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
      let             { Located $$ TLet }
      in              { Located $$ TIn }
      ':'             { Located $$ TTypeAnnotation }
      '='             { Located $$ TEquals }
      '\n'            { Located $$ TNewLine }
      '('             { Located $$ TParenOpen }
      ')'             { Located $$ TParenClose }
      '['             { Located $$ TSquareBracketOpen }
      ']'             { Located $$ TSquareBracketClose }
      '〈'             { Located $$ TAngleBracketOpen }
      '〉'             { Located $$ TAngleBracketClose }
      '{'             { Located $$ TBraceOpen }
      '}'             { Located $$ TBraceClose }
      ','             { Located $$ TComma }
      '.'             { Located $$ TDot }
      '->'            { Located $$ TArrow }
      '∀'             { Located $$ TForall }
      '∃'             { Located $$ TExists }
      type            { Located $$ TType }
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
  : id ':' Type                 { loc [$1.loc, $2, $3.loc] (TypeSig ((.id) `fmap` $1) $3) }
  | id Params '=' Expr          { loc ([$1.loc, $3, $4.loc] <> (.loc) `fmap` $2)
                                    (Equation ((.id) `fmap` $1) $2 $4) }
  | type id Params '=' Type     { loc ([$1, $2.loc, $4, $5.loc] <> (.loc) `fmap` $3)
                                    (TypeSyn ((.id) `fmap` $2) $3 $5) }

Params :: { [Located Name] }
  : {- Empty -}                 { [] }
  | id                          { [(.id) `fmap` $1] }
  | Params id                   { (.id) `fmap` $2 : $1 }

Type :: { Located (Type Located) }
  : TForm                       { $1 }
  | Type '->' Type              { loc [$1.loc, $2, $3.loc] (TFun $1 $3) }
  | '∀' Params '.' Type         { error "forall" }
  | '∃' Params '.' Type         { error "exists" }

TForm :: { Located (Type Located) }
  : TFact                       { $1 }

TFact :: { Located (Type Located) }
  : TFact TAtom                 { loc [$1.loc, $2.loc] (TApp $1 $2) }
  | TAtom                       { $1 }

TAtom :: { Located (Type Located) }
  : id                          { (TVar . (.id)) `fmap` $1 }
  | Row                         { $1 }
  | TRecord                     { $1 }
  | TVariant                    { $1 }

Row :: { Located (Type Located) }
  : '(' ')'                     { error "empty row" }
  | '(' TAssoc ')'              { error "row" }

TRecord :: { Located (Type Located) }
  : '{' '}'                     { error "empty record" }
  | '{' TAssoc '}'              { error "record" }

TVariant :: { Located (Type Located) }
  : '〈' '〉'                     { error "empty variant" }
  | '〈' TAssoc '〉'              { error "variant" }

TAssoc :: { Located (Map (Located Name) (Located (Type Located))) }
  : id ':' Type                 { loc [$1.loc, $2, $3.loc] (Map.singleton ((.id) `fmap` $1) $3) }
  | TAssoc ',' id ':' Type      {% fmap (loc [$1.loc, $2, $3.loc, $4, $5.loc])
                                     (insertMapUnique $1.val ((.id) `fmap` $3) $5) }

Expr :: { Located (Expression Located) }
  : Form                        { $1 }
  | let id '=' Expr in Expr     { loc [$1, $2.loc, $3, $4.loc, $5, $6.loc]
                                    (ELet ((.id) `fmap` $2) $4 $6) }

Form :: { Located (Expression Located) }
  : Fact                        { $1 }

Fact :: { Located (Expression Located) }
  : Fact Atom                   { loc [$1.loc, $2.loc] (EApp $1 $2) }
  | Atom                        { $1 }

Atom :: { Located (Expression Located) }
  : '(' Expr ')'                { $2 }
  | id                          { (EVar . (.id)) `fmap` $1 }
  | Literal                     { ELit `fmap` $1 }

Literal :: { Located (Literal Located) }
  : int                         { (LIntegral . (.int)) `fmap` $1 }
  | float                       { (LFractional . (.frac)) `fmap` $1 }
  | str                         { (LString . (.string)) `fmap` $1 }
  | Record                      { $1 }
  | Variant                     { $1 }

Record :: { Located (Literal Located) }
  : '{' '}'                     { loc [$1, $2] (LRecord Map.empty) }
  | '{' Assoc '}'               { loc ([$1, $3] <> []) (LRecord $2.val) }

Variant :: { Located (Literal Located) }
  : '〈' '〉'                     {% throwError (EmptyVariant ($1 <> $2))}
  | '〈' Assoc '〉'               {% let l = sconcat [$1, $2.loc, $3]
                                    in case onlyMap $2.val of
                                         EmptyMap -> throwError (EmptyVariant l) -- impossible
                                         ManyMap -> throwError (MultipleVariant l)
                                         OnlyMap k v -> pure (Located l (LVariant k v)) }

Assoc :: { Located (Map (Located Name) (Located (Expression Located))) }
  : id '=' Expr                 { loc [$1.loc, $2, $3.loc] (Map.singleton ((.id) `fmap` $1) $3) }
  | Assoc ',' id '=' Expr       {% fmap (loc [$1.loc, $2, $3.loc, $4, $5.loc])
                                     (insertMapUnique $1.val ((.id) `fmap` $3) $5) }

{
{-# ANN module "HLint: ignore" #-}

data ParseError
  = UnexpectedToken (Located Token)
  | DuplicateLabel (Located Name)
  | EmptyVariant Loc
  | MultipleVariant Loc
  deriving stock (Show)

sconcat :: Semigroup a => [a] -> a
sconcat = \case
  [] -> error "sconcat: empty list"
  [x] -> x
  x : xs -> x <> sconcat xs

loc :: [Loc] -> a -> Located a
loc ls x = Located (sconcat ls) x

data MapResult k v
  = EmptyMap
  | OnlyMap k v
  | ManyMap

onlyMap :: Map k v -> MapResult k v
onlyMap = Map.foldrWithKey go EmptyMap
  where
    go k v EmptyMap = OnlyMap k v
    go _ _ (OnlyMap _ _) = ManyMap
    go _ _ ManyMap = ManyMap

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
