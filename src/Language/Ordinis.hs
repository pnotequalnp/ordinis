module Language.Ordinis
  ( LexError (..),
    ParseError (..),
    Span (..),
    runLexer,
    runParser,
  )
where

import Language.Ordinis.Lexer (LexError (..), runLexer)
import Language.Ordinis.Parser (ParseError (..), runParser)
import Language.Ordinis.Syntax (Span (..))
