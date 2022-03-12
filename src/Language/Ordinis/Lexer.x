{
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.Ordinis.Lexer
  ( LexError (..),
    LexState,
    initialLexState,
    lexCont,
    lexList,
    runLexer
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.ByteString.Lazy qualified as LBS
import Data.Char (ord)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Ratio ((%))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Encoding qualified as L
import Data.Word (Word8)
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import Language.Ordinis.Syntax (Token (..), Located (..), Loc (..))

}

$digit = 0-9
$subdigit = [₀-₉]
$alpha = [a-z A-Z α-ω Α-Ω]
$nl = \n
$whitespace = $white # $nl

@identifier = $alpha [$alpha $digit $subdigit \_ \']*

:-
  "--" .*               ;
  $whitespace+          ;
  $nl+                  { lexeme . const TkNewLine }
  \(                    { lexeme . const TkParenOpen }
  \)                    { lexeme . const TkParenClose }
  \{                    { lexeme . const TkBraceOpen }
  \}                    { lexeme . const TkBraceClose }
  〈                     { lexeme . const TkAngleBracketOpen }
  〉                     { lexeme . const TkAngleBracketClose }
  ⦃                     { lexeme . const TkMapOpen }
  "{|"                  { lexeme . const TkMapOpen }
  ⦄                     { lexeme . const TkMapClose }
  "|}"                  { lexeme . const TkMapClose }
  〚                    { lexeme . const TkListOpen }
  "[|"                  { lexeme . const TkListOpen }
  〛                    { lexeme . const TkListClose }
  "|]"                  { lexeme . const TkListClose }
  \.                    { lexeme . const TkDot }
  \,                    { lexeme . const TkComma }
  "->"                  { lexeme . const TkArrow }
  :                     { lexeme . const TkTypeAnnotation }
  =                     { lexeme . const TkEquals }
  :=                    { lexeme . const TkAssign }
  let                   { lexeme . const TkLet }
  in                    { lexeme . const TkIn }
  ∀                     { lexeme . const TkForall }
  ∃                     { lexeme . const TkExists }
  type                  { lexeme . const TkType }
  $digit+               { lexeme . TkIntegral . readIntegralUnsafe }
  \- $digit+            { lexeme . TkIntegral . negate . readIntegralUnsafe . T.drop 1 }
  $digit+ \. $digit*    { lexeme . TkFractional . readFractionalUnsafe }
  @identifier           { lexeme . TkIdentifier }
  \* @identifier        { lexeme . TkDeref }
  \" (. # $nl)* \"      { lexeme . TkString . T.dropEnd 1 . T.drop 1 }

{
{-# ANN module "HLint: ignore" #-}

lexeme :: State LexState :> es => Token -> Eff es (Located Token)
lexeme t = do
  LexPosition {line = startLine, column = startCol} <- getStart
  LexPosition {line = endLine, column = endCol} <- gets @LexState (.position)
  modify \s -> s {startPos = Nothing}
  pure (Located Loc {startLine, startCol, endLine, endCol} t)
  where
    getStart = get @LexState <&> \s -> case s.startPos of
      Nothing -> error ("Lexer matched 0-width lexeme. This is a bug.\n" <> show s)
      Just x -> x

readIntegralUnsafe :: Text -> Integer
readIntegralUnsafe = T.foldl' add 0
  where
    add x c = 10 * x + fromIntegral (ord c - ord '0')

readFractionalUnsafe :: Text -> Rational
readFractionalUnsafe t = case T.splitOn (T.pack ".") t of
  [t1, t2] ->
    let n = T.length t2
        x = readIntegralUnsafe t1 % 1
        y = readIntegralUnsafe t2
     in x + y % 10 ^ n
  _ -> error "Lexer matched invalid fractional number. This is a bug."

data LexState = LexState
  { -- | The remaining source to lex
    remaining :: !L.Text,
    -- | The memorized previous character
    prev :: {-# UNPACK #-} !Char,
    -- | The partial accumulation of the current token
    token :: !(Maybe Text),
    -- | The remaining bytes in the current character
    current :: {-# UNPACK #-} !(Char, [Word8]),
    -- | The start code for the next token scan
    startCode :: {-# UNPACK #-} !Int,
    -- | The position in the source code
    position :: {-# UNPACK #-} !LexPosition,
    -- | The start position of the current lexeme
    startPos :: !(Maybe LexPosition)
  }
  deriving stock (Show)

data LexPosition = LexPosition
  { line :: {-# UNPACK #-} !Word,
    column :: {-# UNPACK #-} !Word
  }
  deriving stock (Show)

data LexError = LexError
  { line :: {-# UNPACK #-} !Word,
    column :: {-# UNPACK #-} !Word
  }
  deriving stock (Show)

type AlexInput = LexState

uncons' :: L.Text -> Maybe (Char, NonEmpty Word8, L.Text)
uncons' t = do
  let (c, t') = L.splitAt 1 t
  (guard . not . L.null) c -- This check validates that `head` and `fromList` will succeed
  let bytes = (NE.fromList . LBS.unpack . L.encodeUtf8) c
  pure (L.head c, bytes, t')

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte s@LexState {current = (c, x : xs)} = Just (x, s {current = (c, xs)})
alexGetByte s@LexState {current = (c, []), remaining, prev, token, position, startPos} = do
  (c', x :| xs, remaining') <- uncons' remaining
  let s' = s
        { current = (c', xs),
          remaining = remaining',
          prev = c,
          token = ((`T.snoc` c') <$> token) <|> Just (T.singleton c'),
          position = position',
          startPos = startPos <|> Just position'
        }
      LexPosition {line, column} = position
      position' :: LexPosition
      position' = case prev of
        '\n' -> position { line = line + 1, column = 1 }
        _ -> position { column = column + 1 }
  pure (x, s')

lexList :: '[State LexState, Error LexError] :>> es => Eff es [Located Token]
lexList = lexCont \case
  Located _ TkEOF -> pure []
  t -> (t :) <$> lexList

lexCont :: '[State LexState, Error LexError] :>> es => (Located Token -> Eff es a) -> Eff es a
lexCont f = do
  s <- get
  case alexScan s s.startCode of
    AlexEOF -> f (Located (Loc 0 0 0 0) TkEOF)
    AlexError LexState {position = LexPosition {line, column}} -> throwError LexError {line, column}
    AlexSkip s' _len -> do
      put (resetToken s')
      lexCont f
    AlexToken s' _len action -> do
      put (resetToken s')
      case s'.token of
        Nothing -> error "Lexer matched empty token. This is a bug."
        Just t -> action t >>= f
  where
    resetToken s = s {token = Nothing}

runLexer :: Error LexError :> es => L.Text -> Eff es [Located Token]
runLexer source = evalState (initialLexState source) lexList

initialLexState :: L.Text -> LexState
initialLexState source =
  LexState
    { remaining = source,
      prev = '\n',
      token = Nothing,
      current = ('\0', []),
      startCode = 0,
      position = LexPosition
        { line = 0,
          column = 1
        },
      startPos = Just LexPosition
        { line = 0,
          column = 1
        }
    }
}
