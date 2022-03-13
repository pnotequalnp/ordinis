module Main where

import Control.Applicative (optional)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (asum, traverse_)
import Data.Text (Text)
import Data.Text.Lazy qualified as L
import Data.Text.Lazy.Encoding qualified as L
import Data.Text.Lazy.IO qualified as L
import Data.Version (showVersion)
import Effectful
import Effectful.Environment (getArgs, runEnvironment, withArgs)
import Effectful.Error.Static (runErrorNoCallStack)
import Errata (Errata)
import Errata qualified
import Errata.Styles qualified
import Language.Ordinis (LexError (LexError), Located (..), ParseError (..), TypeError (..))
import Language.Ordinis qualified as Ordinis
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Opts
import Paths_ordinis (version)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Prelude hiding (lex)

data Mode
  = Lex
  | Parse
  | Typecheck
  | Type
  | Evaluate
  | Interpret
  | Compile
  | Version

data Options = Options
  { mode :: Mode,
    filePath :: Maybe FilePath
  }

main :: IO ()
main = runEff
  . runEnvironment
  $ do
    (compilerArgs, trimArgs -> _programArgs) <- span (/= "--") <$> getArgs

    Options {mode, filePath} <- withArgs compilerArgs . liftIO $ Opts.execParser parser

    (fp, src) <- liftIO case filePath of
      Nothing -> ("<STDIN>",) . L.decodeUtf8 <$> LBS.getContents
      Just f -> (f,) . L.decodeUtf8 <$> LBS.readFile f

    case mode of
      Lex -> lex fp src
      Parse -> parse fp src
      Version -> liftIO (hPutStrLn stderr (showVersion version))
      Type -> error "Typechecker not implemented"
      Evaluate -> error "Evaluator not implemented"
      Typecheck -> typecheck fp src
      Interpret -> error "Interpreter not implemented"
      Compile -> error "Compiler not implemented"
  where
    trimArgs = \case
      "--" : xs -> xs
      xs -> xs

lex :: IOE :> es => FilePath -> L.Text -> Eff es ()
lex fp source =
  runErrorNoCallStack @LexError (Ordinis.runLexer source) >>= \case
    Left err -> liftIO $ (L.hPutStrLn stderr . Errata.prettyErrors source . pure . lexError fp) err *> exitFailure
    Right tokens -> liftIO (traverse_ print tokens)

parse :: IOE :> es => FilePath -> L.Text -> Eff es ()
parse fp source =
  (runErrorNoCallStack @LexError . runErrorNoCallStack @ParseError) (Ordinis.runParser source) >>= \case
    Left lerr -> liftIO $ (L.hPutStrLn stderr . Errata.prettyErrors source . pure . lexError fp) lerr *> exitFailure
    Right (Left perr) -> liftIO $ (L.hPutStrLn stderr . Errata.prettyErrors source . pure . parseError fp) perr *> exitFailure
    Right (Right cst) -> liftIO (traverse_ print cst)

typecheck :: IOE :> es => FilePath -> L.Text -> Eff es ()
typecheck fp source =
  (runErrorNoCallStack @LexError . runErrorNoCallStack @ParseError) (Ordinis.runParser source) >>= \case
    Left err -> liftIO do
      (L.hPutStrLn stderr . Errata.prettyErrors source . pure . lexError fp) err
      exitFailure
    Right (Left err) -> liftIO do
      (L.hPutStrLn stderr . Errata.prettyErrors source . pure . parseError fp) err
      exitFailure
    Right (Right cst) ->
      runErrorNoCallStack @TypeError (Ordinis.runTypechecker cst) >>= \case
        Left err -> liftIO do
          (L.hPutStrLn stderr . Errata.prettyErrors source . pure . typeError fp) err
          exitFailure
        Right _ -> liftIO exitSuccess

lexError :: FilePath -> LexError -> Errata
lexError fp LexError {line, column} = Errata.errataSimple (Just "Failed to lex source") block Nothing
  where
    block =
      Errata.blockSimple'
        Errata.Styles.fancyStyle
        Errata.Styles.fancyRedPointer
        fp
        Nothing
        (fromIntegral line, fromIntegral column, Just "Lexical Error")
        Nothing

parseError :: FilePath -> ParseError -> Errata
parseError fp = \case
  UnexpectedToken t ->
    Errata.errataSimple
      (Just "Failed to parse source")
      ( singleLineError
          fp
          t.loc.line
          t.loc.startCol
          t.loc.endCol
          ("Unexpected token `" <> Ordinis.renderToken t.val <> "`")
      )
      Nothing
  DuplicateLabel l ->
    Errata.errataSimple
      (Just "Failed to parse source")
      (singleLineError fp l.loc.line l.loc.startCol l.loc.endCol ("Duplicate label `" <> l.val <> "`"))
      Nothing
  EmptyVariant loc ->
    Errata.errataSimple
      (Just "Failed to parse source")
      (singleLineError fp loc.line loc.startCol loc.endCol "Empty variant")
      Nothing
  MultipleVariant loc ->
    Errata.errataSimple
      (Just "Failed to parse source")
      (singleLineError fp loc.line loc.startCol loc.endCol "Multiple definitions of variant")
      Nothing

typeError :: FilePath -> TypeError -> Errata
typeError fp = \case
  UnificationError x y -> error "main: unification error"
  LoneTypeSignature n ->
    Errata.errataSimple
      (Just "Type Error")
      ( singleLineError
          fp
          n.loc.line
          n.loc.startCol
          n.loc.endCol
          ("Type signature for `" <> n.val <> "` lacks any equation")
      )
      Nothing
  MismatchedParamCounts n ->
    Errata.errataSimple
      (Just "Type Error")
      ( singleLineError
          fp
          n.loc.line
          n.loc.startCol
          n.loc.endCol
          ("Equations for `" <> n.val <> "` have different numbers of parameters")
      )
      Nothing
  ExtraParameters n ->
    Errata.errataSimple
      (Just "Type Error")
      ( singleLineError
          fp
          n.loc.line
          n.loc.startCol
          n.loc.endCol
          ("Equation for `" <> n.val <> "` has too many parameters for its type")
      )
      Nothing
  DuplicateTypeSynonym n ->
    Errata.errataSimple
      (Just "Type Error")
      ( singleLineError
          fp
          n.loc.line
          n.loc.startCol
          n.loc.endCol
          ("Duplicate definition of type synonym `" <> n.val <> "`")
      )
      Nothing
  DuplicateTypeSignature n ->
    Errata.errataSimple
      (Just "Type Error")
      ( singleLineError
          fp
          n.loc.line
          n.loc.startCol
          n.loc.endCol
          ("Duplicate type signature for `" <> n.val <> "`")
      )
      Nothing
  UnboundVariable v ->
    Errata.errataSimple
      (Just "Type Error")
      ( singleLineError
          fp
          v.loc.line
          v.loc.startCol
          v.loc.endCol
          ("Unbound variable `" <> v.val <> "`")
      )
      Nothing

singleLineError :: FilePath -> Word -> Word -> Word -> Text -> Errata.Block
singleLineError fp line startCol endCol msg =
  Errata.blockSimple
    Errata.Styles.fancyStyle
    Errata.Styles.fancyRedPointer
    fp
    Nothing
    ( fromIntegral line,
      fromIntegral startCol,
      fromIntegral endCol,
      Just msg
    )
    Nothing

parser :: ParserInfo Options
parser = Opts.info (Opts.helper <*> parseOptions) Opts.fullDesc

parseOptions :: Parser Options
parseOptions = Options <$> parseMode <*> optional parseFilePath

parseFilePath :: Parser FilePath
parseFilePath = Opts.strArgument (Opts.metavar "FILEPATH")

parseMode :: Parser Mode
parseMode =
  asum
    [ Opts.flag' Lex (Opts.long "lex" <> Opts.internal),
      Opts.flag' Parse (Opts.long "parse" <> Opts.internal),
      Opts.flag' Version (Opts.long "version" <> Opts.short 'v' <> Opts.help "Print Ordinis version"),
      Opts.flag' Type (Opts.long "type" <> Opts.short 't' <> Opts.help "Infer the type of an expression"),
      Opts.flag' Evaluate (Opts.long "eval" <> Opts.short 'e' <> Opts.help "Evaluate an expression"),
      Opts.flag' Typecheck (Opts.long "check" <> Opts.short 'c' <> Opts.help "Typecheck a module"),
      Opts.flag' Interpret (Opts.long "exec" <> Opts.short 'x' <> Opts.help "Interpret a module"),
      pure Compile
    ]
