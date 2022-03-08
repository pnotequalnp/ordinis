module Main where

import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (asum, traverse_)
import Data.Text qualified as T
import Data.Text.Lazy.Encoding qualified as L
import Data.Text.Lazy.IO qualified as L
import Effectful
import Effectful.Environment (getArgs, runEnvironment, withArgs)
import Effectful.Error.Static (runErrorNoCallStack)
import Errata (Errata)
import Errata qualified
import Errata.Styles qualified
import Language.Ordinis (LexError (LexError), ParseError (..), Span (Span))
import Language.Ordinis qualified as Ordinis
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Opts
import System.Exit (exitFailure)
import System.IO (stderr)
import Prelude hiding (lex)

data Mode
  = Lex
  | Parse
  | Evaluate
  | Interpret
  | Compile

data Options = Options
  { mode :: Mode,
    filePath :: FilePath
  }

main :: IO ()
main = runEff
  . runEnvironment
  $ do
    (compilerArgs, trimArgs -> _programArgs) <- span (/= "--") <$> getArgs

    Options {mode, filePath} <- withArgs compilerArgs . liftIO $ Opts.execParser parser

    case mode of
      Lex -> lex filePath
      Parse -> parse filePath
      Evaluate -> error "Evaluator not implemented"
      Interpret -> error "Interpreter not implemented"
      Compile -> error "Compiler not implemented"
  where
    trimArgs = \case
      "--" : xs -> xs
      xs -> xs

lex :: IOE :> e => FilePath -> Eff e ()
lex fp = do
  source <- L.decodeUtf8 <$> liftIO (LBS.readFile fp)
  runErrorNoCallStack @LexError (Ordinis.runLexer source) >>= \case
    Left err -> liftIO $ (L.hPutStrLn stderr . Errata.prettyErrors source . pure . lexError fp) err *> exitFailure
    Right tokens -> liftIO (traverse_ print tokens)

parse :: IOE :> e => FilePath -> Eff e ()
parse fp = do
  source <- L.decodeUtf8 <$> liftIO (LBS.readFile fp)
  (runErrorNoCallStack @LexError . runErrorNoCallStack @ParseError) (Ordinis.runParser source)
    >>= \case
      Left lerr -> liftIO $ (L.hPutStrLn stderr . Errata.prettyErrors source . pure . lexError fp) lerr *> exitFailure
      Right (Left perr) -> liftIO $ (L.hPutStrLn stderr . Errata.prettyErrors source . pure . parseError fp) perr *> exitFailure
      Right (Right cst) -> liftIO (traverse_ print cst)

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
parseError fp (UnexpectedToken Span {startCol, endCol, endLine, value}) = Errata.errataSimple (Just "Failed to parse source") block Nothing
  where
    block =
      Errata.blockSimple
        Errata.Styles.fancyStyle
        Errata.Styles.fancyRedPointer
        fp
        Nothing
        (fromIntegral endLine, fromIntegral startCol, fromIntegral endCol, Just ("Unexpected token " <> T.pack (show value)))
        Nothing

parser :: ParserInfo Options
parser = Opts.info (Opts.helper <*> parseOptions) Opts.fullDesc

parseOptions :: Parser Options
parseOptions = Options <$> parseMode <*> parseFilePath

parseFilePath :: Parser FilePath
parseFilePath = Opts.strArgument (Opts.metavar "FILEPATH")

parseMode :: Parser Mode
parseMode =
  asum
    [ Opts.flag' Lex (Opts.long "lex"),
      Opts.flag' Parse (Opts.long "parse"),
      Opts.flag' Evaluate (Opts.long "eval" <> Opts.short 'e'),
      Opts.flag' Interpret (Opts.long "exec" <> Opts.short 'x'),
      pure Compile
    ]
