module Typechecking where

import Data.Text.Lazy qualified as L
import Effectful (runPureEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Language.Ordinis
import Test.Tasty.HUnit

newtype LocatedEq a = LocatedEq (Located a)
  deriving newtype (Show)

instance Eq a => Eq (LocatedEq a) where
  LocatedEq (Located l x) == LocatedEq (Located l' y) =
    l == l' && x == y

unit_monomorphicIdentityFunction :: Assertion
unit_monomorphicIdentityFunction = testSuccess source expectation
  where
    source =
      L.unlines
        [ "id_Int64 : Int64 -> Int64",
          "id_Int64 x = x"
        ]
    expectation = ()

unit_unboundVariable :: Assertion
unit_unboundVariable = testFailure source expectation
  where
    source =
      L.unlines
        [ "foo : Int64",
          "foo = bar"
        ]
    expectation = UnboundVariable (Located (Loc 2 7 10) "bar")

unit_loneTypeSignature :: Assertion
unit_loneTypeSignature = testFailure source expectation
  where
    source = "foo : Int64"
    expectation = LoneTypeSignature (Located (Loc 1 1 4) "foo")

unit_extraEquationParameters :: Assertion
unit_extraEquationParameters = testFailure source expectation
  where
    source =
      L.unlines
        [ "foo : Int64",
          "foo x = x"
        ]
    expectation = ExtraParameters (Located (Loc 2 1 4) "foo")

unit_mismatchedEquationParamCounts :: Assertion
unit_mismatchedEquationParamCounts = testFailure source expectation
  where
    source =
      L.unlines
        [ "foo : Int64",
          "foo x = x",
          "foo x y = x"
        ]
    expectation = MismatchedParamCounts (Located (Loc 2 1 4) "foo")

typecheck :: L.Text -> IO (Either TypeError ())
typecheck source = do
  let res = runPureEff
        . runErrorNoCallStack @LexError
        . runErrorNoCallStack @ParseError
        . runErrorNoCallStack @TypeError
        $ do
          cst <- runParser source
          runTypechecker cst
  case res of
    Left _ -> assertFailure "Failed to lex source"
    Right (Left _) -> assertFailure "Failed to parse source"
    Right (Right result) -> pure result

testSuccess :: L.Text -> () -> Assertion
testSuccess source expectation = do
  result <- typecheck source
  result @?= Right expectation

testFailure :: L.Text -> TypeError -> Assertion
testFailure source expectation =
  typecheck source >>= \case
    Right _ -> assertFailure "Typechecking succeeded"
    Left err -> eqTypeErrors err expectation

eqTypeErrors :: TypeError -> TypeError -> Assertion
eqTypeErrors (ExtraParameters name) (ExtraParameters name') = LocatedEq name @?= LocatedEq name'
eqTypeErrors (MismatchedParamCounts name) (MismatchedParamCounts name') = LocatedEq name @?= LocatedEq name'
eqTypeErrors (UnboundVariable name) (UnboundVariable name') = LocatedEq name @?= LocatedEq name'
eqTypeErrors (LoneTypeSignature name) (LoneTypeSignature name') = LocatedEq name @?= LocatedEq name'
eqTypeErrors x y = assertFailure ("Type errors don't match:\nexpected: " <> show y <> "\nbut got: " <> show x)
