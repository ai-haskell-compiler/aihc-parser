{-# LANGUAGE OverloadedStrings #-}

module Test.Oracle.Suite
  ( oracleTests,
  )
where

import Aihc.Parser (ParserConfig (..), TraceEntry (..), defaultConfig, parseModuleWithTrace, tokenKindWithoutSpacing)
import Aihc.Parser.Lex (LexToken (..))
import Aihc.Parser.Syntax qualified as Syntax
import Control.Monad (when)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import ExtensionSupport
  ( CaseMeta (..),
    Expected (..),
    Outcome (..),
    caseSourcePath,
    evaluateCaseFromFile,
    evaluateCaseText,
    loadOracleCases,
    preprocessCaseSource,
  )
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, testCaseInfo)

oracleTests :: IO TestTree
oracleTests = do
  cases <- loadOracleCases
  checks <- mapM mkCaseTest cases
  framework <- frameworkTests
  summary <- mkSummaryTest cases
  traces <- mapM mkTraceTest (filter ((== ExpectPass) . caseExpected) cases)
  pure (testGroup "oracle" (checks <> [framework, summary, testGroup "token trace" traces]))

-- | The parser gives the same token trace for a source file and for its
-- pretty-printed form, when the discarded tokens are removed. A formatter
-- pairs the tokens of the two files by their position in this trace.
mkTraceTest :: CaseMeta -> IO TestTree
mkTraceTest meta = do
  source <- TIO.readFile (caseSourcePath meta)
  pure (testCase (caseId meta) (assertTraceMatches meta (preprocessCaseSource meta source)))

assertTraceMatches :: CaseMeta -> Text -> Assertion
assertTraceMatches meta source =
  case parseModuleWithTrace config source of
    (_ : _, _, _) -> assertFailure "the source does not parse"
    ([], modu, sourceTrace) ->
      let rendered = renderStrict (layoutPretty defaultLayoutOptions (pretty modu))
       in case parseModuleWithTrace config rendered of
            (_ : _, _, _) -> assertFailure "the pretty-printed source does not parse"
            ([], _, renderedTrace) ->
              let kept = filter (not . traceDiscarded)
                  sourceKept = kept sourceTrace
                  renderedKept = kept renderedTrace
                  kindOf = tokenKindWithoutSpacing . lexTokenKind . traceToken
                  sameKind a b = kindOf a == kindOf b
                  common = length (takeWhile id (zipWith sameKind sourceKept renderedKept))
               in when (common < length sourceKept || common < length renderedKept) $
                    assertFailure
                      ( "token traces differ at entry "
                          <> show common
                          <> "\nsource:   "
                          <> showEntries (drop (common - 3) sourceKept)
                          <> "\nrendered: "
                          <> showEntries (drop (common - 3) renderedKept)
                      )
  where
    config =
      defaultConfig
        { parserSourceName = caseSourcePath meta,
          parserExtensions = Syntax.effectiveExtensions Syntax.Haskell2010Edition (caseExtensions meta)
        }
    showEntries = unwords . map showEntry . take 7
    showEntry entry =
      let tok = traceToken entry
          text = lexTokenText tok
       in (if T.null text then show (lexTokenKind tok) else T.unpack text)
            <> "@"
            <> show (Syntax.sourceSpanStartLine (lexTokenSpan tok))
            <> ":"
            <> show (Syntax.sourceSpanStartCol (lexTokenSpan tok))

mkCaseTest :: CaseMeta -> IO TestTree
mkCaseTest meta = do
  source <- TIO.readFile (caseSourcePath meta)
  pure $ case caseExpected meta of
    ExpectXFail -> testCaseInfo (caseId meta) (xfailDetails (evaluateCaseText meta source) <* assertCase meta source)
    _ -> testCase (caseId meta) (assertCase meta source)

xfailDetails :: (CaseMeta, Outcome, String) -> IO String
xfailDetails (_, outcome, details) = do
  case outcome of
    OutcomeXFail -> pure ()
    _ -> assertFailure ("expected xfail outcome, got: " <> show outcome)
  pure details

mkSummaryTest :: [CaseMeta] -> IO TestTree
mkSummaryTest cases = do
  outcomes <- mapM evaluateCaseFromFile cases
  pure $
    testCase "summary" $ do
      let (passN, xfailN, xpassN, failN) = foldr (countOutcome . snd3) (0, 0, 0, 0) outcomes
          totalN = passN + xfailN + xpassN + failN
          completion = pct (passN + xpassN) totalN
      when (failN > 0 || xpassN > 0) $
        assertFailure
          ( "Oracle regressions found. "
              <> "pass="
              <> show passN
              <> " xfail="
              <> show xfailN
              <> " xpass="
              <> show xpassN
              <> " fail="
              <> show failN
              <> " completion="
              <> show completion
              <> "%"
          )
  where
    snd3 (_, b, _) = b

countOutcome :: Outcome -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
countOutcome outcome (passN, xfailN, xpassN, failN) =
  case outcome of
    OutcomePass -> (passN + 1, xfailN, xpassN, failN)
    OutcomeXFail -> (passN, xfailN + 1, xpassN, failN)
    OutcomeXPass -> (passN, xfailN, xpassN + 1, failN)
    OutcomeFail -> (passN, xfailN, xpassN, failN + 1)

pct :: Int -> Int -> Double
pct done totalN
  | totalN <= 0 = 0.0
  | otherwise = fromIntegral (done * 10000 `div` totalN) / 100.0

assertCase :: CaseMeta -> Text -> Assertion
assertCase meta source = do
  let (_, outcome, details) = evaluateCaseText meta source
  case outcome of
    OutcomeFail ->
      assertFailure
        ( "Regression in case "
            <> caseId meta
            <> " ("
            <> caseCategory meta
            <> ") expected "
            <> show (caseExpected meta)
            <> " reason="
            <> caseReason meta
            <> " details="
            <> details
        )
    OutcomeXPass ->
      assertFailure
        ( "Unexpected pass in xfail case "
            <> caseId meta
            <> " ("
            <> caseCategory meta
            <> ") reason="
            <> caseReason meta
        )
    _ -> pure ()

frameworkTests :: IO TestTree
frameworkTests =
  pure $
    testGroup
      "framework"
      [ testCase "oracle parse failure fails xfail case" $
          let meta =
                CaseMeta
                  { caseId = "framework-invalid-xfail",
                    caseCategory = "framework",
                    casePath = "framework-invalid-xfail.hs",
                    caseExpected = ExpectXFail,
                    caseReason = "regression coverage",
                    caseExtensions = []
                  }
           in do
                let (_, outcome, _) = evaluateCaseText meta "module M where\nx = { y = 1, }\n"
                if outcome == OutcomeFail
                  then pure ()
                  else assertFailure ("expected OutcomeFail when oracle rejects fixture, got " <> show outcome),
        testCase "oracle rejects top-level block-argument lambda" $
          let meta =
                CaseMeta
                  { caseId = "framework-block-argument-lambda",
                    caseCategory = "framework",
                    casePath = "framework-block-argument-lambda.hs",
                    caseExpected = ExpectPass,
                    caseReason = "",
                    caseExtensions = []
                  }
           in do
                let (_, outcome, _) = evaluateCaseText meta "module M where\nf \\x -> x\n"
                if outcome == OutcomeFail
                  then pure ()
                  else assertFailure ("expected OutcomeFail when oracle rejects fixture, got " <> show outcome),
        testCase "oracle parse failure fails pass case" $
          let meta =
                CaseMeta
                  { caseId = "framework-invalid-pass",
                    caseCategory = "framework",
                    casePath = "framework-invalid-pass.hs",
                    caseExpected = ExpectPass,
                    caseReason = "",
                    caseExtensions = []
                  }
           in do
                let (_, outcome, _) = evaluateCaseText meta "module M where\nx = { y = 1, }\n"
                if outcome == OutcomeFail
                  then pure ()
                  else assertFailure ("expected OutcomeFail when oracle rejects fixture, got " <> show outcome)
      ]
