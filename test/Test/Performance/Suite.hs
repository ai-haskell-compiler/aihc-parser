{-# LANGUAGE OverloadedStrings #-}

module Test.Performance.Suite
  ( parserPerformanceTests,
  )
where

import Aihc.Parser
import Aihc.Parser.Syntax (Extension (Arrows, PatternSynonyms, RecursiveDo, ViewPatterns), parseExtensionName)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (parseEither, withObject)
import Data.Char (chr, toLower)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Y
import ParserGolden (ExpectedStatus (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, testCaseInfo)

data PerfCase = PerfCase
  { perfCaseId :: !String,
    perfCaseSourceName :: !FilePath,
    perfCaseExtensions :: ![Extension],
    perfCaseInput :: !Text,
    perfCaseStatus :: !ExpectedStatus,
    perfCaseReason :: !String
  }

fixtureRoot :: FilePath
fixtureRoot = "test/Test/Fixtures/performance/module"

timeoutMicros :: Int
timeoutMicros = 1000000

generatedCaseSize :: Int
generatedCaseSize = 200

-- | Depth for nested list-pattern cases. Keep the source below 10KiB.
listPatternSize :: Int
listPatternSize = 4000

-- | Depth for nested record-pattern cases. Keep the source below 10KiB.
recordPatternSize :: Int
recordPatternSize = 2000

-- | Depth for nested view-pattern cases. Keep the source below 10KiB.
viewPatternSize :: Int
viewPatternSize = 1200

-- | Depth for nested proc-do command parentheses. Keep the source below 10KiB.
procDoParenSize :: Int
procDoParenSize = 2000

parserPerformanceTests :: IO TestTree
parserPerformanceTests = do
  fixtureCases <- loadPerfCases
  pure $
    testGroup
      "performance"
      [ testGroup
          "parse-under-1s"
          [ testGroup "fixtures" (map mkPerfCaseTest fixtureCases),
            testGroup "generated" (map mkPerfCaseTest generatedPerfCases)
          ]
      ]

mkPerfCaseTest :: PerfCase -> TestTree
mkPerfCaseTest perfCase = case perfCaseStatus perfCase of
  StatusXFail -> testCaseInfo (perfCaseId perfCase) (xfailDetails perfCase <* assertPerfCase perfCase)
  _ -> testCase (perfCaseId perfCase) (assertPerfCase perfCase)

xfailDetails :: PerfCase -> IO String
xfailDetails perfCase = do
  outcome <-
    timeout timeoutMicros $
      evaluate $
        force $
          parseModule
            defaultConfig
              { parserSourceName = perfCaseSourceName perfCase,
                parserExtensions = perfCaseExtensions perfCase
              }
            (perfCaseInput perfCase)
  case outcome of
    Nothing -> pure ("known bug still present: module parse exceeded " <> show timeoutMicros <> "us")
    Just (errs, _)
      | null errs -> assertFailure "expected xfail performance case to still fail"
      | otherwise ->
          pure
            ( "known bug still present: "
                <> formatParseErrors (perfCaseSourceName perfCase) (Just (perfCaseInput perfCase)) errs
            )

assertPerfCase :: PerfCase -> Assertion
assertPerfCase perfCase = do
  outcome <-
    timeout timeoutMicros $
      evaluate $
        force $
          parseModule
            defaultConfig
              { parserSourceName = perfCaseSourceName perfCase,
                parserExtensions = perfCaseExtensions perfCase
              }
            (perfCaseInput perfCase)
  case (perfCaseStatus perfCase, outcome) of
    (status, Nothing)
      | status /= StatusXFail ->
          assertFailure
            ( "module parse exceeded "
                <> show timeoutMicros
                <> "us for "
                <> perfCaseId perfCase
            )
    (StatusPass, Just (errs, _))
      | not (null errs) ->
          assertFailure
            ( "expected parse success for performance case "
                <> perfCaseId perfCase
                <> ", got parse error: "
                <> formatParseErrors (perfCaseSourceName perfCase) (Just (perfCaseInput perfCase)) errs
            )
    (StatusFail, Just (errs, _))
      | null errs -> assertFailure ("expected parse failure for performance case " <> perfCaseId perfCase)
    (StatusXFail, Nothing) -> pure ()
    (StatusXFail, Just (errs, _))
      | null errs ->
          assertFailure
            ( "Unexpected pass in xfail performance case "
                <> perfCaseId perfCase
                <> " reason="
                <> perfCaseReason perfCase
            )
    _ -> pure ()

loadPerfCases :: IO [PerfCase]
loadPerfCases = do
  exists <- doesDirectoryExist fixtureRoot
  if not exists
    then pure []
    else do
      paths <- listFixtureFiles fixtureRoot
      mapM loadPerfCase paths

loadPerfCase :: FilePath -> IO PerfCase
loadPerfCase path = do
  source <- TIO.readFile path
  case parsePerfCaseText path source of
    Left err -> fail err
    Right perfCase -> pure perfCase

parsePerfCaseText :: FilePath -> Text -> Either String PerfCase
parsePerfCaseText path source = do
  value <-
    case Y.decodeEither' (TE.encodeUtf8 source) of
      Left err -> Left ("Invalid performance fixture " <> path <> ": " <> Y.prettyPrintParseException err)
      Right parsed -> Right parsed
  (extNames, inputText, statusText, reasonText) <-
    case parseEither
      ( withObject "performance fixture" $ \obj -> do
          exts <- obj .:? "extensions" .!= []
          inputText <- obj .: "input"
          status <- obj .:? "status" .!= "pass"
          reason <- obj .:? "reason" .!= ""
          pure (exts, inputText, status, reason)
      )
      value of
      Left err -> Left ("Invalid performance fixture schema in " <> path <> ": " <> err)
      Right parsed -> Right parsed
  exts <- traverse (parseExtension path) extNames
  status <- parseStatus path statusText
  pure
    PerfCase
      { perfCaseId = dropRootPrefix path,
        perfCaseSourceName = dropRootPrefix path,
        perfCaseExtensions = exts,
        perfCaseInput = inputText,
        perfCaseStatus = status,
        perfCaseReason = T.unpack reasonText
      }
  where
    parseExtension fixturePath raw =
      case parseExtensionName raw of
        Just ext -> Right ext
        Nothing -> Left ("Unknown parser extension " <> show raw <> " in " <> fixturePath)
    parseStatus fixturePath raw =
      case map toLower (T.unpack (T.strip raw)) of
        "pass" -> Right StatusPass
        "fail" -> Right StatusFail
        "xfail" -> Right StatusXFail
        _ -> Left ("Invalid [status] in " <> fixturePath <> ": " <> T.unpack raw)

listFixtureFiles :: FilePath -> IO [FilePath]
listFixtureFiles dir = do
  entries <- sort <$> listDirectory dir
  concat
    <$> mapM
      ( \entry -> do
          let path = dir </> entry
          isDir <- doesDirectoryExist path
          if isDir
            then listFixtureFiles path
            else
              if map toLower (takeExtension path) `elem` [".yaml", ".yml"]
                then pure [path]
                else pure []
      )
      entries

dropRootPrefix :: FilePath -> FilePath
dropRootPrefix path =
  case splitAt (length fixtureRoot + 1) path of
    (prefix, rest)
      | prefix == fixtureRoot <> "/" -> rest
    _ -> path

generatedPerfCases :: [PerfCase]
generatedPerfCases =
  [ mkGeneratedPerfCase "tuple-expression-nested" (mkExprModule (nestedTupleExpr generatedCaseSize)),
    mkGeneratedPerfCase "tuple-expression-wide" (mkExprModule (wideTupleExpr generatedCaseSize)),
    mkGeneratedPerfCase "expression-list" (mkExprModule (longListExpr generatedCaseSize)),
    mkGeneratedPerfCase "tuple-type-nested" (mkTypeModule (nestedTupleType generatedCaseSize)),
    mkGeneratedPerfCase "tuple-type-wide" (mkTypeModule (wideTupleType generatedCaseSize)),
    mkGeneratedPerfCase "tuple-pattern-nested" (mkPatternModule (nestedTuplePattern generatedCaseSize)),
    mkGeneratedPerfCase "tuple-pattern-wide" (mkPatternModule (wideTuplePattern generatedCaseSize)),
    mkGeneratedPerfCase "tuple-pattern-function-nested" (mkTuplePatternFunctionModule (nestedTuplePattern generatedCaseSize)),
    mkGeneratedPerfCase "tuple-pattern-function-wide" (mkTuplePatternFunctionModule (wideTuplePattern generatedCaseSize)),
    mkGeneratedPerfCase "enum-data-constructors" (mkDataModule (enumDataDecl generatedCaseSize)),
    mkGeneratedPerfCase "record-data-fields" (mkDataModule (recordDataDecl generatedCaseSize)),
    mkGeneratedPerfCase "type-right-leaning-terms" (mkTypeModule (rightLeaningType generatedCaseSize)),
    mkGeneratedPerfCase "type-left-leaning-terms" (mkTypeModule (leftLeaningType generatedCaseSize)),
    mkGeneratedPerfCase "type-parameters" (mkTypeModule (typeWithParameters generatedCaseSize)),
    mkGeneratedPerfCase "string-escapes" (mkExprModule (escapedStringExpr (generatedCaseSize * 500))),
    mkGeneratedPerfCase "nested-application" (mkExprModule (nestedAppExpr generatedCaseSize)),
    mkGeneratedPerfCaseWithStatus "invalid-module" "module Generated where\nvalue = { x = 1, }\n" StatusFail "",
    -- Nested block expressions must not be parsed again as patterns.
    mkGeneratedPerfCase "nested-paren-do" (mkExprModule (nestedParenDoExpr generatedCaseSize)),
    mkGeneratedPerfCase "nested-paren-do-case" (mkExprModule (nestedParenDoCaseExpr generatedCaseSize)),
    mkGeneratedPerfCase "nested-paren-do-if" (mkExprModule (nestedParenDoIfExpr generatedCaseSize)),
    mkGeneratedPerfCase "nested-paren-do-lambda" (mkExprModule (nestedParenDoLambdaExpr generatedCaseSize)),
    mkGeneratedPerfCase "nested-paren-do-let" (mkExprModule (nestedParenDoLetExpr generatedCaseSize)),
    mkGeneratedPerfCase "nested-paren-do-comp" (mkCompModule (nestedParenDoExpr generatedCaseSize)),
    mkGeneratedPerfCase "nested-paren-do-guard" (mkGuardModule (nestedParenDoExpr generatedCaseSize)),
    mkGeneratedPerfCase
      "nested-paren-do-bind"
      (mkExprModule (nestedWrap "do { x <- (" "); pure x }" "pure 1" generatedCaseSize)),
    mkGeneratedPerfCaseFull
      "nested-paren-mdo"
      generatedCaseSize
      [RecursiveDo]
      (mkExprModule ("mdo { (" <> nestedParenDoExpr generatedCaseSize <> ") }"))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "malformed-nested-proc-do-blocks"
      generatedCaseSize
      [Arrows]
      (mkProcModule (nestedParenDoExpr generatedCaseSize))
      StatusFail
      "",
    mkGeneratedPerfCaseFull
      "nested-proc-do-blocks"
      generatedCaseSize
      [Arrows]
      (mkProcModule (nestedWrap "do { (" ") }" "returnA -< x" generatedCaseSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "malformed-nested-let"
      generatedCaseSize
      []
      (mkExprModule (malformedNestedLetExpr generatedCaseSize))
      StatusFail
      "",
    -- Nested patterns must reuse the expression parse.
    mkGeneratedPerfCaseFull
      "nested-list-pattern"
      listPatternSize
      []
      (mkTuplePatternFunctionModule (nestedListPattern listPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-list-pattern-bind"
      listPatternSize
      []
      (mkPatBindModule (nestedListPattern listPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-list-pattern-case"
      listPatternSize
      []
      (mkCaseModule (nestedListPattern listPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-list-pattern-lambda"
      listPatternSize
      []
      (mkLambdaModule (nestedListPattern listPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-list-pattern-do"
      listPatternSize
      []
      (mkDoStmtModule (nestedListPattern listPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-list-pattern-guard"
      listPatternSize
      []
      (mkGuardModule (nestedListPattern listPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-list-pattern-comp"
      listPatternSize
      []
      (mkCompModule (nestedListPattern listPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-list-pattern-where"
      listPatternSize
      []
      (mkWherePatModule (nestedListPattern listPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-list-pattern-synonym"
      listPatternSize
      [PatternSynonyms]
      (mkPatSynModule (nestedListPattern listPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-list-tuple-pattern"
      listPatternSize
      []
      (mkTuplePatternFunctionModule (nestedWrap "[" "]" "(x, y)" listPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-record-pattern"
      recordPatternSize
      []
      (mkTuplePatternFunctionModule (nestedRecordPattern recordPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-record-pattern-bind"
      recordPatternSize
      []
      (mkPatBindModule (nestedRecordPattern recordPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-record-pattern-case"
      recordPatternSize
      []
      (mkCaseModule (nestedRecordPattern recordPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-record-pattern-lambda"
      recordPatternSize
      []
      (mkLambdaModule (nestedRecordPattern recordPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-view-pattern"
      viewPatternSize
      [ViewPatterns]
      (mkTuplePatternFunctionModule (nestedViewPattern viewPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-view-pattern-bind"
      viewPatternSize
      [ViewPatterns]
      (mkPatBindModule (nestedViewPattern viewPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-view-pattern-case"
      viewPatternSize
      [ViewPatterns]
      (mkCaseModule (nestedViewPattern viewPatternSize))
      StatusPass
      "",
    mkGeneratedPerfCaseFull
      "nested-proc-do-parens"
      procDoParenSize
      [Arrows]
      (mkProcModule ("do { " <> nestedWrap "(" ")" "returnA -< x" procDoParenSize <> " }"))
      StatusPass
      ""
  ]

mkGeneratedPerfCase :: String -> Text -> PerfCase
mkGeneratedPerfCase label inputText =
  mkGeneratedPerfCaseWithStatus label inputText StatusPass ""

mkGeneratedPerfCaseWithStatus :: String -> Text -> ExpectedStatus -> String -> PerfCase
mkGeneratedPerfCaseWithStatus label =
  mkGeneratedPerfCaseFull label generatedCaseSize []

mkGeneratedPerfCaseFull :: String -> Int -> [Extension] -> Text -> ExpectedStatus -> String -> PerfCase
mkGeneratedPerfCaseFull label size exts inputText status reason =
  let caseId = "generated/" <> label <> "-" <> show size <> ".hs"
   in PerfCase
        { perfCaseId = caseId,
          perfCaseSourceName = caseId,
          perfCaseExtensions = exts,
          perfCaseInput = inputText,
          perfCaseStatus = status,
          perfCaseReason = reason
        }

mkExprModule :: Text -> Text
mkExprModule expr = T.unlines ["module Generated where", "value = " <> expr]

mkTypeModule :: Text -> Text
mkTypeModule ty = T.unlines ["module Generated where", "value :: " <> ty, "value = undefined"]

mkPatternModule :: Text -> Text
mkPatternModule pat = T.unlines ["module Generated where", "value " <> pat <> " = x1", "  where", "    x1 = 1"]

mkTuplePatternFunctionModule :: Text -> Text
mkTuplePatternFunctionModule pat = T.unlines ["module Generated where", "fn " <> pat <> " = ()"]

mkDataModule :: Text -> Text
mkDataModule decl = T.unlines ["module Generated where", decl]

mkPatBindModule :: Text -> Text
mkPatBindModule pat = T.unlines ["module Generated where", pat <> " = ()"]

mkCaseModule :: Text -> Text
mkCaseModule pat = T.unlines ["module Generated where", "fn v = case v of", "  " <> pat <> " -> ()"]

mkLambdaModule :: Text -> Text
mkLambdaModule pat = T.unlines ["module Generated where", "fn = \\" <> pat <> " -> ()"]

mkDoStmtModule :: Text -> Text
mkDoStmtModule stmt = T.unlines ["module Generated where", "fn = do", "  " <> stmt, "  pure ()"]

mkGuardModule :: Text -> Text
mkGuardModule guard = T.unlines ["module Generated where", "fn | " <> guard <> " = ()"]

mkCompModule :: Text -> Text
mkCompModule qual = T.unlines ["module Generated where", "fn = [() | " <> qual <> "]"]

mkWherePatModule :: Text -> Text
mkWherePatModule pat = T.unlines ["module Generated where", "fn = ()", "  where", "    " <> pat <> " = ()"]

mkPatSynModule :: Text -> Text
mkPatSynModule pat = T.unlines ["module Generated where", "pattern P = " <> pat]

mkProcModule :: Text -> Text
mkProcModule cmd = T.unlines ["module Generated where", "fn = proc x -> " <> cmd]

-- | Wrap an inner term in @n@ copies of an open/close pair.
nestedWrap :: Text -> Text -> Text -> Int -> Text
nestedWrap open close inner n =
  T.concat (replicate n open) <> inner <> T.concat (replicate n close)

nestedParenDoExpr :: Int -> Text
nestedParenDoExpr = nestedWrap "do { (" ") }" "pure 1"

nestedParenDoCaseExpr :: Int -> Text
nestedParenDoCaseExpr = nestedWrap "do { (case x of { _ -> " " }) }" "1"

nestedParenDoIfExpr :: Int -> Text
nestedParenDoIfExpr = nestedWrap "do { (if True then " " else 0) }" "1"

nestedParenDoLambdaExpr :: Int -> Text
nestedParenDoLambdaExpr = nestedWrap "do { (\\x -> " ") }" "x"

nestedParenDoLetExpr :: Int -> Text
nestedParenDoLetExpr = nestedWrap "do { (let x = " " in x) }" "1"

malformedNestedLetExpr :: Int -> Text
malformedNestedLetExpr n = nestedWrap "let x = " " in " "1" n <> "x"

nestedListPattern :: Int -> Text
nestedListPattern = nestedWrap "[" "]" "x"

nestedRecordPattern :: Int -> Text
nestedRecordPattern = nestedWrap "T{x=" "}" "y"

nestedViewPattern :: Int -> Text
nestedViewPattern = nestedWrap "(id -> " ")" "x"

nestedTupleExpr :: Int -> Text
nestedTupleExpr n =
  case n of
    0 -> "a"
    _ -> "(" <> "a, " <> nestedTupleExpr (n - 1) <> ")"

wideTupleExpr :: Int -> Text
wideTupleExpr n = tupleText n "a"

longListExpr :: Int -> Text
longListExpr n = "[" <> T.intercalate ", " (replicate n "a") <> "]"

nestedTupleType :: Int -> Text
nestedTupleType n =
  case n of
    0 -> "A"
    _ -> "(" <> "A, " <> nestedTupleType (n - 1) <> ")"

wideTupleType :: Int -> Text
wideTupleType n = tupleText n "A"

nestedTuplePattern :: Int -> Text
nestedTuplePattern n =
  case n of
    0 -> "x1"
    _ -> "(" <> T.pack ("x" <> show (n + 1)) <> ", " <> nestedTuplePattern (n - 1) <> ")"

wideTuplePattern :: Int -> Text
wideTuplePattern n = tupleItemsText (patternVars n)

enumDataDecl :: Int -> Text
enumDataDecl n =
  "data T = "
    <> T.intercalate " | " [T.pack ("C" <> show ix) | ix <- [1 .. n]]

recordDataDecl :: Int -> Text
recordDataDecl n =
  "data T = T\n  { "
    <> T.intercalate "\n  , " [T.pack ("field" <> show ix) <> " :: A" | ix <- [1 .. n]]
    <> "\n  }"

rightLeaningType :: Int -> Text
rightLeaningType n =
  case n of
    0 -> "A"
    1 -> "A"
    _ -> "A -> (" <> rightLeaningType (n - 1) <> ")"

leftLeaningType :: Int -> Text
leftLeaningType n =
  case n of
    0 -> "A"
    1 -> "A"
    _ -> "(" <> leftLeaningType (n - 1) <> " -> A)"

typeWithParameters :: Int -> Text
typeWithParameters n =
  T.unwords ("A" : replicate n "a")

escapedStringExpr :: Int -> Text
escapedStringExpr desiredLength =
  T.concat ["\"", takeEscapedText desiredLength (cycle escapedStringFragments), "\""]

escapedStringFragments :: [Text]
escapedStringFragments =
  [shownCharBody (chr n) | n <- [0 .. 255]]
  where
    shownCharBody ch =
      let shown = show [ch]
       in T.pack (take (length shown - 2) (drop 1 shown))

takeEscapedText :: Int -> [Text] -> Text
takeEscapedText desiredLength = go 0
  where
    go accLen (fragment : rest)
      | accLen >= desiredLength = ""
      | otherwise =
          fragment <> go (accLen + T.length fragment) rest
    go _ [] = ""

tupleText :: Int -> Text -> Text
tupleText n atom =
  tupleItemsText (replicate n atom)

tupleItemsText :: [Text] -> Text
tupleItemsText items =
  "(" <> T.intercalate ", " items <> ")"

patternVars :: Int -> [Text]
patternVars n = [T.pack ("x" <> show ix) | ix <- [1 .. n]]

-- | Generate deeply nested constructor application: A(A(A(...A(a)...)))
-- This exercises the paren expression parser's performance under deep nesting.
nestedAppExpr :: Int -> Text
nestedAppExpr n =
  case n of
    0 -> "a"
    _ -> "A(" <> nestedAppExpr (n - 1) <> ")"
