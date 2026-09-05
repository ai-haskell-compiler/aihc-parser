-- | End-to-end checks for the file-level pipeline the coverage command runs:
-- collect a file's CPP includes, then parse it with the extensions its
-- @.cabal@ file declares.
module Test.ParserBenchPipeline
  ( parserBenchPipelineTests,
  )
where

import Aihc.Hackage.Cabal qualified as HC
import Aihc.Hackage.Util qualified as HU
import Aihc.Parser.Bench.Parsers (ParseResult (..), collectCppIncludes, parseWithAihcExts)
import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.FilePath (makeRelative, (</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)

parserBenchPipelineTests :: TestTree
parserBenchPipelineTests =
  testGroup
    "parser coverage pipeline"
    [ testCase "parses a literate Haskell module" test_parsesLiterateModule,
      testCase "expands macros from a header behind include-dirs" test_expandsMacrosFromIncludeDirs
    ]

-- | Regression: 22 snapshot packages ship @.lhs@ sources. Handing the
-- literate text to the parser unchanged rejected every one of them.
test_parsesLiterateModule :: Assertion
test_parsesLiterateModule =
  withPackage packageCabal $ \root -> do
    createDirectoryIfMissing True (root </> "src")
    writeFile (root </> "src" </> "Lit.lhs") literateModule
    assertParses root

-- | Regression: @conduit@, @hashmap@, @thyme@ and @ghc-internal@ keep their
-- CPP macros in a header the @include-dirs@ field points at. Without that
-- search path the macro call survived preprocessing and the parser saw a
-- bare @LENS(Foo, bar, Int)@ where a declaration belongs.
test_expandsMacrosFromIncludeDirs :: Assertion
test_expandsMacrosFromIncludeDirs =
  withPackage packageCabal $ \root -> do
    createDirectoryIfMissing True (root </> "src")
    createDirectoryIfMissing True (root </> "include")
    writeFile (root </> "include" </> "macros.h") macroHeader
    writeFile (root </> "src" </> "Lit.lhs") literateMacroModule
    assertParses root

-- | Run the coverage pipeline over every source the cabal file declares.
assertParses :: FilePath -> Assertion
assertParses root = do
  cabalBytes <- BS.readFile (root </> "pipeline-demo.cabal")
  gpd <-
    case snd (runParseResult (parseGenericPackageDescription cabalBytes)) of
      Right parsed -> pure parsed
      Left (_, errs) -> assertFailure ("failed to parse test cabal file: " <> show errs)
  files <- HC.collectComponentFiles gpd root
  case files of
    [] -> assertFailure "expected the cabal file to declare a source file"
    _ -> mapM_ (parseOne root) files

parseOne :: FilePath -> HC.FileInfo -> Assertion
parseOne root info = do
  let absFile = HC.fileInfoPath info
      relPath = makeRelative root absFile
      exts = HC.fileInfoExtensions info
      cppOpts = HC.fileInfoCppOptions info
      lang = HC.fileInfoLanguage info
      deps = HC.fileInfoDependencies info
  source <- HU.readTextFileLenient absFile
  includes <-
    collectCppIncludes root (HC.fileInfoIncludeDirs info) absFile exts cppOpts lang deps source
  let includeMap = includeEntryMap root includes
  case parseWithAihcExts includeMap relPath exts cppOpts lang deps source of
    ParseSuccess -> pure ()
    ParseFailure err -> assertFailure (relPath <> " was rejected:\n" <> err)

includeEntryMap :: FilePath -> [(FilePath, Text)] -> Map.Map FilePath Text
includeEntryMap root includes =
  Map.fromList [(makeRelative root path, contents) | (path, contents) <- includes]

packageCabal :: String
packageCabal =
  unlines
    [ "cabal-version: 2.0",
      "name: pipeline-demo",
      "version: 0.1.0.0",
      "build-type: Simple",
      "",
      "library",
      "  exposed-modules: Lit",
      "  hs-source-dirs: src",
      "  include-dirs: include",
      "  build-depends: base",
      "  default-language: Haskell2010"
    ]

literateModule :: String
literateModule =
  unlines
    [ "Some prose about the module.",
      "",
      "> module Lit (answer) where",
      ">",
      "> answer :: Int",
      "> answer = 42"
    ]

literateMacroModule :: String
literateMacroModule =
  unlines
    [ "A literate module that also needs the preprocessor.",
      "",
      "> {-# LANGUAGE CPP #-}",
      "> module Lit (answer) where",
      ">",
      "> #include \"macros.h\"",
      ">",
      "> LENS(answer, Int, 42)"
    ]

macroHeader :: String
macroHeader =
  unlines
    [ "#define LENS(name, ty, val) name :: ty ; name = val"
    ]

withPackage :: String -> (FilePath -> IO a) -> IO a
withPackage cabalContents act =
  withTempDir "aihc-parser-bench-pipeline" $ \root -> do
    writeFile (root </> "pipeline-demo.cabal") cabalContents
    act root

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir template act = do
  tmp <- getTemporaryDirectory
  bracket (reserveDir tmp) removeDirectoryRecursive act
  where
    reserveDir tmp = do
      (path, handle) <- openTempFile tmp template
      hClose handle
      removeFile path
      createDirectory path
      pure path
