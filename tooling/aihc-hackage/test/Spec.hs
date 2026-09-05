module Main (main) where

import Aihc.Cpp (IncludeKind (..), IncludeRequest (..))
import Aihc.Hackage.Cabal qualified as HC
import Aihc.Hackage.Cpp qualified as HCpp
import Aihc.Hackage.Index (parseHackageIndex, parseHackageIndexUpdatedSince)
import Aihc.Hackage.Stackage (parseSnapshotConstraints)
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Hackage.Util (normalizeSourceForParser)
import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Tar.Entry qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.List (isInfixOf, isSuffixOf, sort)
import Data.Text qualified as T
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import System.Directory (createDirectory, createDirectoryIfMissing, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main =
  defaultMain . testGroup "aihc-hackage" $
    [ testCase "maps selected installed packages to fixed versions" $ do
        parseSnapshotConstraints "constraints: binary installed, bytestring installed, Cabal installed, unix installed"
          @?= Right
            [ PackageSpec "binary" "0.8.9.3",
              PackageSpec "bytestring" "0.12.2.0",
              PackageSpec "Cabal" "3.14.2.0",
              PackageSpec "unix" "2.8.8.0"
            ],
      testCase "keeps installed for packages without a fixed override" $ do
        parseSnapshotConstraints "constraints: ghc-prim installed, custom-package installed"
          @?= Right
            [ PackageSpec "ghc-prim" "installed",
              PackageSpec "custom-package" "installed"
            ],
      testCase "parses latest package versions from Hackage index tarball" $ do
        parseHackageIndex testHackageIndex
          @?= Right
            [ PackageSpec "alpha" "1.2.0",
              PackageSpec "beta" "0.1"
            ],
      testCase "filters Hackage index packages by latest upload time" $ do
        parseHackageIndexUpdatedSince 100 testHackageIndex
          @?= Right
            [ PackageSpec "alpha" "1.2.0"
            ],
      testCase "generates Cabal Paths module as a normal source file" test_generatesPathsModule,
      testCase "collects exposed modules from active conditional library branches" test_collectsConditionalExposedModules,
      testCase "extracts active build tool dependency names" test_extractsBuildToolDependencyNames,
      testCase "detects packages that default to Haskell98" test_detectsHaskell98DefaultLanguage,
      testCase "ignores inactive Haskell98 default-language branches" test_ignoresInactiveHaskell98DefaultLanguage,
      testCase "detects active custom preprocessor options" test_detectsCustomPreprocessorOptions,
      testCase "collects the include-dirs a component declares" test_collectsIncludeDirs,
      testCase "finds a header that only include-dirs points at" test_resolvesIncludeFromIncludeDirs,
      testCase "finds a header kept at the package root" test_resolvesIncludeFromPackageRoot,
      testCase "leaves a plain .hs file alone" test_leavesNonLiterateSourceAlone,
      testCase "strips a leading byte order mark" test_stripsByteOrderMark,
      testCase "unliterates bird tracks in a .lhs file" test_unliteratesBirdTracks,
      testCase "keeps bird-track columns so layout still lines up" test_unliteratePreservesColumns,
      testCase "unliterates LaTeX code blocks in a .lhs file" test_unliteratesLatexCodeBlocks,
      QC.testProperty "dummy quickcheck property" prop_dummy
    ]

-- | Dummy QuickCheck property that always passes.
-- Added so that --quickcheck-tests flag is accepted by the test suite.
prop_dummy :: Bool
prop_dummy = True

(@?=) :: (Eq a, Show a) => Either String a -> Either String a -> Assertion
actual @?= expected =
  if actual == expected
    then pure ()
    else assertFailure ("expected: " <> show expected <> "\n but got: " <> show actual)

testHackageIndex :: LBS.ByteString
testHackageIndex =
  GZip.compress $
    Tar.write
      [ cabalEntryAt "alpha/1.0.0/alpha.cabal" 20,
        cabalEntryAt "alpha/1.2.0/alpha.cabal" 100,
        cabalEntryAt "alpha/1.1.0/alpha.cabal" 200,
        cabalEntryAt "beta/0.1/beta.cabal" 99,
        cabalEntry "beta/0.2/not-beta.cabal",
        cabalEntry "preferred-versions"
      ]
  where
    cabalEntry path =
      cabalEntryAt path 0

    cabalEntryAt path uploadedAt =
      case Tar.toTarPath False path of
        Left err -> error ("invalid test tar path: " <> show err)
        Right tarPath ->
          let contents = LBS.fromStrict (BSC.pack "name: ignored\n")
           in (Tar.simpleEntry tarPath (Tar.NormalFile contents (LBS.length contents))) {Tar.entryTime = uploadedAt}

test_generatesPathsModule :: Assertion
test_generatesPathsModule =
  withTempDir "aihc-hackage-paths" $ \root -> do
    let cabalFile = root </> "paths-demo.cabal"
        srcDir = root </> "src"
        sourceFile = srcDir </> "PathsUser.hs"
    createDirectoryIfMissing True srcDir
    writeFile cabalFile pathsDemoCabal
    writeFile sourceFile pathsUserSource

    cabalBytes <- BS.readFile cabalFile
    gpd <-
      case snd (runParseResult (parseGenericPackageDescription cabalBytes)) of
        Right parsed -> pure parsed
        Left (_, errs) -> assertFailure ("failed to parse test cabal file: " <> show errs)

    files <- HC.collectComponentFiles gpd root
    let paths = map HC.fileInfoPath files
        generated = root </> ".aihc-autogen" </> "Paths_paths_demo.hs"
    assertBool "expected package source module to be selected" (any ("src/PathsUser.hs" `isSuffixOf`) paths)
    assertBool "expected generated Paths module to be selected" (generated `elem` paths)

    generatedSource <- readFile generated
    assertBool "expected Cabal module header" ("module Paths_paths_demo" `isInfixOf` generatedSource)
    assertBool "expected version export" ("version :: Version" `isInfixOf` generatedSource)
    assertBool "expected getDataDir export" ("getDataDir" `isInfixOf` generatedSource)
    assertBool "expected getDataFileName export" ("getDataFileName :: FilePath -> IO FilePath" `isInfixOf` generatedSource)

    case filter ((== generated) . HC.fileInfoPath) files of
      [info] -> assertEqual "expected generated module to depend on base" [T.pack "base"] (HC.fileInfoDependencies info)
      _ -> assertFailure "expected exactly one FileInfo for generated Paths module"

test_collectsConditionalExposedModules :: Assertion
test_collectsConditionalExposedModules =
  withTempDir "aihc-hackage-conditional-exposed" $ \root -> do
    let cabalFile = root </> "conditional-exposed.cabal"
        srcDir = root </> "src" </> "Control" </> "Category"
        sourceFile = srcDir </> "Unicode.hs"
    createDirectoryIfMissing True srcDir
    writeFile cabalFile conditionalExposedCabal
    writeFile sourceFile "module Control.Category.Unicode where\n"

    cabalBytes <- BS.readFile cabalFile
    gpd <-
      case snd (runParseResult (parseGenericPackageDescription cabalBytes)) of
        Right parsed -> pure parsed
        Left (_, errs) -> assertFailure ("failed to parse test cabal file: " <> show errs)

    files <- HC.collectComponentFiles gpd root
    let paths = map HC.fileInfoPath files
    assertBool "expected conditionally exposed module to be selected" (any ("src/Control/Category/Unicode.hs" `isSuffixOf`) paths)

test_extractsBuildToolDependencyNames :: Assertion
test_extractsBuildToolDependencyNames = do
  gpd <- parseTestCabal buildToolDependsCabal
  assertEqual
    "expected active modern and legacy build tools"
    (sort ["alex", "genprimopcode"])
    (sort (map T.unpack (HC.buildToolDependencyNames gpd)))

test_detectsHaskell98DefaultLanguage :: Assertion
test_detectsHaskell98DefaultLanguage = do
  explicit <- parseTestCabal haskell98DefaultLanguageCabal
  missing <- parseTestCabal missingDefaultLanguageCabal
  supported <- parseTestCabal haskell2010DefaultLanguageCabal
  assertBool "explicit Haskell98 default-language is unsupported" (HC.packageDefaultsToHaskell98 explicit)
  assertBool "missing default-language falls back to Haskell98" (HC.packageDefaultsToHaskell98 missing)
  assertBool "Haskell2010 default-language is supported" (not (HC.packageDefaultsToHaskell98 supported))

test_ignoresInactiveHaskell98DefaultLanguage :: Assertion
test_ignoresInactiveHaskell98DefaultLanguage = do
  gpd <- parseTestCabal inactiveHaskell98DefaultLanguageCabal
  assertBool "inactive Haskell98 branch should not filter the package" (not (HC.packageDefaultsToHaskell98 gpd))

test_detectsCustomPreprocessorOptions :: Assertion
test_detectsCustomPreprocessorOptions = do
  hsp <- parseTestCabal customPreprocessorCabal
  inactive <- parseTestCabal inactiveCustomPreprocessorCabal
  pgmFOnly <- parseTestCabal pgmFWithoutFPreprocessorCabal
  assertBool "expected -F with -pgmF to require filtering" (HC.packageUsesCustomPreprocessor hsp)
  assertBool "expected inactive custom preprocessor options to be ignored" (not (HC.packageUsesCustomPreprocessor inactive))
  assertBool "expected -pgmF without -F not to enable preprocessing" (not (HC.packageUsesCustomPreprocessor pgmFOnly))

parseTestCabal :: String -> IO GenericPackageDescription
parseTestCabal source =
  case snd (runParseResult (parseGenericPackageDescription (BSC.pack source))) of
    Right parsed -> pure parsed
    Left (_, errs) -> assertFailure ("failed to parse test cabal file: " <> show errs)

pathsDemoCabal :: String
pathsDemoCabal =
  unlines
    [ "cabal-version: 3.0",
      "name: paths-demo",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: PathsUser",
      "  autogen-modules: Paths_paths_demo",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010"
    ]

pathsUserSource :: String
pathsUserSource =
  unlines
    [ "module PathsUser where",
      "import Paths_paths_demo (version, getDataDir, getDataFileName)",
      "pathsVersion = version",
      "pathsDataDir = getDataDir",
      "pathsDataFileName = getDataFileName"
    ]

conditionalExposedCabal :: String
conditionalExposedCabal =
  unlines
    [ "cabal-version: 3.0",
      "name: conditional-exposed",
      "version: 0.1.0.0",
      "",
      "flag old-base",
      "  default: False",
      "  manual: True",
      "",
      "library",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010",
      "  if flag(old-base)",
      "    build-depends: base >= 3.0 && < 3.0.3.1",
      "  else",
      "    exposed-modules: Control.Category.Unicode",
      "    build-depends: base >= 3.0.3.1 && < 5"
    ]

buildToolDependsCabal :: String
buildToolDependsCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: build-tool-demo",
      "version: 0.1.0.0",
      "",
      "flag generated",
      "  default: False",
      "  manual: True",
      "",
      "library",
      "  exposed-modules: BuildToolDemo",
      "  hs-source-dirs: src",
      "  build-tool-depends: genprimopcode:genprimopcode >= 0",
      "  build-tools: alex >= 3",
      "  default-language: Haskell2010",
      "  if flag(generated)",
      "    build-tool-depends: inactive-tool:inactive-tool >= 0"
    ]

haskell98DefaultLanguageCabal :: String
haskell98DefaultLanguageCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: haskell98-language",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: Haskell98Language",
      "  hs-source-dirs: src",
      "  default-language: Haskell98"
    ]

missingDefaultLanguageCabal :: String
missingDefaultLanguageCabal =
  unlines
    [ "cabal-version: 1.10",
      "name: missing-language",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: MissingLanguage",
      "  hs-source-dirs: src"
    ]

haskell2010DefaultLanguageCabal :: String
haskell2010DefaultLanguageCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: haskell2010-language",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: Haskell2010Language",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010"
    ]

inactiveHaskell98DefaultLanguageCabal :: String
inactiveHaskell98DefaultLanguageCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: inactive-haskell98-language",
      "version: 0.1.0.0",
      "",
      "flag legacy",
      "  default: False",
      "  manual: True",
      "",
      "library",
      "  exposed-modules: InactiveHaskell98Language",
      "  hs-source-dirs: src",
      "  default-language: Haskell2010",
      "  if flag(legacy)",
      "    default-language: Haskell98"
    ]

customPreprocessorCabal :: String
customPreprocessorCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: custom-preprocessor-demo",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: CustomPreprocessorDemo",
      "  ghc-options: -F -pgmFtrhsx -Wall",
      "  build-depends: base",
      "  default-language: Haskell2010"
    ]

inactiveCustomPreprocessorCabal :: String
inactiveCustomPreprocessorCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: inactive-custom-preprocessor-demo",
      "version: 0.1.0.0",
      "",
      "flag generated",
      "  default: False",
      "  manual: True",
      "",
      "library",
      "  exposed-modules: InactiveCustomPreprocessorDemo",
      "  build-depends: base",
      "  default-language: Haskell2010",
      "  if flag(generated)",
      "    ghc-options: -F -pgmFtrhsx"
    ]

pgmFWithoutFPreprocessorCabal :: String
pgmFWithoutFPreprocessorCabal =
  unlines
    [ "cabal-version: 2.4",
      "name: pgmf-without-f-demo",
      "version: 0.1.0.0",
      "",
      "library",
      "  exposed-modules: PgmFWithoutFDemo",
      "  ghc-options: -pgmFtrhsx -Wall",
      "  build-depends: base",
      "  default-language: Haskell2010"
    ]

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir prefix action = do
  tempRoot <- getTemporaryDirectory
  (tempFile, tempHandle) <- openTempFile tempRoot (prefix ++ "-XXXXXX")
  hClose tempHandle
  removeFile tempFile
  createDirectory tempFile
  bracket
    (pure tempFile)
    removeDirectoryRecursive
    action

--------------------------------------------------------------------------------
-- CPP include resolution
--------------------------------------------------------------------------------

-- | Regression: @conduit@, @hashmap@, @thyme@ and @ghc-internal@ keep their
-- CPP headers outside the source tree and point at them with @include-dirs@.
-- Dropping that field left every macro call unexpanded, so the parser saw
-- lines such as @STREAMING(yieldMany, ...)@ and rejected the file.
test_collectsIncludeDirs :: Assertion
test_collectsIncludeDirs =
  withTempDir "aihc-hackage-include-dirs" $ \root -> do
    let cabalFile = root </> "macros-demo.cabal"
        srcDir = root </> "src"
    createDirectoryIfMissing True srcDir
    createDirectoryIfMissing True (root </> "include")
    writeFile cabalFile macrosDemoCabal
    writeFile (srcDir </> "Macros.hs") "module Macros where\n"
    writeFile (root </> "include" </> "macros.h") "#define LENS(a) a\n"

    cabalBytes <- BS.readFile cabalFile
    gpd <-
      case snd (runParseResult (parseGenericPackageDescription cabalBytes)) of
        Right parsed -> pure parsed
        Left (_, errs) -> assertFailure ("failed to parse test cabal file: " <> show errs)

    files <- HC.collectComponentFiles gpd root
    case files of
      [info] ->
        assertEqual
          "include dirs"
          [root </> "include"]
          (HC.fileInfoIncludeDirs info)
      _ -> assertFailure ("expected exactly one source file, got " <> show (map HC.fileInfoPath files))

test_resolvesIncludeFromIncludeDirs :: Assertion
test_resolvesIncludeFromIncludeDirs =
  withTempDir "aihc-hackage-resolve-include" $ \root -> do
    let srcDir = root </> "src" </> "Deep"
        sourceFile = srcDir </> "Module.hs"
        header = BSC.pack "#define LENS(a) a\n"
    createDirectoryIfMissing True srcDir
    createDirectoryIfMissing True (root </> "headers")
    writeFile sourceFile "module Deep.Module where\n"
    BS.writeFile (root </> "headers" </> "macros.h") header

    let req = includeRequestFor sourceFile "macros.h"
    missing <- HCpp.resolveIncludeBestEffort root [] sourceFile req
    found <- HCpp.resolveIncludeBestEffort root [root </> "headers"] sourceFile req

    assertEqual "no header without include-dirs" Nothing missing
    assertEqual "header found through include-dirs" (Just header) found

-- | @conduit@\'s shape: the header sits at the package root, well above the
-- module that includes it, and only the package-wide search path finds it.
test_resolvesIncludeFromPackageRoot :: Assertion
test_resolvesIncludeFromPackageRoot =
  withTempDir "aihc-hackage-resolve-root-include" $ \root -> do
    let srcDir = root </> "src" </> "Data" </> "Conduit"
        sourceFile = srcDir </> "Combinators.hs"
        header = BSC.pack "#define STREAMING(a,b,c,d)\n"
    createDirectoryIfMissing True srcDir
    writeFile sourceFile "module Data.Conduit.Combinators where\n"
    BS.writeFile (root </> "fusion-macros.h") header

    found <-
      HCpp.resolveIncludeBestEffort
        root
        [root]
        sourceFile
        (includeRequestFor sourceFile "fusion-macros.h")
    assertEqual "header found at the package root" (Just header) found

includeRequestFor :: FilePath -> FilePath -> IncludeRequest
includeRequestFor from path =
  IncludeRequest
    { includePath = path,
      includeFrom = from,
      includeKind = IncludeLocal,
      includeLine = 1
    }

macrosDemoCabal :: String
macrosDemoCabal =
  unlines
    [ "cabal-version: 2.0",
      "name: macros-demo",
      "version: 0.1.0.0",
      "build-type: Simple",
      "",
      "library",
      "  exposed-modules: Macros",
      "  hs-source-dirs: src",
      "  include-dirs: include",
      "  build-depends: base",
      "  default-language: Haskell2010"
    ]

--------------------------------------------------------------------------------
-- Literate Haskell
--------------------------------------------------------------------------------

-- | Regression: 22 snapshot packages ship @.lhs@ sources (README.lhs entry
-- points, @happy@\'s grammar, @unlit@ itself). Handing the literate text
-- straight to the parser rejected every one of them.
test_leavesNonLiterateSourceAlone :: Assertion
test_leavesNonLiterateSourceAlone =
  assertEqual
    "plain source"
    (T.pack "module A where\nf = 1\n")
    (normalizeSourceForParser "A.hs" (T.pack "module A where\nf = 1\n"))

test_stripsByteOrderMark :: Assertion
test_stripsByteOrderMark =
  assertEqual
    "bom stripped"
    (T.pack "module A where\n")
    (normalizeSourceForParser "A.hs" (T.pack "\xfeffmodule A where\n"))

test_unliteratesBirdTracks :: Assertion
test_unliteratesBirdTracks =
  assertEqual
    "bird tracks"
    (T.pack "  module A where\n\n  f = 1\n")
    (normalizeSourceForParser "A.lhs" (T.pack "> module A where\nSome prose.\n> f = 1\n"))

-- | The leading @>@ becomes a space rather than being dropped, so tab stops
-- and alignment survive unliterating.
test_unliteratePreservesColumns :: Assertion
test_unliteratePreservesColumns =
  assertEqual
    "columns"
    (T.pack "  f = do\n      one\n      two\n")
    (normalizeSourceForParser "A.lhs" (T.pack "> f = do\n>     one\n>     two\n"))

test_unliteratesLatexCodeBlocks :: Assertion
test_unliteratesLatexCodeBlocks =
  assertEqual
    "latex blocks"
    (T.pack "\n\nmodule A where\n\n\n")
    ( normalizeSourceForParser
        "A.lhs"
        (T.pack "Some prose.\n\\begin{code}\nmodule A where\n\\end{code}\nMore prose.\n")
    )
