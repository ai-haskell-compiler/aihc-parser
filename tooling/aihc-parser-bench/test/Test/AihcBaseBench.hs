module Test.AihcBaseBench
  ( aihcBaseBenchTests,
  )
where

import Aihc.Parser.Bench.AihcBase
  ( ParsedModule (..),
    SourceFile (..),
    findHaskellFiles,
    forceModuleBody,
    forceModuleHeader,
    loadCorpus,
    parseAndForce,
    parseSourceFile,
  )
import Aihc.Parser.Syntax (ImportDecl (..), Module (..), ModuleHead (..))
import Control.Exception (bracket)
import Data.List (isInfixOf)
import Data.Text qualified as T
import System.Directory
  ( createDirectoryIfMissing,
    getTemporaryDirectory,
    makeAbsolute,
    removeDirectoryRecursive,
  )
import System.FilePath (isAbsolute, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

aihcBaseBenchTests :: TestTree
aihcBaseBenchTests =
  testGroup
    "aihc-base benchmark"
    [ testCase "collects .hs files recursively, sorted, ignoring other files" $
        withCorpus $ \root -> do
          files <- findHaskellFiles root
          assertEqual
            "corpus files"
            [root </> "Nested" </> "Deep.hs", root </> "Top.hs"]
            files,
      testCase "loads each file under its absolute path" $
        withCorpus $ \root -> do
          absoluteRoot <- makeAbsolute root
          corpus <- loadCorpus root
          assertBool "all absolute" (all (isAbsolute . sourceFileName) corpus)
          assertEqual
            "absolute names"
            [absoluteRoot </> "Nested" </> "Deep.hs", absoluteRoot </> "Top.hs"]
            (map sourceFileName corpus),
      testCase "parses GHC2021 sources that need no per-file pragma" $
        assertEqual "no errors" Nothing (parseAndForce (sourceFile "Top.hs" topModule)),
      testCase "reports the failing file when a source does not parse" $ do
        let result = parseAndForce (sourceFile "Broken.hs" "module Broken where\nx = (\n")
        assertBool "names the file" (maybe False ("Broken.hs" `isInfixOf`) result),
      testCase "the header phase covers the module name and the imports" $ do
        let parsed = parseSourceFile (sourceFile "Importer.hs" importerModule)
            tree = parsedTree parsed
        forceModuleHeader parsed `seq` pure ()
        assertEqual "module name" (Just (T.pack "Importer")) (moduleHeadName <$> moduleHead tree)
        assertEqual
          "imported modules"
          (map T.pack ["Data.List", "Data.Maybe"])
          (map importDeclModule (moduleImports tree)),
      testCase "the body phase covers the declarations" $ do
        let parsed = parseSourceFile (sourceFile "Importer.hs" importerModule)
        forceModuleHeader parsed `seq` forceModuleBody parsed `seq` pure ()
        assertEqual "no parse errors" [] (parsedErrors parsed)
        assertEqual "declaration count" 2 (length (moduleDecls (parsedTree parsed)))
    ]

-- | Has both an import list and declarations, so the two phases can be told
-- apart by what each one reaches.
importerModule :: String
importerModule =
  unlines
    [ "module Importer (sorted) where",
      "",
      "import Data.List (sort)",
      "import Data.Maybe qualified as M",
      "",
      "sorted :: [Int] -> [Int]",
      "sorted = sort"
    ]

-- | A module that only parses because the corpus is treated as GHC2021 --
-- @ExplicitForAll@ is part of that edition but not of Haskell2010.
topModule :: String
topModule =
  unlines
    [ "module Top where",
      "",
      "identity :: forall a. a -> a",
      "identity x = x"
    ]

sourceFile :: FilePath -> String -> SourceFile
sourceFile name contents =
  SourceFile
    { sourceFileName = name,
      sourceFileText = T.pack contents,
      sourceFileBytes = length contents
    }

withCorpus :: (FilePath -> IO a) -> IO a
withCorpus act =
  bracket acquire removeDirectoryRecursive $ \root -> do
    createDirectoryIfMissing True (root </> "Nested")
    writeFile (root </> "Top.hs") topModule
    writeFile (root </> "Nested" </> "Deep.hs") "module Nested.Deep where\ny :: Int\ny = 1\n"
    writeFile (root </> "aihc-base.cabal") "name: aihc-base\n"
    writeFile (root </> "Nested" </> "notes.txt") "not Haskell\n"
    act root
  where
    acquire = do
      tmp <- getTemporaryDirectory
      let root = tmp </> "aihc-base-corpus-test"
      createDirectoryIfMissing True root
      pure root
