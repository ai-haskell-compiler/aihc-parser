module Test.AihcBaseBench
  ( aihcBaseBenchTests,
  )
where

import Aihc.Parser.Bench.AihcBase
  ( SourceFile (..),
    findHaskellFiles,
    loadCorpus,
    parseAndForce,
  )
import Control.Exception (bracket)
import Data.List (isInfixOf)
import Data.Text qualified as T
import System.Directory
  ( createDirectoryIfMissing,
    getTemporaryDirectory,
    removeDirectoryRecursive,
  )
import System.FilePath ((</>))
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
      testCase "loads each file with a corpus-relative name" $
        withCorpus $ \root -> do
          corpus <- loadCorpus root
          assertEqual
            "relative names"
            ["Nested/Deep.hs", "Top.hs"]
            (map sourceFileName corpus),
      testCase "parses GHC2021 sources that need no per-file pragma" $
        assertEqual "no errors" Nothing (parseAndForce (sourceFile "Top.hs" topModule)),
      testCase "reports the failing file when a source does not parse" $ do
        let result = parseAndForce (sourceFile "Broken.hs" "module Broken where\nx = (\n")
        assertBool "names the file" (maybe False ("Broken.hs" `isInfixOf`) result)
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
