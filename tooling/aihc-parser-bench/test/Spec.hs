module Main (main) where

import Test.ParserBenchCoverage (parserBenchCoverageTests)
import Test.ParserBenchPipeline (parserBenchPipelineTests)
import Test.ParserBenchReport (parserBenchReportTests)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main =
  defaultMain $
    testGroup
      "aihc-parser-bench"
      [ parserBenchReportTests,
        parserBenchCoverageTests,
        parserBenchPipelineTests,
        -- Present so that the suite accepts the --quickcheck-* flags that
        -- `just check` passes to every test suite in the project.
        QC.testProperty "dummy quickcheck property" prop_dummy
      ]

prop_dummy :: Bool
prop_dummy = True
