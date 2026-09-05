module Test.ParserBenchCoverage
  ( parserBenchCoverageTests,
  )
where

import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Parser.Bench.Coverage (CoverageResult (..), formatCoverageSummary)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

parserBenchCoverageTests :: TestTree
parserBenchCoverageTests =
  testGroup
    "parser coverage summary"
    [ testCase "reports the parsed ratio over the packages that were checked" $
        -- scripts/update-generated-content.sh parses this line.
        assertEqual
          "summary line"
          "AIHC: 8 / 10 (80.00%)"
          (last (formatCoverageSummary "lts-24.36" sampleResult)),
      testCase "counts skipped packages separately from parse failures" $
        assertEqual
          "counters"
          [ "Snapshot:           lts-24.36",
            "Snapshot packages:  12",
            "Checked packages:   10",
            "Parse failures:     2",
            "Skipped packages:   2"
          ]
          (drop 2 (init (formatCoverageSummary "lts-24.36" sampleResult)))
    ]

sampleResult :: CoverageResult
sampleResult =
  CoverageResult
    { coverageTotal = 12,
      coverageParsed = 8,
      coverageFailed =
        [ (pkg "broken-a", "broken-a-1.0/src/A.hs", "parse error"),
          (pkg "broken-b", "broken-b-1.0/src/B.hs", "parse error")
        ],
      coverageSkipped =
        [ (pkg "gone", "download failed"),
          (pkg "empty", "no Haskell files")
        ]
    }
  where
    pkg name = PackageSpec {pkgName = name, pkgVersion = "1.0"}
