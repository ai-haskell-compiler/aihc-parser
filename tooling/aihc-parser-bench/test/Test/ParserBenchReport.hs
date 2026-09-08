module Test.ParserBenchReport
  ( parserBenchReportTests,
  )
where

import Aihc.Parser.Bench.Report (ParserResult (..), renderParserRatioRow)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

parserBenchReportTests :: TestTree
parserBenchReportTests =
  testGroup
    "parser benchmark report"
    [ testCase "renders time and memory ratios so that lower is better" $ do
        let ghc = ParserResult "GHC (`ghc-lib-parser`)" 200 400 600
            aihc = ParserResult "AIHC" 100 100 300
        assertEqual
          "report row"
          "| AIHC | `0.50x` | `0.25x` | `0.50x` |"
          (renderParserRatioRow ghc aihc),
      testCase "renders the baseline as 1.00x in every column" $ do
        let ghc = ParserResult "GHC (`ghc-lib-parser`)" 200 400 600
        assertEqual
          "report row"
          "| GHC (`ghc-lib-parser`) | `1.00x` | `1.00x` | `1.00x` |"
          (renderParserRatioRow ghc ghc),
      testCase "keeps significant digits for ratios far below one" $ do
        let ghc = ParserResult "GHC (`ghc-lib-parser`)" 100000 100000 100000
            aihc = ParserResult "AIHC" 2000 500 40
        assertEqual
          "report row"
          "| AIHC | `0.020x` | `0.0050x` | `0.0004x` |"
          (renderParserRatioRow ghc aihc)
    ]
