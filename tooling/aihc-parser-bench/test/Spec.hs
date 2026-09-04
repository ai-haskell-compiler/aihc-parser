module Main (main) where

import Test.ParserBenchReport (parserBenchReportTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup "aihc-parser-bench" [parserBenchReportTests]
