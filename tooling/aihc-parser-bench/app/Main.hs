-- | Entry point for @aihc-parser-bench@.
module Main (main) where

import Aihc.Parser.Bench.CLI (parseOptionsIO)
import Aihc.Parser.Bench.Run (run)

main :: IO ()
main = parseOptionsIO >>= run
