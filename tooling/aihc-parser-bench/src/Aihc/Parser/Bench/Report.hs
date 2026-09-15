{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Markdown report generation for relative Stackage parser benchmarks.
module Aihc.Parser.Bench.Report
  ( ParserResult (..),
    renderParserRatioRow,
    runParserMeasurement,
    runReport,
  )
where

import Aihc.Parser.Bench.CLI
  ( FilterOptions (..),
    GenerateOptions (..),
    MeasureOptions (..),
    ParserChoice (..),
    ReportOptions (..),
  )
import Aihc.Parser.Bench.Parsers
  ( ParseResult (..),
    parseWithAihcExtsWithCpp,
    parseWithGhcExtsWithCpp,
    prepareSourceAndExtensionsWithCpp,
  )
import Aihc.Parser.Bench.Tarball
  ( TarballEntry (..),
    generateTarballEntries,
    isHaskellEntry,
    isIncludeEntry,
  )
import Aihc.Parser.Syntax qualified as Syntax
import Control.DeepSeq (deepseq)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats qualified as Stats
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)
import System.Mem (performMajorGC)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)
import Text.Read (readMaybe)

data Corpus = Corpus
  { corpusHaskellEntries :: ![TarballEntry],
    corpusIncludeMap :: !(Map.Map FilePath Text),
    corpusPackageCount :: !Int,
    corpusFileCount :: !Int,
    corpusCppFileCount :: !Int,
    corpusByteCount :: !Integer
  }

data Timed a = Timed
  { timedResult :: !a,
    timedNanos :: !Integer
  }

data ParserResult = ParserResult
  { parserName :: !String,
    parserNanos :: !Integer,
    parserAllocatedBytes :: !Integer,
    parserPeakHeapBytes :: !Integer
  }
  deriving (Read, Show)

runReport :: ReportOptions -> IO ()
runReport opts@ReportOptions {reportOutput} = do
  corpus <- loadCorpus opts
  unless (corpusFileCount corpus > 0) $
    fail "benchmark corpus is empty"

  hPutStrLn stderr "Benchmarking parsers..."
  ghcParser <- benchmarkParserSubprocess opts ParserGhc
  aihcParser <- benchmarkParserSubprocess opts ParserAihc

  commit <- currentCommit
  let markdown =
        renderReport
          opts
          corpus
          commit
          [ghcParser, aihcParser]
  TIO.writeFile reportOutput markdown
  hPutStrLn stderr ("Wrote " ++ reportOutput)

loadCorpus :: ReportOptions -> IO Corpus
loadCorpus ReportOptions {reportSnapshot, reportOffline} = do
  let genOpts =
        GenerateOptions
          { genSnapshot = reportSnapshot,
            genOutput = Nothing,
            genFilters =
              FilterOptions
                { filterAihc = False,
                  filterHse = False,
                  filterGhc = False
                },
            genCacheDir = Nothing,
            genOffline = reportOffline,
            genVerbose = False,
            genDryRun = True,
            genPreprocess = False
          }
  result <- generateTarballEntries genOpts
  case result of
    Left err -> fail err
    Right (entries, _summary) -> do
      let hsEntries = filter isHaskellEntry entries
          includeMap = Map.fromList [(entryFilePath e, entryContents e) | e <- filter isIncludeEntry entries]
          packages = nub [entryPackage e | e <- hsEntries]
      pure
        Corpus
          { corpusHaskellEntries = hsEntries,
            corpusIncludeMap = includeMap,
            corpusPackageCount = length packages,
            corpusFileCount = length hsEntries,
            corpusCppFileCount = length (filter sourceUsesCpp hsEntries),
            corpusByteCount = sum (map (fromIntegral . entryByteSize) hsEntries)
          }

sourceUsesCpp :: TarballEntry -> Bool
sourceUsesCpp entry =
  Syntax.CPP `elem` extensions
  where
    (_, extensions) =
      prepareSourceAndExtensionsWithCpp
        True
        Map.empty
        (entryFilePath entry)
        (entryExtensions entry)
        (entryCppOptions entry)
        (entryLanguage entry)
        (entryDependencies entry)
        (entryContents entry)

preprocessCorpus :: Corpus -> IO [TarballEntry]
preprocessCorpus Corpus {corpusHaskellEntries, corpusIncludeMap} =
  mapM preprocessEntry corpusHaskellEntries
  where
    preprocessEntry entry = do
      let (source, _) =
            prepareSourceAndExtensionsWithCpp
              False
              corpusIncludeMap
              (entryFilePath entry)
              (entryExtensions entry)
              (entryCppOptions entry)
              (entryLanguage entry)
              (entryDependencies entry)
              (entryContents entry)
      evaluate (source `deepseq` entry {entryContents = source, entryByteSize = BS.length (TE.encodeUtf8 source), entryCppOptions = []})

benchmarkParser :: String -> (TarballEntry -> ParseResult) -> [TarballEntry] -> IO ParserResult
benchmarkParser name parser entries = do
  enabled <- Stats.getRTSStatsEnabled
  unless enabled $
    fail "RTS statistics are not enabled for parser measurement"
  performMajorGC
  before <- Stats.getRTSStats
  timed <- timeAction $ evaluateResults (map parser entries)
  performMajorGC
  after <- Stats.getRTSStats
  pure
    ParserResult
      { parserName = name,
        parserNanos = timedNanos timed,
        parserAllocatedBytes = fromIntegral (Stats.allocated_bytes after - Stats.allocated_bytes before),
        parserPeakHeapBytes = fromIntegral (Stats.max_live_bytes after)
      }

benchmarkParserSubprocess :: ReportOptions -> ParserChoice -> IO ParserResult
benchmarkParserSubprocess ReportOptions {reportSnapshot, reportOffline} parser = do
  executable <- getExecutablePath
  let args =
        [ "report-measure",
          "--snapshot",
          reportSnapshot,
          "--parser",
          parserArgument parser
        ]
          ++ ["--offline" | reportOffline]
          ++ ["+RTS", "-T", "-RTS"]
  (exitCode, out, err) <- readProcessWithExitCode executable args ""
  unless (null err) (hPutStrLn stderr err)
  case (exitCode, readMaybe out) of
    (ExitSuccess, Just result) -> pure result
    (ExitSuccess, Nothing) -> fail ("invalid parser measurement output: " ++ out)
    (ExitFailure code, _) -> fail ("parser measurement failed with exit code " ++ show code)

parserArgument :: ParserChoice -> String
parserArgument ParserAihc = "aihc"
parserArgument ParserHse = "hse"
parserArgument ParserGhc = "ghc"

-- | Run one parser measurement. This is called in an isolated subprocess so
-- each parser gets an independent RTS heap high-water mark.
runParserMeasurement :: MeasureOptions -> IO ()
runParserMeasurement MeasureOptions {measureSnapshot, measureOffline, measureParser} = do
  let opts =
        ReportOptions
          { reportSnapshot = measureSnapshot,
            reportOutput = "",
            reportOffline = measureOffline
          }
  corpus <- loadCorpus opts
  unless (corpusFileCount corpus > 0) $
    fail "benchmark corpus is empty"
  hPutStrLn stderr "Preprocessing parser corpus with aihc-cpp..."
  entries <- preprocessCorpus corpus
  result <- case measureParser of
    ParserGhc -> benchmarkParser "GHC (`ghc-lib-parser`)" parseGhc entries
    ParserAihc -> benchmarkParser "AIHC" parseAihc entries
    ParserHse -> fail "haskell-src-exts is not part of the generated parser report"
  print result

evaluateResults :: [ParseResult] -> IO ()
evaluateResults results =
  evaluate (results `deepseq` ())

parseAihc :: TarballEntry -> ParseResult
parseAihc entry =
  parseWithAihcExtsWithCpp
    True
    Map.empty
    (entryFilePath entry)
    (entryExtensions entry)
    []
    (entryLanguage entry)
    (entryDependencies entry)
    (entryContents entry)

parseGhc :: TarballEntry -> ParseResult
parseGhc entry =
  parseWithGhcExtsWithCpp
    True
    Map.empty
    (entryFilePath entry)
    (entryExtensions entry)
    []
    (entryLanguage entry)
    (entryDependencies entry)
    (entryContents entry)

timeAction :: IO a -> IO (Timed a)
timeAction action = do
  start <- getMonotonicTimeNSec
  result <- action
  end <- getMonotonicTimeNSec
  pure Timed {timedResult = result, timedNanos = fromIntegral (end - start)}

currentCommit :: IO String
currentCommit = do
  result <- try (readProcessWithExitCode "git" ["rev-parse", "HEAD"] "") :: IO (Either SomeException (ExitCode, String, String))
  pure $ case result of
    Right (ExitSuccess, out, _) -> trim out
    _ -> "unknown"

renderReport :: ReportOptions -> Corpus -> String -> [ParserResult] -> Text
renderReport ReportOptions {reportSnapshot} Corpus {corpusPackageCount, corpusFileCount, corpusCppFileCount, corpusByteCount} commit parserResults =
  T.pack $
    unlines $
      [ "# AIHC Benchmarks",
        "",
        "Generated by `nix run .#generate-benchmarks`.",
        "",
        "## Metadata",
        "",
        "| Field | Value |",
        "| --- | --- |",
        "| Stackage snapshot | `" ++ reportSnapshot ++ "` |",
        "| Commit | `" ++ commit ++ "` |",
        "| Packages | `" ++ formatInt corpusPackageCount ++ "` |",
        "| Haskell files | `" ++ formatInt corpusFileCount ++ "` |",
        "| CPP-enabled Haskell files | `" ++ formatInt corpusCppFileCount ++ "` |",
        "| Corpus size | `" ++ formatBytes corpusByteCount ++ "` |",
        "",
        "## Parser Performance",
        "",
        "Parser input is preprocessed with `aihc-cpp` before measurement. GHC is the baseline. Every value is a fraction of GHC's total, so lower is better throughout; peak heap is the RTS maximum live heap.",
        "",
        "Neither parser sets an `-O` level, so both are built at the Cabal default of `-O1` and the time column compares parsers rather than optimisation levels. For reference, `-O2` is worth about 10% to `ghc-lib-parser` and about 6% to `aihc-parser`.",
        "",
        "| Parser | Relative Time | Relative Allocations | Relative Peak Heap |",
        "| --- | ---: | ---: | ---: |"
      ]
        ++ map (renderParserRatioRow (parserBaseline "GHC (`ghc-lib-parser`)" parserResults)) parserResults

renderParserRatioRow :: ParserResult -> ParserResult -> String
renderParserRatioRow baselineResult result =
  "| "
    ++ parserName result
    ++ " | `"
    ++ formatRelative (parserNanos baselineResult) (parserNanos result)
    ++ "` | `"
    ++ formatRelative (parserAllocatedBytes baselineResult) (parserAllocatedBytes result)
    ++ "` | `"
    ++ formatRelative (parserPeakHeapBytes baselineResult) (parserPeakHeapBytes result)
    ++ "` |"

parserBaseline :: String -> [ParserResult] -> ParserResult
parserBaseline name results =
  case [result | result <- results, parserName result == name] of
    result : _ -> result
    [] -> error ("missing benchmark baseline: " ++ name)

-- | Render a measurement as a fraction of the baseline's, so that smaller is
-- better for every column of the report.  Ratios far below one get extra
-- decimals; two would round the smallest ones down to a single digit.
formatRelative :: Integer -> Integer -> String
formatRelative 0 _ = "0.00x"
formatRelative baselineValue candidateValue
  | ratio < 0.01 = printf "%.4fx" ratio
  | ratio < 0.1 = printf "%.3fx" ratio
  | otherwise = printf "%.2fx" ratio
  where
    ratio = fromIntegral candidateValue / fromIntegral baselineValue :: Double

formatInt :: Int -> String
formatInt n
  | n < 1000 = show n
  | otherwise = formatInt (n `div` 1000) ++ "," ++ printf "%03d" (n `mod` 1000)

formatBytes :: Integer -> String
formatBytes bytes =
  let units = ["B", "KB", "MB", "GB", "TB"] :: [String]
      go value (unit : nextUnits)
        | value < 1024 || null nextUnits = printf "%.1f %s" value unit
        | otherwise = go (value / 1024) nextUnits
      go value [] = printf "%.1f B" value
   in if bytes < 1024
        then show bytes ++ " B"
        else go (fromIntegral bytes / 1024 :: Double) (drop 1 units)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'
    dropWhileEnd p = reverse . dropWhile p . reverse
