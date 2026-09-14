-- | A small, fixed-corpus parser benchmark.
--
-- Unlike the Stackage benchmark in "Aihc.Parser.Bench.Benchmark", this one
-- runs over a single pinned source tree — @core-libs/aihc-base@ from the
-- @aihc@ repository, fetched at an exact commit by the flake — and fully
-- forces every 'Aihc.Parser.Syntax.Module' it produces with 'rnf'.  It is
-- deliberately sized so that one iteration takes a fraction of a second,
-- which makes it usable as an inner loop for optimisation work.
--
-- None of the sources use CPP, so no preprocessor runs and the measured time
-- is lexing plus parsing plus the cost of forcing the resulting tree.
module Aihc.Parser.Bench.AihcBase
  ( -- * Corpus
    SourceFile (..),
    findHaskellFiles,
    loadCorpus,

    -- * Running
    runAihcBaseBenchmark,
    parseAndForce,
  )
where

import Aihc.Parser qualified as Aihc
import Aihc.Parser.Bench.Benchmark
  ( BenchmarkResult (..),
    GCStatsSnapshot (..),
    IterationResult (..),
  )
import Aihc.Parser.Bench.CLI (AihcBaseOptions (..))
import Aihc.Parser.Bench.Parsers (prepareSourceAndExtensionsWithCpp)
import Control.DeepSeq (NFData (..), force, rnf)
import Control.Exception (evaluate)
import Control.Monad (filterM, replicateM, unless)
import Data.ByteString qualified as BS
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats qualified as Stats
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (makeRelative, takeExtension, (</>))
import System.IO (hFlush, hPutStr, hPutStrLn, stderr)

-- | One Haskell source file from the corpus, already decoded.
data SourceFile = SourceFile
  { -- | Path relative to the corpus root, used as the parser's source name.
    sourceFileName :: !FilePath,
    sourceFileText :: !Text,
    sourceFileBytes :: !Int
  }

instance NFData SourceFile where
  rnf (SourceFile n t b) = rnf n `seq` rnf t `seq` rnf b

-- | @aihc-base@ is built with these, so the benchmark parses with them too.
-- The language edition supplies the bulk of the extension set; per-file
-- @LANGUAGE@ pragmas are picked up during the timed run, as they would be by
-- any real consumer.
corpusCabalExtensions :: [String]
corpusCabalExtensions = ["NoImplicitPrelude"]

corpusLanguage :: Maybe String
corpusLanguage = Just "GHC2021"

-- | Recursively collect @.hs@ files under a directory, sorted so that the
-- corpus order is stable across machines.
findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles root = sort <$> go root
  where
    go dir = do
      names <- listDirectory dir
      let entries = map (dir </>) (sort names)
      dirs <- filterM doesDirectoryExist entries
      files <- filterM doesFileExist entries
      nested <- concat <$> mapM go dirs
      pure ([f | f <- files, takeExtension f == ".hs"] ++ nested)

-- | Read every @.hs@ file under the corpus root into memory.
loadCorpus :: FilePath -> IO [SourceFile]
loadCorpus root = do
  paths <- findHaskellFiles root
  mapM readSourceFile paths
  where
    readSourceFile path = do
      bytes <- BS.readFile path
      pure
        SourceFile
          { sourceFileName = makeRelative root path,
            sourceFileText = TE.decodeUtf8 bytes,
            sourceFileBytes = BS.length bytes
          }

-- | Parse one file and force the resulting module with 'rnf'.
--
-- Returns 'Nothing' on success, or the formatted parse errors on failure.
-- Extension resolution happens inside the timed section on purpose: scanning
-- the module header is part of what it costs to parse a file.
parseAndForce :: SourceFile -> Maybe String
parseAndForce file =
  let (source, extensions) =
        prepareSourceAndExtensionsWithCpp
          True -- no CPP: aihc-base does not use it
          Map.empty
          (sourceFileName file)
          corpusCabalExtensions
          []
          corpusLanguage
          []
          (sourceFileText file)
      config =
        Aihc.defaultConfig
          { Aihc.parserSourceName = sourceFileName file,
            Aihc.parserExtensions = extensions
          }
      (errs, m) = Aihc.parseModule config source
   in rnf m `seq`
        if null errs
          then Nothing
          else Just (Aihc.formatParseErrors (sourceFileName file) (Just source) errs)

-- | Load the corpus, then time repeated parse-and-force passes over it.
runAihcBaseBenchmark :: AihcBaseOptions -> IO (BenchmarkResult, [(FilePath, String)])
runAihcBaseBenchmark opts = do
  let root = aihcBaseSource opts
  exists <- doesDirectoryExist root
  unless exists $ fail ("Corpus directory does not exist: " ++ root)

  hPutStr stderr $ "Loading corpus from " ++ root ++ " ..."
  hFlush stderr
  corpus <- force <$> loadCorpus root
  _ <- evaluate (rnf corpus)
  hPutStrLn stderr $ " " ++ show (length corpus) ++ " Haskell files"

  hPutStr stderr "Checking that every file parses..."
  hFlush stderr
  failures <-
    evaluate $
      force
        [ (sourceFileName f, err)
        | f <- corpus,
          Just err <- [parseAndForce f]
        ]
  hPutStrLn stderr $ " " ++ show (length failures) ++ " failure(s)"

  hPutStrLn stderr $ "Running " ++ show (aihcBaseWarmup opts) ++ " warmup iteration(s)..."
  warmups <- replicateM (aihcBaseWarmup opts) (reportIteration "Warmup" corpus)

  gcBefore <- captureGCStatsIf (aihcBaseGcStats opts)

  hPutStrLn stderr $ "Running " ++ show (aihcBaseIterations opts) ++ " benchmark iteration(s)..."
  mains <- replicateM (aihcBaseIterations opts) (reportIteration "Iteration" corpus)

  gcAfter <- captureGCStatsIf (aihcBaseGcStats opts)

  pure
    ( BenchmarkResult
        { benchWarmupResults = warmups,
          benchMainResults = mains,
          benchGcBefore = gcBefore,
          benchGcAfter = gcAfter
        },
      failures
    )
  where
    reportIteration label corpus = do
      result <- runIteration corpus
      hPutStrLn stderr $
        "  "
          ++ label
          ++ ": "
          ++ show (iterFilesRead result)
          ++ " files, "
          ++ show (fromIntegral (iterWallTimeNs result) / 1e6 :: Double)
          ++ "ms"
      pure result

-- | One timed pass: parse every file and force every tree.
runIteration :: [SourceFile] -> IO IterationResult
runIteration corpus = do
  start <- getMonotonicTimeNSec
  let results = map parseAndForce corpus
  _ <- evaluate (rnf results)
  end <- getMonotonicTimeNSec
  let failed = length [() | Just _ <- results]
  pure
    IterationResult
      { iterWallTimeNs = fromIntegral (end - start),
        iterBytesRead = sum (map (fromIntegral . sourceFileBytes) corpus),
        iterFilesRead = length corpus,
        iterParseSuccess = length corpus - failed,
        iterParseFailed = failed
      }

captureGCStatsIf :: Bool -> IO (Maybe GCStatsSnapshot)
captureGCStatsIf False = pure Nothing
captureGCStatsIf True = do
  enabled <- Stats.getRTSStatsEnabled
  if enabled
    then do
      stats <- Stats.getRTSStats
      pure $
        Just
          GCStatsSnapshot
            { gcBytesAllocated = fromIntegral (Stats.allocated_bytes stats),
              gcBytesCopied = fromIntegral (Stats.copied_bytes stats),
              gcMaxLiveBytes = fromIntegral (Stats.max_live_bytes stats),
              gcGcCpuNs = fromIntegral (Stats.gc_cpu_ns stats),
              gcGcElapsedNs = fromIntegral (Stats.gc_elapsed_ns stats),
              gcNumGcs = fromIntegral (Stats.gcs stats)
            }
    else do
      hPutStrLn stderr "Warning: GC stats requested but RTS stats not enabled. Run with +RTS -T"
      pure Nothing
