-- | A small, fixed-corpus parser benchmark.
--
-- Unlike the Stackage benchmark in "Aihc.Parser.Bench.Benchmark", this one
-- runs over a single pinned source tree — @core-libs/aihc-base@ from the
-- @aihc@ repository, fetched at an exact commit by the flake — and fully
-- forces every 'Aihc.Parser.Syntax.Module' it produces with 'rnf'.  It is
-- deliberately sized so that one iteration takes a fraction of a second,
-- which makes it usable as an inner loop for optimisation work.
--
-- The forcing happens in two phases, mirroring how a compiler front end
-- consumes a batch of modules.  The first phase forces the module head and
-- the import list of /every/ module in the corpus, which is what a driver
-- needs before it can build a dependency graph.  Only then does the second
-- phase force the declarations and the parse errors.  Every parse tree is
-- therefore live across the whole iteration, so peak heap reflects holding
-- the batch rather than one module at a time.
--
-- Each timed iteration starts from the corpus root on disk: it scans the
-- directory tree for @.hs@ files, reads and decodes them, and only then
-- parses.  A compiler front end pays for that file IO on every build, so the
-- benchmark charges for it too rather than measuring against a corpus that is
-- already in memory.  None of the sources use CPP, so no preprocessor runs
-- and the measured time is file IO plus lexing plus parsing plus the cost of
-- forcing the resulting tree.
module Aihc.Parser.Bench.AihcBase
  ( -- * Corpus
    SourceFile (..),
    findHaskellFiles,
    loadCorpus,

    -- * Parsing and forcing
    ParsedModule (..),
    parseSourceFile,
    forceModuleHeader,
    forceModuleBody,
    parseAndForce,

    -- * Running
    runAihcBaseBenchmark,
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
import Aihc.Parser.Syntax
  ( Module (..),
    SourceSpan,
  )
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
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, makeAbsolute)
import System.FilePath (takeExtension, (</>))
import System.IO (hFlush, hPutStr, hPutStrLn, stderr)

-- | One Haskell source file from the corpus, already decoded.
data SourceFile = SourceFile
  { -- | Absolute path of the file, used as the parser's source name.
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
--
-- The root is made absolute first, so every 'sourceFileName' is an absolute
-- path however the root was given on the command line.
loadCorpus :: FilePath -> IO [SourceFile]
loadCorpus root = do
  absoluteRoot <- makeAbsolute root
  paths <- findHaskellFiles absoluteRoot
  mapM readSourceFile paths
  where
    readSourceFile path = do
      bytes <- BS.readFile path
      pure
        SourceFile
          { sourceFileName = path,
            sourceFileText = TE.decodeUtf8 bytes,
            sourceFileBytes = BS.length bytes
          }

-- | A parsed file, held live between the two forcing phases.
data ParsedModule = ParsedModule
  { parsedFileName :: !FilePath,
    -- | Kept so that a failure can be reported with source context.
    parsedSource :: Text,
    parsedErrors :: [(Maybe SourceSpan, Text)],
    parsedTree :: Module
  }

-- | Parse one file, without forcing anything beyond what producing the
-- top-level result requires.
--
-- Extension resolution is part of this on purpose: scanning the module header
-- is part of what it costs to parse a file, so it belongs inside the timed
-- section.
parseSourceFile :: SourceFile -> ParsedModule
parseSourceFile file =
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
   in ParsedModule
        { parsedFileName = sourceFileName file,
          parsedSource = source,
          parsedErrors = errs,
          parsedTree = m
        }

-- | Phase one: the module head — its name and export list — and the import
-- list.  This is what a driver needs from every module before it can order
-- the batch.
forceModuleHeader :: ParsedModule -> ()
forceModuleHeader p =
  rnf (moduleHead tree) `seq` rnf (moduleImports tree)
  where
    tree = parsedTree p

-- | Phase two: everything the header phase left alone, plus the parse errors.
forceModuleBody :: ParsedModule -> ()
forceModuleBody p =
  rnf (moduleAnns tree) `seq`
    rnf (moduleLanguagePragmas tree) `seq`
      rnf (moduleDecls tree) `seq`
        rnf (parsedErrors p)
  where
    tree = parsedTree p

-- | The formatted parse errors for a module, or 'Nothing' if it parsed.
moduleFailure :: ParsedModule -> Maybe String
moduleFailure p
  | null (parsedErrors p) = Nothing
  | otherwise =
      Just
        ( Aihc.formatParseErrors
            (parsedFileName p)
            (Just (parsedSource p))
            (parsedErrors p)
        )

-- | Parse one file and force it completely, header phase first.
--
-- Returns 'Nothing' on success, or the formatted parse errors on failure.
-- This is the single-module equivalent of what an iteration does across the
-- whole corpus; the benchmark itself interleaves the phases differently.
parseAndForce :: SourceFile -> Maybe String
parseAndForce file =
  forceModuleHeader parsed `seq` forceModuleBody parsed `seq` moduleFailure parsed
  where
    parsed = parseSourceFile file

-- | Check that the corpus parses, then time repeated load-parse-and-force
-- passes over it.
runAihcBaseBenchmark :: AihcBaseOptions -> IO (BenchmarkResult, [(FilePath, String)])
runAihcBaseBenchmark opts = do
  root <- makeAbsolute (aihcBaseSource opts)
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
  warmups <- replicateM (aihcBaseWarmup opts) (reportIteration "Warmup" root)

  gcBefore <- captureGCStatsIf (aihcBaseGcStats opts)

  hPutStrLn stderr $ "Running " ++ show (aihcBaseIterations opts) ++ " benchmark iteration(s)..."
  mains <- replicateM (aihcBaseIterations opts) (reportIteration "Iteration" root)

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
    reportIteration label root = do
      result <- runIteration root
      hPutStrLn stderr $
        "  "
          ++ label
          ++ ": "
          ++ show (iterFilesRead result)
          ++ " files, "
          ++ show (fromIntegral (iterWallTimeNs result) / 1e6 :: Double)
          ++ "ms"
      pure result

-- | One timed pass over the corpus, starting from the directory on disk.
--
-- The clock starts before the directory scan, so finding, reading and
-- decoding the files is part of the measured time.  Phase one then forces the
-- head and imports of every module before phase two touches any declaration,
-- so the whole batch of parse trees stays reachable until the iteration ends.
runIteration :: FilePath -> IO IterationResult
runIteration root = do
  start <- getMonotonicTimeNSec
  corpus <- loadCorpus root
  let parsed = map parseSourceFile corpus
  _ <- evaluate (forceAll forceModuleHeader parsed)
  _ <- evaluate (forceAll forceModuleBody parsed)
  end <- getMonotonicTimeNSec
  let failed = length [() | p <- parsed, not (null (parsedErrors p))]
  pure
    IterationResult
      { iterWallTimeNs = fromIntegral (end - start),
        iterBytesRead = sum (map (fromIntegral . sourceFileBytes) corpus),
        iterFilesRead = length corpus,
        iterParseSuccess = length corpus - failed,
        iterParseFailed = failed
      }

-- | Apply a forcing function to every element, forcing the list spine too.
forceAll :: (a -> ()) -> [a] -> ()
forceAll f = foldr (\x rest -> f x `seq` rest) ()

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
