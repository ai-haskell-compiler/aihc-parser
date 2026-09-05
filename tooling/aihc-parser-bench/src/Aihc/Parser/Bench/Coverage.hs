{-# LANGUAGE ScopedTypeVariables #-}

-- | Stackage parse coverage for @aihc-parser@.
--
-- Walks every package of a Stackage snapshot and records whether
-- @aihc-parser@ accepts all of the sources the package's @.cabal@ file
-- declares. Only a verdict is kept per package, so the whole snapshot
-- fits in memory.
module Aihc.Parser.Bench.Coverage
  ( CoverageResult (..),
    measureCoverage,
    coverageChecked,
    coveragePercentage,
    formatCoverageSummary,
  )
where

import Aihc.Hackage.Cabal qualified as HC
import Aihc.Hackage.Download qualified as HD
import Aihc.Hackage.Stackage qualified as HS
import Aihc.Hackage.Types (PackageSpec (..), formatPackage)
import Aihc.Hackage.Util qualified as HU
import Aihc.Parser.Bench.CLI (CoverageOptions (..))
import Aihc.Parser.Bench.Parsers (ParseResult (..), collectCppIncludes, parseWithAihcExts)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (foldM, when)
import Data.ByteString qualified as BS
import Data.List (isSuffixOf)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Distribution.PackageDescription.Parsec qualified as Cabal
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

-- | Outcome of a coverage run.
data CoverageResult = CoverageResult
  { -- | Packages listed in the snapshot.
    coverageTotal :: !Int,
    -- | Packages whose declared sources all parsed.
    coverageParsed :: !Int,
    -- | Packages with at least one file @aihc-parser@ rejected, with the
    -- first rejected file and its error.
    coverageFailed :: ![(PackageSpec, FilePath, String)],
    -- | Packages that could not be checked at all: download failures,
    -- unparseable @.cabal@ files, or no declared sources.
    coverageSkipped :: ![(PackageSpec, String)]
  }
  deriving (Show)

-- | Packages that were actually parsed, successfully or not.
coverageChecked :: CoverageResult -> Int
coverageChecked result = coverageParsed result + length (coverageFailed result)

-- | Percentage of the checked packages that parsed.
coveragePercentage :: CoverageResult -> Double
coveragePercentage result
  | checked <= 0 = 0
  | otherwise = fromIntegral (coverageParsed result) * 100 / fromIntegral checked
  where
    checked = coverageChecked result

-- | Measure how much of a Stackage snapshot @aihc-parser@ accepts.
measureCoverage :: CoverageOptions -> IO (Either String CoverageResult)
measureCoverage opts = do
  manager <- newManager tlsManagerSettings
  snapshot <- HS.loadStackageSnapshot (Just manager) (coverageSnapshot opts) (coverageOffline opts)
  case snapshot of
    Left err -> pure (Left err)
    Right packages -> do
      when (coverageVerbose opts) $
        hPutStrLn stderr $
          "Checking " ++ show (length packages) ++ " packages from " ++ coverageSnapshot opts
      let start =
            CoverageResult
              { coverageTotal = length packages,
                coverageParsed = 0,
                coverageFailed = [],
                coverageSkipped = []
              }
      result <- foldM (checkOne opts manager) start packages
      pure $
        Right
          result
            { coverageFailed = reverse (coverageFailed result),
              coverageSkipped = reverse (coverageSkipped result)
            }

checkOne :: CoverageOptions -> Manager -> CoverageResult -> PackageSpec -> IO CoverageResult
checkOne opts manager acc pkg = do
  outcome <- checkPackage opts manager pkg
  case outcome of
    Right Nothing -> pure $! acc {coverageParsed = coverageParsed acc + 1}
    Right (Just (path, err)) -> do
      when (coverageVerbose opts) $
        hPutStrLn stderr ("  parse failure: " ++ formatPackage pkg ++ ": " ++ path)
      pure $! acc {coverageFailed = (pkg, path, err) : coverageFailed acc}
    Left reason -> do
      when (coverageVerbose opts) $
        hPutStrLn stderr ("  skipped: " ++ formatPackage pkg ++ ": " ++ reason)
      pure $! acc {coverageSkipped = (pkg, reason) : coverageSkipped acc}

-- | Parse every source file a package declares. @Left@ means the package
-- could not be checked, @Right Nothing@ that everything parsed, and
-- @Right (Just failure)@ that a file was rejected.
checkPackage :: CoverageOptions -> Manager -> PackageSpec -> IO (Either String (Maybe (FilePath, String)))
checkPackage opts manager pkg = do
  let dlOpts =
        HD.defaultDownloadOptions
          { HD.downloadVerbose = False,
            HD.downloadAllowNetwork = not (coverageOffline opts),
            HD.downloadManager = Just manager
          }
  downloaded <- try $ HD.downloadPackageWithOptions dlOpts pkg
  case downloaded of
    Left (err :: SomeException) -> pure (Left ("download failed: " ++ displayException err))
    Right pkgDir -> do
      collected <- try $ declaredFiles pkgDir
      case collected of
        Left (err :: SomeException) -> pure (Left ("cabal file parse failed: " ++ displayException err))
        Right [] -> pure (Left "no declared Haskell sources")
        Right files -> Right <$> firstFailure pkgDir pkg files

-- | The source files a package's @.cabal@ file declares for its buildable
-- library and executable components. Stray sources such as @Setup.hs@ are
-- not part of the package's own build, so they are not counted.
declaredFiles :: FilePath -> IO [HC.FileInfo]
declaredFiles pkgDir = do
  cabalFile <- findCabalFile pkgDir
  cabalBytes <- BS.readFile cabalFile
  case snd (Cabal.runParseResult (Cabal.parseGenericPackageDescription cabalBytes)) of
    Left errs -> ioError (userError ("failed to parse " ++ cabalFile ++ ": " ++ show errs))
    Right gpd -> HC.collectComponentFiles gpd (takeDirectory cabalFile)

findCabalFile :: FilePath -> IO FilePath
findCabalFile pkgDir = do
  entries <- listDirectory pkgDir
  cabalFiles <- filterFiles [pkgDir </> e | e <- entries, ".cabal" `isSuffixOf` e]
  case cabalFiles of
    (f : _) -> pure f
    [] -> ioError (userError ("No .cabal file found in " ++ pkgDir))
  where
    filterFiles paths = do
      flags <- mapM doesDirectoryExist paths
      pure [p | (p, isDir) <- zip paths flags, not isDir]

firstFailure :: FilePath -> PackageSpec -> [HC.FileInfo] -> IO (Maybe (FilePath, String))
firstFailure pkgDir pkg = go
  where
    go [] = pure Nothing
    go (info : rest) = do
      outcome <- checkFile pkgDir pkg info
      case outcome of
        Nothing -> go rest
        failure -> pure failure

checkFile :: FilePath -> PackageSpec -> HC.FileInfo -> IO (Maybe (FilePath, String))
checkFile pkgDir pkg info = do
  let absFile = HC.fileInfoPath info
      -- Include maps are keyed the way the tarball corpus keys them, so
      -- that CPP include resolution behaves identically here.
      relPath = formatPackage pkg </> makeRelative pkgDir absFile
      exts = HC.fileInfoExtensions info
      cppOpts = HC.fileInfoCppOptions info
      lang = HC.fileInfoLanguage info
      deps = HC.fileInfoDependencies info
  source <- HU.readTextFileLenient absFile
  includes <- collectCppIncludes absFile exts cppOpts lang deps source
  let includeMap = includeEntryMap pkgDir pkg includes
  pure $ case parseWithAihcExts includeMap relPath exts cppOpts lang deps source of
    ParseSuccess -> Nothing
    ParseFailure err -> Just (relPath, err)

includeEntryMap :: FilePath -> PackageSpec -> [(FilePath, Text)] -> Map.Map FilePath Text
includeEntryMap pkgDir pkg includes =
  Map.fromList [(formatPackage pkg </> makeRelative pkgDir path, contents) | (path, contents) <- includes]

-- | Render the summary. The @AIHC:@ line is what
-- @scripts/update-generated-content.sh@ parses, so keep its shape stable.
formatCoverageSummary :: String -> CoverageResult -> [String]
formatCoverageSummary snapshot result =
  [ "Coverage Summary",
    "================",
    "Snapshot:           " ++ snapshot,
    "Snapshot packages:  " ++ show (coverageTotal result),
    "Checked packages:   " ++ show (coverageChecked result),
    "Parse failures:     " ++ show (length (coverageFailed result)),
    "Skipped packages:   " ++ show (length (coverageSkipped result)),
    printf
      "AIHC: %d / %d (%.2f%%)"
      (coverageParsed result)
      (coverageChecked result)
      (coveragePercentage result)
  ]
