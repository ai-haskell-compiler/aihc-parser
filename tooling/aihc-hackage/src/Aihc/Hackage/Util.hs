{-# LANGUAGE OverloadedStrings #-}

-- | Shared file-system utilities for Hackage package processing.
module Aihc.Hackage.Util
  ( readTextFileLenient,
    normalizeSourceForParser,
    existingPaths,
    dedupeExistingFiles,
    findCabalFiles,
    chooseBestCabalFile,
    moduleFilesForBuildInfo,
    sourceDirs,
  )
where

import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.List (isPrefixOf, isSuffixOf, nub, sortOn)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Distribution.ModuleName (ModuleName, toFilePath)
import Distribution.PackageDescription (BuildInfo, hsSourceDirs)
import Distribution.Utils.Path (getSymbolicPath)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
import System.FilePath (makeRelative, normalise, splitDirectories, takeExtension, takeFileName, (<.>), (</>))

-- | Read a file as 'Text' with lenient UTF-8 decoding.
readTextFileLenient :: FilePath -> IO Text
readTextFileLenient filePath = do
  bytes <- BS.readFile filePath
  pure (decodeUtf8With lenientDecode bytes)

-- | Prepare a source file for a Haskell parser: drop a leading byte order
-- mark and, for @.lhs@ files, strip the literate markup the way GHC's
-- unlit step does.
normalizeSourceForParser :: FilePath -> Text -> Text
normalizeSourceForParser inputFile =
  unliterateIfNeeded inputFile . stripLeadingBom

stripLeadingBom :: Text -> Text
stripLeadingBom txt =
  fromMaybe txt (T.stripPrefix "\xfeff" txt)

unliterateIfNeeded :: FilePath -> Text -> Text
unliterateIfNeeded inputFile source
  | map toLower (takeExtension inputFile) /= ".lhs" = source
  | otherwise =
      let ls = T.lines source
       in if any (\line -> T.strip line == "\\begin{code}") ls
            then T.unlines (unlitLatex False ls)
            else T.unlines (map unlitBirdLine ls)
  where
    -- Replace the leading '>' with a space instead of stripping it.
    -- This preserves original column positions, which is critical for
    -- layout-sensitive parsing when tabs are present.  Stripping "> "
    -- shifts columns by 2, but tab stops depend on absolute column
    -- position, so tab-aligned code and space-aligned code would end
    -- up at different columns after the shift.
    unlitBirdLine line =
      case T.stripPrefix ">" line of
        Just rest -> " " <> rest
        Nothing -> ""

    unlitLatex _ [] = []
    unlitLatex inCode (line : rest)
      | T.strip line == "\\begin{code}" = "" : unlitLatex True rest
      | T.strip line == "\\end{code}" = "" : unlitLatex False rest
      | inCode = line : unlitLatex inCode rest
      | otherwise = "" : unlitLatex inCode rest

-- | Return only the paths that exist on disk, normalised.
existingPaths :: [FilePath] -> IO [FilePath]
existingPaths candidates = do
  existing <- forM candidates $ \candidate -> do
    fileExists <- doesFileExist candidate
    pure (if fileExists then Just (normalise candidate) else Nothing)
  pure (catMaybes existing)

-- | Deduplicate and filter to existing files.
dedupeExistingFiles :: [FilePath] -> IO [FilePath]
dedupeExistingFiles files = fmap nub (existingPaths files)

-- | Find all @.cabal@ files under a directory (recursive, skips @.git@).
findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles dir = do
  entries <- listDirectory dir
  paths <- fmap concat $
    forM entries $ \entry -> do
      let fullPath = dir </> entry
      isDir <- doesDirectoryExist fullPath
      if isDir
        then
          if ".git" `isPrefixOf` entry
            then pure []
            else findCabalFiles fullPath
        else
          if ".cabal" `isSuffixOf` entry
            then pure [fullPath]
            else pure []
  pure (nub (map normalise paths))

-- | When multiple @.cabal@ files are found, pick the \"best\" one.
--
-- Heuristic: prefer files closer to the root and outside test\/example
-- directories.
chooseBestCabalFile :: FilePath -> [FilePath] -> FilePath
chooseBestCabalFile extractedRoot files =
  case sortOn rank files of
    best : _ -> best
    [] -> error ("chooseBestCabalFile: no .cabal files found under " ++ extractedRoot)
  where
    rank file =
      let rel = splitDirectories (makeRelative extractedRoot file)
          dirParts = case reverse rel of
            _fileName : restRev -> reverse restRev
            [] -> []
          lowerDirParts = map (map toLower) dirParts
          isLikelyFixtureDir = any (`elem` fixtureDirNames) lowerDirParts
       in ( if isLikelyFixtureDir then (1 :: Int) else 0,
            length rel,
            length dirParts,
            scoreByFileName (map toLower (takeFileName file)),
            file
          )

    scoreByFileName fileNameLower
      | "test-" `isPrefixOf` fileNameLower = 1 :: Int
      | "example-" `isPrefixOf` fileNameLower = 1
      | otherwise = 0

    fixtureDirNames =
      [ "test",
        "tests",
        "testing",
        "example",
        "examples",
        "benchmark",
        "benchmarks"
      ]

-- | Resolve module names to existing source files for a 'BuildInfo'.
moduleFilesForBuildInfo :: FilePath -> BuildInfo -> [ModuleName] -> IO [FilePath]
moduleFilesForBuildInfo packageRoot build modules = do
  let dirs = sourceDirs packageRoot build
      moduleCandidates =
        [ dir </> toFilePath modu <.> ext
        | dir <- dirs,
          modu <- modules,
          ext <- ["hs", "lhs"]
        ]
  dedupeExistingFiles moduleCandidates

-- | Compute source directories from a 'BuildInfo'.
sourceDirs :: FilePath -> BuildInfo -> [FilePath]
sourceDirs packageRoot build =
  case map getSymbolicPath (hsSourceDirs build) of
    [] -> [packageRoot]
    dirs -> [packageRoot </> dir | dir <- dirs]
