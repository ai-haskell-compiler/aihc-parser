module CppSupport
  ( preprocessForParser,
    preprocessForParserIfEnabled,
    preprocessForParserWithoutIncludes,
    preprocessForParserWithoutIncludesIfEnabled,
    moduleHeaderExtensionSettings,
    moduleHeaderPragmas,
    cppEnabledInSource,
  )
where

import Aihc.Cpp
  ( Config (..),
    IncludeRequest (..),
    Result (..),
    Step (..),
    defaultConfig,
    preprocess,
  )
import Aihc.Hackage.Cpp qualified as HackageCpp
import Aihc.Hackage.Util (normalizeSourceForParser)
import Aihc.Parser.Syntax (Extension (CPP), ExtensionSetting (..), ModuleHeaderPragmas (..))
import Aihc.Parser.Token (readModuleHeaderExtensions, readModuleHeaderPragmas)
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE

preprocessForParser :: (Monad m) => FilePath -> [Text] -> (IncludeRequest -> m (Maybe ByteString)) -> Text -> m Result
preprocessForParser inputFile deps resolveInclude source =
  preprocessForParserWithCppOptions [] inputFile deps resolveInclude (normalizeSourceForParser inputFile source)

preprocessForParserWithCppOptions :: (Monad m) => [String] -> FilePath -> [Text] -> (IncludeRequest -> m (Maybe ByteString)) -> Text -> m Result
preprocessForParserWithCppOptions cppOptions inputFile deps resolveInclude source = do
  let minVersionMacros = HackageCpp.minVersionMacroNamesFromDeps deps
  let injected = HackageCpp.injectSyntheticCppMacros cppOptions minVersionMacros source
  let cfg =
        defaultConfig
          { configInputFile = inputFile,
            configMacros = HackageCpp.cppMacrosFromOptions cppOptions
          }
  drive (preprocess cfg (TE.encodeUtf8 injected))
  where
    drive (Done result) = pure result
    drive (NeedInclude req k) = resolveInclude req >>= drive . k

preprocessForParserWithoutIncludes :: FilePath -> [Text] -> Text -> Result
preprocessForParserWithoutIncludes inputFile deps source =
  runIdentity (preprocessForParser inputFile deps (\_ -> Identity Nothing) source)

preprocessForParserIfEnabled :: (Monad m) => [ExtensionSetting] -> [String] -> FilePath -> [Text] -> (IncludeRequest -> m (Maybe ByteString)) -> Text -> m Result
preprocessForParserIfEnabled globalExtensionNames cppOptions inputFile deps resolveInclude source =
  let normalizedSource = normalizeSourceForParser inputFile source
      shouldPreprocess = cppEnabledInSourceWithGlobals globalExtensionNames normalizedSource
   in if shouldPreprocess
        then preprocessForParserWithCppOptions cppOptions inputFile deps resolveInclude normalizedSource
        else pure Result {resultOutput = normalizedSource, resultDiagnostics = []}

preprocessForParserWithoutIncludesIfEnabled :: [ExtensionSetting] -> [String] -> FilePath -> [Text] -> Text -> Result
preprocessForParserWithoutIncludesIfEnabled globalExtensionNames cppOptions inputFile deps source =
  runIdentity (preprocessForParserIfEnabled globalExtensionNames cppOptions inputFile deps (\_ -> Identity Nothing) source)

moduleHeaderExtensionSettings :: Text -> [ExtensionSetting]
moduleHeaderExtensionSettings = readModuleHeaderExtensions

moduleHeaderPragmas :: Text -> ModuleHeaderPragmas
moduleHeaderPragmas = readModuleHeaderPragmas

cppEnabledInSource :: Text -> Bool
cppEnabledInSource = cppEnabledInSettings . moduleHeaderExtensionSettings

cppEnabledInSourceWithGlobals :: [ExtensionSetting] -> Text -> Bool
cppEnabledInSourceWithGlobals globalExtensionNames source =
  cppEnabledInSettings globalExtensionNames
    || cppEnabledInSettings (moduleHeaderExtensionSettings source)

cppEnabledInSettings :: [ExtensionSetting] -> Bool
cppEnabledInSettings = foldl apply False
  where
    apply enabled setting =
      case setting of
        EnableExtension CPP -> True
        DisableExtension CPP -> False
        _ -> enabled
