{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Parser (ParserConfig (..), defaultConfig, formatParseErrors, parseModule)
import Aihc.Parser.Pretty ()
import Aihc.Parser.Shorthand (Shorthand (shorthand))
import Aihc.Parser.Syntax
  ( Extension (GHC2021, GHC2024, Haskell2010, Haskell98),
    ExtensionSetting (..),
    LanguageEdition (Haskell2010Edition),
    effectiveExtensions,
    parseExtensionName,
    parseExtensionSettingName,
    parseLanguageEdition,
  )
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Options.Applicative qualified as OA
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

data Options = Options
  { optPretty :: Bool,
    optLanguageEdition :: LanguageEdition,
    optExtensionSettings :: [ExtensionSetting]
  }

main :: IO ()
main = do
  opts <- OA.execParser optionsParser
  source <- TIO.getContents
  let extensions =
        effectiveExtensions
          (optLanguageEdition opts)
          (reverse (optExtensionSettings opts))
      config =
        defaultConfig
          { parserSourceName = "<stdin>",
            parserExtensions = extensions
          }
      (errors, modu) = parseModule config source
  if null errors
    then
      TIO.putStrLn . renderStrict . layoutPretty defaultLayoutOptions $
        if optPretty opts then pretty modu else shorthand modu
    else do
      hPutStrLn stderr (formatParseErrors "<stdin>" (Just source) errors)
      exitFailure

optionsParser :: OA.ParserInfo Options
optionsParser =
  OA.info
    (optionsP OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.progDesc "Parse a Haskell module from stdin and print its AST"
        <> OA.header "aihc-parser-dev - inspect the aihc-parser AST"
    )

optionsP :: OA.Parser Options
optionsP =
  Options
    <$> OA.switch
      ( OA.long "pretty"
          <> OA.long "pretty-print"
          <> OA.help "Pretty-print Haskell source instead of rendering the AST"
      )
    <*> OA.option
      languageEditionReader
      ( OA.long "language-edition"
          <> OA.long "language"
          <> OA.metavar "EDITION"
          <> OA.value Haskell2010Edition
          <> OA.showDefaultWith (const "Haskell2010")
          <> OA.help "Language edition: Haskell98, Haskell2010, GHC2021, or GHC2024"
      )
    <*> OA.many extensionSettingOption

extensionSettingOption :: OA.Parser ExtensionSetting
extensionSettingOption =
  OA.option
    extensionSettingReader
    ( OA.short 'X'
        <> OA.metavar "EXTENSION"
        <> OA.help "Enable or disable an extension (for example -XLambdaCase or -XNoImplicitPrelude)"
    )
    OA.<|> OA.option
      enabledExtensionReader
      ( OA.long "enable-extension"
          <> OA.metavar "EXTENSION"
          <> OA.help "Enable a language extension"
      )
    OA.<|> OA.option
      disabledExtensionReader
      ( OA.long "disable-extension"
          <> OA.metavar "EXTENSION"
          <> OA.help "Disable a language extension"
      )

languageEditionReader :: OA.ReadM LanguageEdition
languageEditionReader = OA.eitherReader $ \raw ->
  case parseLanguageEdition (T.pack raw) of
    Just edition -> Right edition
    Nothing -> Left ("unknown language edition: " <> raw)

extensionSettingReader :: OA.ReadM ExtensionSetting
extensionSettingReader = OA.eitherReader $ \raw ->
  case parseExtensionSettingName (T.pack raw) of
    Just setting
      | not (isEditionExtension (settingExtension setting)) -> Right setting
    Just _ -> Left (raw <> " is a language edition; use --language-edition")
    Nothing -> Left ("unknown language extension: " <> raw)

enabledExtensionReader :: OA.ReadM ExtensionSetting
enabledExtensionReader =
  OA.eitherReader $ \raw -> EnableExtension <$> parseCliExtension raw (T.pack raw)

disabledExtensionReader :: OA.ReadM ExtensionSetting
disabledExtensionReader =
  OA.eitherReader $ \raw -> DisableExtension <$> parseCliExtension raw (T.pack raw)

parseCliExtension :: String -> T.Text -> Either String Extension
parseCliExtension raw name =
  case parseExtensionName name of
    Just ext
      | not (isEditionExtension ext) -> Right ext
    Just _ -> Left (raw <> " is a language edition; use --language-edition")
    Nothing -> Left ("unknown language extension: " <> raw)

isEditionExtension :: Extension -> Bool
isEditionExtension ext = ext `elem` [Haskell98, Haskell2010, GHC2021, GHC2024]

settingExtension :: ExtensionSetting -> Extension
settingExtension setting =
  case setting of
    EnableExtension ext -> ext
    DisableExtension ext -> ext
