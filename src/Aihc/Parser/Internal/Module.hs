-- |
-- Module      : Aihc.Parser.Internal.Module
-- Description : Internal module parser
-- License     : Unlicense
--
-- Internal module containing the core module parser.
-- This is used by both 'Aihc.Parser' and 'Aihc.Parser.Internal.FromTokens'.
module Aihc.Parser.Internal.Module
  ( moduleParser,
  )
where

import Aihc.Parser.Internal.Common
  ( TokParser,
    closeAndExpectRBrace,
    eofTok,
    expectedTok,
    inputStartSpan,
    lazy,
    skipSemicolons,
  )
import Aihc.Parser.Internal.Decl (declParser)
import Aihc.Parser.Internal.Import (importDeclParser, languagePragmaParser, moduleHeaderParser)
import Aihc.Parser.Lex (LexToken (lexTokenSpan), LexTokenKind (..), lexTokenKind)
import Aihc.Parser.Syntax (Decl, ImportDecl, Module (..), mergeSourceSpans, mkAnnotation, noSourceSpan)
import Aihc.Parser.Types (TokStream (tokStreamPrevToken))
import Control.Monad (void)
import Text.Megaparsec qualified as MP

data RecoverParseStep a
  = RecoverDone
  | RecoverParsed !a
  | RecoverFailed

-- | Parse the header and imports now, leaving declarations, their errors, and
-- the whole-module span suspended until the corresponding result is demanded.
moduleParser :: TokParser Module
moduleParser = do
  startInput <- MP.getInput
  languagePragmas <- MP.many (languagePragmaParser <* MP.many (expectedTok TkSpecialSemicolon))
  mHeader <- MP.optional (moduleHeaderParser <* MP.many (expectedTok TkSpecialSemicolon))
  expectedTok TkSpecialLBrace
  imports <- importDeclsWithRecovery
  (decls, finalState) <-
    lazy
      ( declsWithRecovery
          <* skipSemicolons
          <* closeAndExpectRBrace
          <* MP.lookAhead eofTok
      )
  MP.updateParserState (\state -> state {MP.stateParseErrors = MP.stateParseErrors finalState})
  let endSpan = maybe noSourceSpan lexTokenSpan (tokStreamPrevToken (MP.stateInput finalState))
      moduleSpan = mergeSourceSpans (inputStartSpan startInput) endSpan
  pure
    Module
      { moduleAnns = [mkAnnotation moduleSpan],
        moduleHead = mHeader,
        moduleLanguagePragmas = concat languagePragmas,
        moduleImports = imports,
        moduleDecls = decls
      }

importDeclsWithRecovery :: TokParser [ImportDecl]
importDeclsWithRecovery = recoverDeclLike importDeclParser

declsWithRecovery :: TokParser [Decl]
declsWithRecovery = recoverDeclLike declParser

recoverDeclLike :: TokParser a -> TokParser [a]
recoverDeclLike parser = go []
  where
    go acc = do
      skipSemicolons
      step <- MP.withRecovery recoverParseError parseStep
      case step of
        RecoverDone -> pure (reverse acc)
        RecoverParsed parsed -> do
          skipSemicolons
          go (parsed : acc)
        RecoverFailed -> do
          skipSemicolons
          go acc

    parseStep = do
      mParsed <- MP.optional parser
      pure $ maybe RecoverDone RecoverParsed mParsed

    recoverParseError err = do
      MP.registerParseError err
      skipUntilDeclBoundary
      pure RecoverFailed

skipUntilDeclBoundary :: TokParser ()
skipUntilDeclBoundary = do
  _ <-
    MP.takeWhileP
      Nothing
      ( \tok ->
          let kind = lexTokenKind tok
           in kind /= TkSpecialSemicolon && kind /= TkSpecialRBrace
      )
  void (MP.optional (expectedTok TkSpecialSemicolon))
