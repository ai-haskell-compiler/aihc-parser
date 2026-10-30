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
    consumedSpan,
    eofTok,
    expectedTok,
    lazy,
    optionalTok,
    skipSemicolons,
  )
import Aihc.Parser.Internal.Decl (declParser)
import Aihc.Parser.Internal.Import (importDeclParser, languagePragmaParser, moduleHeaderParser)
import Aihc.Parser.Lex (LexTokenKind (..), lexTokenKind)
import Aihc.Parser.Syntax (Decl, ImportDecl, Module (..), mkSpanAnnotation)
import Aihc.Parser.Types (TokStream (..))
import Control.Monad (void, when)
import Data.Maybe (isJust)
import Text.Megaparsec qualified as MP

data RecoverParseStep a
  = RecoverDone
  | RecoverParsed !a
  | RecoverFailed

-- | Parse the header and imports now, leaving declarations, their errors, and
-- the whole-module span suspended until the corresponding result is demanded.
moduleParser :: TokParser Module
moduleParser = do
  startState <- MP.getParserState
  let !startInput = MP.stateInput startState
      !startOffset = MP.stateOffset startState
      !inputStart = MP.pstateSourcePos (MP.statePosState startState)
  languagePragmas <- MP.many (languagePragmaParser <* skipSemicolons)
  mHeader <- MP.optional (moduleHeaderParser <* skipSemicolons)
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
  -- The declarations were parsed in a separate run. With a token trace, take
  -- the stream from the end of that run, so the trace has their tokens. This
  -- forces the declarations, so only a traced parse does it.
  when (isJust (tokStreamTrace startInput)) $
    MP.updateParserState (\state -> state {MP.stateInput = MP.stateInput finalState})
  let moduleSpan = consumedSpan inputStart startInput startOffset (MP.stateInput finalState) (MP.stateOffset finalState)
  pure
    Module
      { moduleAnns = [mkSpanAnnotation moduleSpan],
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
  void (optionalTok TkSpecialSemicolon)
