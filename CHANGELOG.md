# Changelog

All notable changes to `aihc-parser` will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Fixed

- Restored 100% Stackage coverage (3327/3327 packages, up from 3295/3327).
  The coverage tooling, not the parser, had regressed: it handed literate
  Haskell to the parser without unliterating it, which rejected the 22
  snapshot packages that ship `.lhs` sources, and it resolved `#include`
  directives only next to the including file, so the CPP macros that
  `conduit`, `hashmap`, `thyme` and `ghc-internal` keep in a header behind
  `include-dirs` were never expanded. Unliterating and the package-wide
  header search now live in `aihc-hackage`, shared by the test suite and the
  benchmark tooling instead of being implemented twice.

### Changed

- Made module parsing about 1.5x faster on the Stackage benchmark corpus and
  reduced allocation by a third. The context-item kind-signature lookahead now
  stops at declaration boundaries instead of scanning to the end of the
  module, the token stream memoizes each step so lookahead and backtracking
  no longer rerun the layout algorithm, and the lexer dispatches on the first
  character of each token.
- Stackage coverage no longer counts files that `ghc-lib-parser` rejects as
  well. GHC is the reference, so a file GHC cannot parse -- such as
  `ghc`'s `GHC/Builtin/PrimOps.hs`, which needs headers generated during a
  GHC build -- says nothing about `aihc-parser`. The `coverage` command
  reports how many files were set aside this way, and takes a repeatable
  `--package` flag for checking a single package.

## [2.0.0.0] - 2026-09-03

### Changed

- Added dedicated `EImplicitParam` and `DeclImplicitParam` AST constructors
  for `?x` expressions and `?x = e` bindings under `ImplicitParams`. These
  forms previously reused `EVar` and `PatternBind`/`PVar`, which made an
  implicit parameter indistinguishable from an ordinary variable downstream.
  `TImplicitParam` (the type-level form) is unchanged.

## [1.0.0.6] - 2026-09-02

### Changed

- Deferred module declaration parsing until declarations, parse errors, or the
  module span are demanded.

## [1.0.0.5] - 2026-07-27

### Fixed

- Removed the developer-only `fuzz` sublibrary and flag. Property generators
  and fuzz tests now exist only as internals of the test suite, so Hackage
  exposes only the parser library and its runtime dependencies.

## [1.0.0.4] - 2026-07-26

### Added

- Added `Addr#` literal syntax and expanded GHC layout oracle coverage.
- Added a developer-only public fuzz registry for downstream property suites.
- Added a `Read` instance for `FixityAssoc` so downstream metadata containing
  operator fixities can be persisted and restored.

### Changed

- Improved parser throughput and reduced allocations across large Stackage
  inputs and deeply nested syntax.
- Moved development to the standalone
  [`ai-haskell-compiler/aihc-parser`](https://github.com/ai-haskell-compiler/aihc-parser)
  repository, including the full test, progress, doctest, fuzz, and
  compatibility CI configuration.

### Fixed

- Aligned multiline `case` scrutinee layout with GHC.

## [1.0.0.3] - 2026-06-01

### Changed

- Simplified the Cabal package synopsis for cleaner Hackage metadata.
- Added `tested-with` metadata for supported GHC versions and tightened
  package bounds to the validated dependency range.

### Fixed

- Preserved source-span annotations on binder names so downstream consumers can
  locate binders parsed from declarations such as foreign imports.

## [1.0.0.2] - 2026-05-28

### Fixed

- Removed the internal `parser-tooling-common` and `parser-test-support`
  sublibraries from the published Cabal package so Hackage exposes only the
  core `aihc-parser` library.

## [1.0.0.1] - 2026-05-28

### Fixed

- Removed internal progress executables from the published Cabal package so
  Hackage lists `aihc-parser` as library-only.
- Included parser fixture files in the source distribution so Hackage can run
  the package test suite and report coverage.

## [1.0.0.0] - 2026-05-27

### Added

- Initial stable release of the from-scratch Haskell parser package.
- Public parser, lexer, syntax, pretty-printer, shorthand, token, and
  parenthesis-insertion modules.
- Oracle-backed parser validation against GHC with parse/pretty round-trip
  fingerprint checks.
- Haskell2010 and extension coverage tracking, including the current fully
  implemented Haskell2010 baseline and supported tracked extensions.
