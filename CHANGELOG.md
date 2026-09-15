# Changelog

All notable changes to `aihc-parser` will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Changed

- `sourceSpanSourceName` is now a `Text` rather than a `FilePath`. Callers
  that read the field get a `Text`; callers that build a `SourceSpan` by hand
  pass a `Text`. The name given as `parserSourceName` is still a `FilePath`
  and is converted once when lexing starts.
- `SourceSpan` is now a bidirectional record pattern synonym over a packed
  representation. Constructing a span, matching on one and reading any of its
  seven fields all work exactly as before; what no longer compiles is record
  *update* syntax (`span {sourceSpanStartLine = n}`), which pattern synonyms
  do not support, and code that relies on the `Data` instance seeing seven
  `Int` fields. Import it as `SourceSpan (NoSourceSpan), pattern SourceSpan`
  plus the field names, since `SourceSpan (..)` no longer brings the fields
  into scope.
- `applyImpliedExtensions` returns its result in `Extension` constructor order
  and without duplicates when it has anything to add, rather than in
  most-recently-enabled-first order. Only membership was ever meaningful; a
  list that is already closed is still returned untouched.

### Performance

- `SourceSpan` is a flat, packed record: the source name is a `Text` shared by
  every span from the same file, and the six positions live in three unboxed
  `Word64` fields instead of six `Int` ones. Forcing a span with `rnf` is now
  constant time instead of walking the source name character by character,
  which a consumer that `deepseq`s a parse tree paid once per span, and the
  most numerous object in a parse tree is a third smaller.
- Expression parsing dispatches the block forms (`do`, `mdo`, qualified `do`,
  `if`, `case`, `let`, `proc`, `\`) and prefix negation on the next token
  instead of trying them in turn. Each block form starts with its own
  keyword, so at most one could ever match, but the old chain of alternatives
  allocated a continuation for all nine plus a backtracking negation at every
  expression position.
- The type-atom parser's fallback branch tries only the three alternatives
  whose leading token is not already dispatched, instead of re-running the
  full eleven-way chain.
- The implied-extension fixpoint runs on the `ExtensionSet` bitset instead of
  on lists, so closing a set costs word operations rather than a `filter` per
  implication and a pair of `sort`s per round. It ran once per file.

Together these cut the `bench-aihc-base` benchmark's allocation by about 21%
(2.90 GB to 2.30 GB over five iterations), its wall time by about 12%
(162 ms to 142 ms per iteration) and its peak heap by about 10%.

## [3.0.1.1] - 2026-09-15

### Fixed

- Read a quantified constraint that is one item of a comma-separated context.
  The context-item parser had no rule for `forall a. C a => D (f a)` or for
  `p => q`, so a context such as
  `class (Eq1 t, forall a. Eq a => Eq (t a)) => Eq1Wrapper t` did not divide
  into items. The parentheses then fell back to the general type parser, which
  read the full list as one tuple type, and `classDeclContext` held a single
  `TTuple` instead of two constraints. A quantified constraint that is the only
  item of a context was not affected.

## [3.0.1.0] - 2026-09-09

### Performance

- Cut parser wall time by about 10%, allocations by about 4%, and peak heap by
  about 27% on the Stackage corpus benchmark. The lexer decides ASCII
  characters without consulting the Unicode general-category tables, groups
  the keyword table by length, and derives byte offsets from the length of the
  consumed text; the token stream builds its successor strictly instead of
  through a thunk; identifier atoms are built inside a single token match; and
  implied `LANGUAGE` extensions are resolved through a map rather than a
  linear scan.

### Fixed

- Accept `(@)` as a parenthesized operator variable in expressions. A tight
  `@` lexes as a reserved token, and the parenthesized-operator parser rejected
  it, so `(@)`, `(@) 1 2`, and `$(@)` failed to parse even though GHC accepts
  them (rejecting `(@)` only later, in the renamer). The pretty-printer already
  rendered such names as `(@)`, so they did not round-trip. Other reserved
  operators (`->`, `=>`, `::`, `|`, `<-`, `=`, `..`) are still rejected.

- Wrap `DeclPatSynSig`, `DeclDefault`, and `DeclSplice` in `DeclAnn` with a
  source span, like every other top-level declaration. Consumers that locate
  declarations by span (such as attaching `-- |` comments) can now handle
  pattern synonym signatures, `default` declarations, and declaration splices.

- Reuse parsed expressions in nested list, record, and view patterns to avoid
  quadratic backtracking.
- Limit retries for local function bindings to the binding head. Invalid
  nested `let` expressions no longer cause exponential backtracking.
- Parse parenthesized arrow commands before trying expression or pattern
  bindings. Deeply nested commands no longer cause quadratic backtracking.

- Removed exponential backtracking for nested parenthesized block expressions
  in `do` statements, guards, and list comprehensions. Parse expressions first
  and use the pattern parser when pattern-only syntax requires it. This also
  speeds up nested list expressions in these positions.

- Apply `LANGUAGE` settings left to right, the order GHC applies them in, so
  that a later setting overrides an earlier one. A later explicit disable of
  an extension could previously be resurrected by an implication from an
  earlier enable.

## [3.0.0.0] - 2026-09-06

### Changed

- **Breaking:** Added the `BuiltinCon` type for the constructors that the
  grammar builds in: `(,)`, `(# , #)`, `(->)`, `[]`, and `(:)`. These
  constructors have no name that a scope can bind, so the AST no longer
  spells them as a `Name`. The type namespace uses `TBuiltinCon BuiltinCon
  TypePromotion` and the pattern namespace uses `PBuiltinCon BuiltinCon
  [Type] [Pattern]`. This replaces `TypeBuiltinCon`, `TBuiltinCon
  TypeBuiltinCon`, and `PTupleCon`.
  - A prefix tuple constructor in a pattern, such as `(,) a b`, parsed as a
    `PCon` whose name was only commas, such as `PCon ","
    [PVar "a", PVar "b"]`. The dedicated pattern parser now also accepts the
    prefix tuple constructor outside parentheses, for example in a `case`
    alternative.
  - An unboxed prefix tuple constructor in a type, such as `(# , #) Int
    Bool`, parsed as `TCon "(#,#)"`. The boxed form already had a dedicated
    constructor.
  - A promoted built-in constructor, such as `'[]`, `'(:)`, or `'(,)`,
    parsed as a `TCon` whose name was the surface syntax, such as `TCon "[]"
    Promoted`. `TBuiltinCon` now carries the promotion flag.
- **Breaking:** Added the `EViewPat` expression constructor for the
  view-pattern arrow. The parser made an `EInfix` with an operator named
  `->`, and `checkPattern` found the view pattern by a comparison against
  that name. No scope binds a term named `->`.
- The parser now rejects `'(->)`, which GHC also rejects. The arrow has no
  promoted form.
- The parser now rejects a reserved operator in a parenthesized expression:
  `(->)`, `(=>)`, `(::)`, `(=)`, `(|)`, `(<-)`, `(..)`, and `(@)`. These
  parsed as an `EVar` with the reserved operator as its name. GHC rejects
  each of them. `(-)` and `(:)` are unchanged.
- Made module parsing about 1.5x faster on the Stackage benchmark corpus and
  reduced allocation by a third. The context-item kind-signature lookahead now
  stops at declaration boundaries instead of scanning to the end of the
  module, the token stream memoizes each step so lookahead and backtracking
  no longer rerun the layout algorithm, and the lexer dispatches on the first
  character of each token.

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
