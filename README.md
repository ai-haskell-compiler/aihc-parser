# aihc-parser

[![CI](https://github.com/ai-haskell-compiler/aihc-parser/actions/workflows/nix-flake-check.yml/badge.svg)](https://github.com/ai-haskell-compiler/aihc-parser/actions/workflows/nix-flake-check.yml)
[![GHC compatibility](https://github.com/ai-haskell-compiler/aihc-parser/actions/workflows/minimum-ghc.yml/badge.svg)](https://github.com/ai-haskell-compiler/aihc-parser/actions/workflows/minimum-ghc.yml)
[![Hackage](https://img.shields.io/hackage/v/aihc-parser.svg)](https://hackage.haskell.org/package/aihc-parser)

`aihc-parser` is a Haskell parser library. It parses modules, declarations,
expressions, patterns, and types into an AST, and can pretty-print that AST
back to source code.

Its goal is to accept exactly the same Haskell as GHC. There are currently no
known compatibility bugs; if you find one, please report it.

## Status

| Metric | Progress |
| --- | ---: |
| Parser tests | <!-- AUTO-GENERATED: START parser-progress --> `2387/2387` (`100.00%`) ●●●●● <!-- AUTO-GENERATED: END parser-progress --> |
| Lexer tests | <!-- AUTO-GENERATED: START lexer-progress --> `107/107` (`100.00%`) ●●●●● <!-- AUTO-GENERATED: END lexer-progress --> |
| Stackage packages parsed | <!-- AUTO-GENERATED: START stackage-progress --> `3327/3327` (`100.00%`) ●●●●● <!-- AUTO-GENERATED: END stackage-progress --> |

A weekly workflow updates these numbers. Stackage coverage shows how many
packages of a Stackage snapshot `aihc-parser` accepts. A package is a pass if
the parser accepts each source file that the `.cabal` file declares. Packages
that do not download, or that declare no Haskell sources, are not counted, and
neither are individual files that `ghc-lib-parser` rejects as well: GHC is the
reference, so a file GHC cannot parse says nothing about `aihc-parser`.

## A Quick Taste

```console
% echo 'main = putStrLn "hello world"' | aihc-parser-dev
Module {[DeclValue (PatternBind (PVar "main") (EApp (EVar "putStrLn") (EString "hello world")))]}
```

It understands modern GHC syntax too:

```console
% echo 'x = (.f.g)' | aihc-parser-dev -XOverloadedRecordDot
Module {[DeclValue (PatternBind (PVar "x") (EParen (EGetFieldProjection ["f", "g"])))]}
```

`aihc-parser-dev` reads from standard input and defaults to Haskell2010. Use
`--pretty` to emit Haskell source, `--language-edition GHC2024` to select an
edition, and `-XExtension`/`-XNoExtension` to enable or disable extensions.

Use it for source inspection, syntax-aware rewriting, Haskell syntax
experiments, or compiler-adjacent tools that want a regular library API instead
of reaching into GHC internals.

## What It Supports

`aihc-parser` tracks GHC's parser behavior. The test suite checks both whether
syntax is accepted and whether `aihc-parser` builds the same AST that GHC does.

For the current support overview, see
[aihc-parser-supported-extensions.md](docs/aihc-parser-supported-extensions.md).

## Caveats

`aihc-parser` is ready to try, but it has not had anything like GHC's years of
production exposure.

The intended behavior is exact GHC compatibility. Rejecting code that GHC
accepts is a bug. Accepting code that GHC rejects is also a bug, but that class
of bug is harder to test exhaustively, so some cases almost certainly remain.

## What Gets Tested

The test suite uses golden examples, GHC oracle checks, package fixtures, and
generated syntax. The core properties are:

- `parse ∘ pretty = id`: pretty-printed ASTs parse back to the same
  `aihc-parser` AST.
- `to_ghc_ast ∘ parse = ghc_parse`: parsed ASTs convert to exactly the same
  GHC AST that `ghc-lib-parser` produces.
- `parse`, `pretty`, `parens`, and shorthand rendering do not throw on generated
  inputs.

## Development

Run the complete local CI suite with `just check`, or the hermetic suite with
`nix flake check`. Run `just qc` for continuous deep QuickCheck fuzzing.

Run `just reports` (or `nix run .#generate-reports`) to update the status table
and [aihc-parser-supported-extensions.md](docs/aihc-parser-supported-extensions.md).
Run `just stackage-coverage` to measure the Stackage coverage only. Both
commands download the snapshot packages, so the first run takes a long time.

## Performance

[BENCHMARKS.md](BENCHMARKS.md) shows the parser time and the memory use
relative to `ghc-lib-parser`, measured on a Stackage snapshot. Every column is
a fraction of the baseline's, so lower is better.

Run `just benchmarks` (or `nix run .#generate-benchmarks`) to measure again and
write a new `BENCHMARKS.md`. The tool downloads the snapshot packages, so the
first run takes a long time.

For optimisation work, `just bench-aihc-base` (or `nix run .#bench-aihc-base`)
is the short inner loop. It parses the 263 Haskell files of `core-libs/aihc-base`
from the `aihc` repository and forces every resulting `Module` with `deepseq`,
which is what a downstream compiler pass actually does with a parse tree. One
iteration takes roughly 130 ms, and the corpus is pinned by commit in
`flake.lock`, so numbers stay comparable across machines and across days.

The benchmark executable links against the threaded RTS, matching how AIHC
itself is built, but defaults to `-N1`: this workload is sequential, and the
parallel GC a larger `-N` turns on costs wall time and adds variance. `-rtsopts`
is on, so `+RTS -N4 -RTS` is still available for a deliberate experiment.

```bash
nix run .#bench-aihc-base -- --warmup 1 --iterations 5 --gc-stats +RTS -T -RTS
```

`Allocated` and `Max live` from `--gc-stats` are byte-identical between runs of
the same binary, so they are the better primary signal; treat wall time as
confirmation. Bumping the pinned commit in `flake.nix` changes the corpus and
makes new numbers incomparable with old ones.
