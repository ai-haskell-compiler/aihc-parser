# Test runner for aihc-parser

test:
  cabal test -v0 all --test-options='--hide-successes --quickcheck-tests 1000 --quickcheck-timeout 20s --quickcheck-shrinks 10000'

replay ARGUMENT:
  cabal test aihc-parser:spec --jobs=1 -v0 --test-options='--pattern properties --quickcheck-replay="{{ARGUMENT}}" --hide-successes'

qc:
  while true; do just qc1 || break; done

qc1:
  cabal test aihc-parser:spec -v0 --jobs=1 --test-options='--pattern properties --quickcheck-tests 10000 --quickcheck-shrinks 1000000 --hide-successes'

progress:
  cabal run -v0 parser-progress

lexer-progress:
  cabal run -v0 lexer-progress

extension-progress:
  cabal run -v0 parser-extension-progress

progress-strict:
  cabal run -v0 parser-progress -- --strict
  cabal run -v0 lexer-progress -- --strict
  cabal run -v0 parser-extension-progress -- --strict

fmt:
  nix develop --quiet --command bash -c 'while IFS= read -r -d "" file; do cabal-gild --mode format --io "$file"; done < <(find . -name "*.cabal" -not -path "*/dist-newstyle/*" -print0); ormolu --mode inplace $(find src test common app tooling -name "*.hs" -not -path "*/Test/Fixtures/*")'

check:
  nix develop --quiet --command bash -c 'failed=0; while IFS= read -r -d "" file; do cabal-gild --mode check --input "$file" || failed=1; done < <(find . -name "*.cabal" -not -path "*/dist-newstyle/*" -print0); exit "$failed"'
  nix develop --quiet --command bash -c 'ormolu --mode check $(find src test common app tooling -name "*.hs" -not -path "*/Test/Fixtures/*")'
  nix develop --quiet --command bash -c 'hlint -j $(find src test common app tooling -name "*.hs" -not -path "*/Test/Fixtures/*")'
  cabal test -v0 all --ghc-options=-Werror --test-options='--hide-successes --quickcheck-tests 1000 --quickcheck-timeout 20s --quickcheck-shrinks 10000'
  just progress-strict
