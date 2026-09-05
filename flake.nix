{
  description = "aihc-parser development flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs = {nixpkgs, ...}: let
    systems = [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];
    forAllSystems = nixpkgs.lib.genAttrs systems;
    projectSource = pkgs:
      pkgs.lib.fileset.toSource {
        root = ./.;
        fileset = pkgs.lib.fileset.unions [
          ./LICENSE
          ./CHANGELOG.md
          ./aihc-parser.cabal
          ./src
          ./test
          ./common
          ./docs
          ./app
          ./aihc-parser-compat
          ./tooling
          ./scripts
        ];
      };
    mkHsPkgs = pkgs: let
      hsLib = pkgs.haskell.lib;
      src = projectSource pkgs;
      localPackageNames = [
        "aihc-hackage"
        "aihc-parser"
        "aihc-parser-bench"
        "aihc-parser-compat"
        "aihc-parser-tooling-common"
      ];
      isOverridableHaskellDrv = drv:
        pkgs.lib.isDerivation drv && drv.isHaskellLibrary or false;
      withoutProfiling = drv:
        hsLib.disableExecutableProfiling (hsLib.disableLibraryProfiling drv);
      parserTestFlags = [
        "--hide-successes"
        "--quickcheck-tests"
        "1000"
        "--quickcheck-timeout"
        "20s"
        "--quickcheck-shrinks"
        "10000"
      ];
      # Local packages are always built with tests and -Werror so that the
      # `checks` outputs reuse the exact same derivations as `packages`,
      # instead of forcing a second full compile of every local package.
      withStrictChecks = testFlags: drv:
        hsLib.overrideCabal drv (old: {
          doCheck = true;
          configureFlags = (old.configureFlags or []) ++ ["--ghc-options=-Werror"];
          inherit testFlags;
        });
      disableUpstreamChecks = builtins.mapAttrs (
        name: drv:
          if builtins.elem name localPackageNames || !(isOverridableHaskellDrv drv)
          then drv
          else
            hsLib.dontCheck
            (hsLib.dontHaddock
              (withoutProfiling drv))
      );
    in
      pkgs.haskell.packages.ghc9124.override {
        overrides = final: prev: let
          mkSubpackage = name: subpath: options:
            hsLib.overrideCabal
            (final.callCabal2nixWithOptions name (src + "/${subpath}") options {})
            (_old: {
              inherit src;
              postUnpack = ''
                sourceRoot+="/${subpath}"
                echo "source root reset to $sourceRoot"
              '';
            });
        in
          disableUpstreamChecks prev
          // {
            ghc-lib-parser = hsLib.dontCheck (hsLib.dontHaddock (
              withoutProfiling final.ghc-lib-parser_9_14_1_20251220
            ));
            aihc-cpp = hsLib.dontCheck (hsLib.dontHaddock (
              withoutProfiling (final.callHackageDirect {
                pkg = "aihc-cpp";
                ver = "1.0.0.2";
                sha256 = "1bsq5549wq9nz62qrij6iabac4xv57dbwcqnflgvbfimj910jcz6";
              } {})
            ));
            aihc-hackage =
              withStrictChecks [] (withoutProfiling (final.callCabal2nix
                  "aihc-hackage" (src + "/tooling/aihc-hackage") {}));
            aihc-parser = withStrictChecks parserTestFlags (withoutProfiling (final.callCabal2nix
              "aihc-parser"
              src {}));
            aihc-parser-bench =
              hsLib.doCheck (withoutProfiling (mkSubpackage
                  "aihc-parser-bench" "tooling/aihc-parser-bench" ""));
            aihc-parser-compat =
              withoutProfiling (mkSubpackage
                "aihc-parser-compat" "aihc-parser-compat" "");
            aihc-parser-tooling-common =
              withoutProfiling (mkSubpackage
                "aihc-parser-tooling-common" "tooling/aihc-parser-tooling-common" "");
          };
      };
  in {
    packages = forAllSystems (system: let
      pkgs = import nixpkgs {inherit system;};
      hsPkgs = mkHsPkgs pkgs;
    in {
      default = hsPkgs.aihc-parser;
      aihc-parser = hsPkgs.aihc-parser;
      aihc-parser-bench = hsPkgs.aihc-parser-bench;
      parser-progress = hsPkgs.aihc-parser-tooling-common;
    });

    apps = forAllSystems (system: let
      pkgs = import nixpkgs {inherit system;};
      hsPkgs = mkHsPkgs pkgs;
      benchExe = pkgs.lib.getExe' hsPkgs.aihc-parser-bench "aihc-parser-bench";
      toolingExe = name: pkgs.lib.getExe' hsPkgs.aihc-parser-tooling-common name;
      mkProgressApp = name: description: {
        type = "app";
        program = toolingExe name;
        meta.description = description;
      };
    in {
      parser-progress = mkProgressApp "parser-progress" "Report parser test progress";
      lexer-progress = mkProgressApp "lexer-progress" "Report lexer test progress";
      parser-extension-progress = mkProgressApp "parser-extension-progress" "Report per-extension parser test progress";

      stackage-coverage = {
        type = "app";
        program = "${pkgs.writeShellApplication {
          name = "stackage-coverage";
          text = ''
            set -euo pipefail
            exec ${benchExe} coverage "$@"
          '';
        }}/bin/stackage-coverage";
        meta.description = "Report how many Stackage packages aihc-parser parses";
      };

      generate-reports = {
        type = "app";
        program = "${pkgs.writeShellApplication {
          name = "generate-reports";
          runtimeInputs = [pkgs.bash pkgs.gawk pkgs.gnugrep pkgs.coreutils];
          text = ''
            set -euo pipefail
            test -f aihc-parser.cabal || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            PARSER_PROGRESS_CMD=${toolingExe "parser-progress"} \
            LEXER_PROGRESS_CMD=${toolingExe "lexer-progress"} \
            PARSER_EXTENSION_PROGRESS_CMD="${toolingExe "parser-extension-progress"} --markdown" \
            PARSER_EXTENSION_PROGRESS_TEXT_CMD=${toolingExe "parser-extension-progress"} \
            STACKAGE_COVERAGE_CMD="${benchExe} coverage" \
              exec ./scripts/update-generated-content.sh "''${1:---update}"
          '';
        }}/bin/generate-reports";
        meta.description = "Regenerate the README status table and the extension support docs";
      };

      generate-benchmarks = {
        type = "app";
        program = "${pkgs.writeShellApplication {
          name = "generate-benchmarks";
          runtimeInputs = [
            pkgs.bash
            pkgs.git
            pkgs.llvmPackages.clang
            hsPkgs.cpphs
          ];
          text = ''
            set -euo pipefail
            test -f aihc-parser.cabal || {
              echo "Run this app from the repository root." >&2
              exit 1
            }
            exec ${benchExe} report "$@"
          '';
        }}/bin/generate-benchmarks";
        meta.description = "Regenerate BENCHMARKS.md from a Stackage snapshot";
      };
    });

    checks = forAllSystems (system: let
      pkgs = import nixpkgs {inherit system;};
      hsPkgs = mkHsPkgs pkgs;
      src = projectSource pkgs;
      ghcEnv = hsPkgs.ghcWithPackages (p: [p.aihc-parser p.doctest]);
      parserProgress = pkgs.lib.getExe' hsPkgs.aihc-parser-tooling-common "parser-progress";
      lexerProgress = pkgs.lib.getExe' hsPkgs.aihc-parser-tooling-common "lexer-progress";
      extensionProgress = pkgs.lib.getExe' hsPkgs.aihc-parser-tooling-common "parser-extension-progress";
      sourceCheck = name: inputs: command:
        pkgs.runCommand name {
          nativeBuildInputs = inputs;
          inherit src;
        } ''
          cp -r "$src" source
          chmod -R u+w source
          cd source
          ${command}
          touch "$out"
        '';
    in {
      parser-tests = hsPkgs.aihc-parser;
      parser-compat-tests = hsPkgs.aihc-parser-compat;
      hackage-tests = hsPkgs.aihc-hackage;
      parser-bench-tests = hsPkgs.aihc-parser-bench;
      doctest = sourceCheck "aihc-parser-doctest" [ghcEnv] ''
        packageDb=$(ghc --print-global-package-db)
        doctest -XGHC2021 -package-db="$packageDb" -isrc \
          src/Aihc/Parser/Parens.hs \
          src/Aihc/Parser/Pretty.hs \
          src/Aihc/Parser/Shorthand.hs \
          src/Aihc/Parser.hs
      '';
      parser-progress-strict = sourceCheck "aihc-parser-progress-strict" [] ''
        ${parserProgress} --strict
      '';
      lexer-progress-strict = sourceCheck "aihc-lexer-progress-strict" [] ''
        ${lexerProgress} --strict
      '';
      extension-progress-strict = sourceCheck "aihc-parser-extension-progress-strict" [] ''
        ${extensionProgress} --strict
      '';
      haskell-format = sourceCheck "aihc-parser-haskell-format" [pkgs.ormolu pkgs.findutils] ''
        find src test common app tooling -name '*.hs' -not -path '*/Test/Fixtures/*' -print0 | xargs -0 -r ormolu --mode check
      '';
      haskell-lint = sourceCheck "aihc-parser-haskell-lint" [pkgs.hlint pkgs.findutils] ''
        find src test common app tooling -name '*.hs' -not -path '*/Test/Fixtures/*' -print0 | xargs -0 -r hlint
      '';
      cabal-format = sourceCheck "aihc-parser-cabal-format" [pkgs.haskellPackages.cabal-gild pkgs.findutils] ''
        failed=0
        while IFS= read -r -d "" file; do
          cabal-gild --mode check --input "$file" || failed=1
        done < <(find . -name '*.cabal' -print0)
        test "$failed" -eq 0
      '';
    });

    devShells = forAllSystems (system: let
      pkgs = import nixpkgs {inherit system;};
      hsPkgs = mkHsPkgs pkgs;
    in {
      default = pkgs.mkShell {
        packages = [
          hsPkgs.ghc
          pkgs.cabal-install
          pkgs.just
          pkgs.ormolu
          pkgs.hlint
          pkgs.haskellPackages.cabal-gild
        ];
      };
    });

    formatter = forAllSystems (system: (import nixpkgs {inherit system;}).alejandra);
  };
}
