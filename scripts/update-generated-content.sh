#!/usr/bin/env bash
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/update-generated-content.sh [--update|--check]

  --update  Rewrite generated files/sections in place
  --check   Exit non-zero if generated files/sections are out of date

Commands are overridable through the environment, which is how the
--check mode can run without repeating an expensive Stackage sweep:

  PARSER_PROGRESS_CMD, LEXER_PROGRESS_CMD, PARSER_EXTENSION_PROGRESS_CMD,
  PARSER_EXTENSION_PROGRESS_TEXT_CMD, STACKAGE_COVERAGE_CMD
USAGE
}

if [ "$#" -ne 1 ]; then
	usage >&2
	exit 2
fi

mode="$1"
case "$mode" in
--update | --check) ;;
*)
	usage >&2
	exit 2
	;;
esac

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

if [ ! -f flake.nix ]; then
	echo "Run this script from inside the repository." >&2
	exit 1
fi

run_cmd() {
	local cmd="$1"
	bash -c "$cmd"
}

parser_cmd="${PARSER_PROGRESS_CMD:-nix run .#parser-progress}"
lexer_cmd="${LEXER_PROGRESS_CMD:-nix run .#lexer-progress}"
extension_markdown_cmd="${PARSER_EXTENSION_PROGRESS_CMD:-nix run .#parser-extension-progress -- --markdown}"
extension_progress_cmd="${PARSER_EXTENSION_PROGRESS_TEXT_CMD:-nix run .#parser-extension-progress}"
stackage_cmd="${STACKAGE_COVERAGE_CMD:-nix run .#stackage-coverage}"

tmpdir="$(mktemp -d)"
cleanup() {
	rm -rf "$tmpdir"
}
trap cleanup EXIT

parser_out="$tmpdir/parser-progress.txt"
lexer_out="$tmpdir/lexer-progress.txt"
extension_out="$tmpdir/parser-extension-progress.md"
extension_progress_out="$tmpdir/parser-extension-progress.txt"
stackage_out="$tmpdir/stackage-coverage.txt"

run_cmd "$parser_cmd" >"$parser_out"
run_cmd "$lexer_cmd" >"$lexer_out"
run_cmd "$extension_markdown_cmd" | sed -n '/^# Haskell Parser Extension Support Status/,$p' >"$extension_out"
run_cmd "$extension_progress_cmd" >"$extension_progress_out"
run_cmd "$stackage_cmd" >"$stackage_out"

parse_progress() {
	local infile="$1"
	awk '
    /^PASS[[:space:]]+/ { pass=$2 }
    /^XFAIL[[:space:]]+/ { xfail=$2 }
    /^XPASS[[:space:]]+/ { xpass=$2 }
    /^FAIL[[:space:]]+/ { fail=$2 }
    /^TOTAL[[:space:]]+/ { total=$2 }
    /^COMPLETE[[:space:]]+/ {
      gsub(/%/, "", $2)
      complete=$2
    }
    END {
      if (total == "" || pass == "" || xfail == "" || xpass == "" || fail == "" || complete == "") {
        exit 2
      }
      implemented = pass + xpass
      printf "%d %d %d %d %d %d %.2f\n", pass, xfail, xpass, fail, total, implemented, complete
    }
  ' "$infile"
}

parse_extension_progress() {
	local infile="$1"
	awk '
    {
      line_has_counts = 0
      line_pass = 0
      line_xfail = 0
      line_xpass = 0
      line_fail = 0

      for (i = 1; i <= NF; i++) {
        if ($i ~ /^PASS=[0-9]+$/) {
          value = $i
          sub(/^PASS=/, "", value)
          line_pass = value + 0
          line_has_counts = 1
        } else if ($i ~ /^XFAIL=[0-9]+$/) {
          value = $i
          sub(/^XFAIL=/, "", value)
          line_xfail = value + 0
          line_has_counts = 1
        } else if ($i ~ /^XPASS=[0-9]+$/) {
          value = $i
          sub(/^XPASS=/, "", value)
          line_xpass = value + 0
          line_has_counts = 1
        } else if ($i ~ /^FAIL=[0-9]+$/) {
          value = $i
          sub(/^FAIL=/, "", value)
          line_fail = value + 0
          line_has_counts = 1
        }
      }

      if (line_has_counts) {
        pass += line_pass
        xfail += line_xfail
        xpass += line_xpass
        fail += line_fail
      }
    }
    END {
      total = pass + xfail + xpass + fail
      if (total <= 0) {
        exit 2
      }
      complete = ((pass + xpass) * 100.0) / total
      printf "%d %d %d %d %d %d %.2f\n", pass, xfail, xpass, fail, total, pass + xpass, complete
    }
  ' "$infile"
}

parse_stackage_coverage() {
	local infile="$1"
	tr '\r' '\n' <"$infile" | awk '
    {
      for (i=1; i<=NF; i++) {
        if ($i == "/" && $(i-2) == "AIHC:") {
          implemented = $(i-1) + 0
          total = $(i+1) + 0
        }
      }
    }
    END {
      if (total == "" || total <= 0) {
        exit 2
      }
      complete = (implemented * 100.0) / total
      printf "%d %d %.2f\n", implemented, total, complete
    }
  '
}

progress_circles() {
	local complete="$1"
	awk -v complete="$complete" '
    BEGIN {
      filled = int(complete / 20)
      if (filled < 0) {
        filled = 0
      } else if (filled > 5) {
        filled = 5
      }

      for (i = 1; i <= filled; i++) {
        printf "●"
      }
      for (i = filled + 1; i <= 5; i++) {
        printf "○"
      }
    }
  '
}

parser_line=""
if ! parser_line="$(parse_progress "$parser_out")"; then
	echo "update-generated-content.sh: could not parse parser-progress summary (expected PASS/XFAIL/XPASS/FAIL/TOTAL/COMPLETE on stdout)." >&2
	exit 2
fi
read -r _ _ _ _ parser_total parser_implemented _ <<<"$parser_line"

lexer_line=""
if ! lexer_line="$(parse_progress "$lexer_out")"; then
	echo "update-generated-content.sh: could not parse lexer-progress summary (expected PASS/XFAIL/XPASS/FAIL/TOTAL/COMPLETE on stdout)." >&2
	exit 2
fi
read -r _ _ _ _ lexer_total lexer_implemented lexer_complete <<<"$lexer_line"

ext_line=""
if ! ext_line="$(parse_extension_progress "$extension_progress_out")"; then
	echo "update-generated-content.sh: could not parse parser-extension-progress text (expected PASS=/XFAIL=/XPASS=/FAIL= fields)." >&2
	exit 2
fi
read -r _ _ _ _ ext_total ext_implemented _ <<<"$ext_line"

stackage_line=""
if ! stackage_line="$(parse_stackage_coverage "$stackage_out")"; then
	echo "update-generated-content.sh: could not parse stackage coverage output (expected 'AIHC: N / M' line on stdout)." >&2
	exit 2
fi
read -r stackage_implemented stackage_total stackage_complete <<<"$stackage_line"

parser_total_tests=$((parser_total + ext_total))
parser_passing_tests=$((parser_implemented + ext_implemented))
parser_total_complete="$(awk -v passing="$parser_passing_tests" -v total="$parser_total_tests" 'BEGIN { if (total <= 0) { printf "0.00" } else { printf "%.2f", (passing * 100.0) / total } }')"

parser_circles="$(progress_circles "$parser_total_complete")"
lexer_circles="$(progress_circles "$lexer_complete")"
stackage_circles="$(progress_circles "$stackage_complete")"

cat >"$tmpdir/readme-parser.txt" <<EOF2
\`${parser_passing_tests}/${parser_total_tests}\` (\`${parser_total_complete}%\`) ${parser_circles}
EOF2

cat >"$tmpdir/readme-lexer.txt" <<EOF2
\`${lexer_implemented}/${lexer_total}\` (\`${lexer_complete}%\`) ${lexer_circles}
EOF2

cat >"$tmpdir/readme-stackage.txt" <<EOF2
\`${stackage_implemented}/${stackage_total}\` (\`${stackage_complete}%\`) ${stackage_circles}
EOF2

replace_marker_inline() {
	local file="$1"
	local marker="$2"
	local content_file="$3"
	local start="<!-- AUTO-GENERATED: START ${marker} -->"
	local end="<!-- AUTO-GENERATED: END ${marker} -->"
	local tmp_out
	tmp_out="$tmpdir/$(basename "$file").${marker}.inline.out"

	local start_count
	local end_count
	start_count="$(grep -Foc "$start" "$file" || true)"
	end_count="$(grep -Foc "$end" "$file" || true)"
	if [ "$start_count" -ne 1 ] || [ "$end_count" -ne 1 ]; then
		echo "Expected exactly one inline marker pair for '${marker}' in ${file}" >&2
		exit 1
	fi

	local content
	content="$(tr -d '\n' <"$content_file")"

	awk -v start="$start" -v end="$end" -v content="$content" '
    {
      s = index($0, start)
      e = index($0, end)
      if (s > 0 && e > s) {
        prefix = substr($0, 1, s + length(start) - 1)
        suffix = substr($0, e)
        print prefix " " content " " suffix
      } else {
        print
      }
    }
  ' "$file" >"$tmp_out"

	if [ "$mode" = "--update" ]; then
		if ! cmp -s "$file" "$tmp_out"; then
			cat "$tmp_out" >"$file"
		fi
	else
		if ! cmp -s "$file" "$tmp_out"; then
			echo "Generated inline block out of date: ${file} (${marker})" >&2
			stale=1
		fi
	fi
}

stale=0

if [ "$mode" = "--update" ]; then
	cp "$extension_out" docs/aihc-parser-supported-extensions.md
else
	if ! cmp -s docs/aihc-parser-supported-extensions.md "$extension_out"; then
		echo "Generated file out of date: docs/aihc-parser-supported-extensions.md" >&2
		stale=1
	fi
fi

replace_marker_inline README.md "parser-progress" "$tmpdir/readme-parser.txt"
replace_marker_inline README.md "lexer-progress" "$tmpdir/readme-lexer.txt"
replace_marker_inline README.md "stackage-progress" "$tmpdir/readme-stackage.txt"

if [ "$mode" = "--check" ] && [ "$stale" -ne 0 ]; then
	exit 1
fi
