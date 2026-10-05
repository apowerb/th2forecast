#!/usr/bin/env bash
# Tests for check-versions.sh. Run: bash .github/scripts/test-check-versions.sh
set -uo pipefail

script="$(cd "$(dirname "$0")" && pwd)/check-versions.sh"
failures=0

# make_root R_VERSION PY_VERSION -> prints a temporary source root
make_root() {
  local dir
  dir="$(mktemp -d)"
  mkdir -p "$dir/python/th2fc"
  printf 'Package: th2forecast\nVersion: %s\nTitle: x\n' "$1" > "$dir/DESCRIPTION"
  printf '"""th2fc."""\n\n__version__ = "%s"\n' "$2" > "$dir/python/th2fc/__init__.py"
  printf '%s' "$dir"
}

# expect PASS|FAIL DESCRIPTION R_VERSION PY_VERSION [TAG]
expect() {
  local want="$1" label="$2" root got
  root="$(make_root "$3" "$4")"
  shift 4
  if ROOT="$root" bash "$script" "$@" >/dev/null 2>&1; then got=PASS; else got=FAIL; fi
  rm -rf "$root"
  if [ "$got" = "$want" ]; then
    echo "ok   - $label"
  else
    echo "FAIL - $label (expected $want, got $got)"
    failures=$((failures + 1))
  fi
}

expect PASS "same versions, no tag" 0.1.3 0.1.3
expect FAIL "R behind Python (the 0.1.2 case)" 0.0.48 0.1.2
expect PASS "release tag matches" 0.1.3 0.1.3 v0.1.3
expect FAIL "release tag ahead of the sources" 0.1.2 0.1.2 v0.1.3
expect PASS "pre-release tag matches its base version" 0.2.0 0.2.0 v0.2.0-rc.1
expect FAIL "pre-release tag on another version" 0.1.3 0.1.3 v0.2.0-rc.1
expect PASS "empty tag means no release" 0.1.3 0.1.3 ""

# The repository itself must pass.
if bash "$script" >/dev/null 2>&1; then
  echo "ok   - repository versions agree"
else
  echo "FAIL - repository versions disagree"
  failures=$((failures + 1))
fi

[ "$failures" -eq 0 ] || { echo "$failures failure(s)"; exit 1; }
echo "all passed"
