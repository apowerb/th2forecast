#!/usr/bin/env bash
# Checks the tag rules of image-tags.sh. Run locally or in CI:
#   bash .github/scripts/test-image-tags.sh
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
fail=0

expect() { # expect <ref> <prerelease> <expected tags, space separated>
  local got
  got="$(bash "$here/image-tags.sh" "$1" "$2" 2>&1 | tr '\n' ' ' | sed 's/ $//')" || true
  if [ "$got" = "$3" ]; then
    echo "ok   $1 (prerelease=$2) -> $got"
  else
    echo "FAIL $1 (prerelease=$2) -> '$got', expected '$3'"
    fail=1
  fi
}

expect_refused() { # expect_refused <ref> <prerelease>
  if bash "$here/image-tags.sh" "$1" "$2" >/dev/null 2>&1; then
    echo "FAIL $1 (prerelease=$2) was accepted, expected a refusal"
    fail=1
  else
    echo "ok   $1 (prerelease=$2) refused"
  fi
}

# Stable release: full version, minor line and latest.
expect v1.2.3 false "1.2.3 1.2 latest"
expect v0.1.0 false "0.1.0 0.1 latest"
# workflow_dispatch passes the version without the leading v.
expect 1.2.3 false "1.2.3 1.2 latest"
# A semver pre-release suffix never moves the shared tags...
expect v1.2.3-rc.1 false "1.2.3-rc.1"
# ...and neither does a release marked as pre-release on GitHub.
expect v1.2.3 true "1.2.3"
# Anything that is not a version is refused instead of being published.
expect_refused main false
expect_refused v1.2 false
expect_refused 'v1.2.3;rm' false
expect_refused '' false
expect_refused v1.2.3 maybe

exit "$fail"
