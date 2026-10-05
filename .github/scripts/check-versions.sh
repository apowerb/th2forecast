#!/usr/bin/env bash
# Check that both engines report the same version, and that it matches the
# release being published.
#
# Each image serves its version on /health: the R engine reads the package
# version from DESCRIPTION, the Python engine reads th2fc.__version__.
# Deployments check /health against the release they deploy and roll back on
# a mismatch, so a release whose DESCRIPTION was not bumped cannot be
# deployed as the R engine (0.1.2 shipped an R image reporting 0.0.48).
#
# Usage: check-versions.sh [TAG]
#   Without TAG (pull request, push to main): the two files must agree.
#   With TAG (vX.Y.Z or vX.Y.Z-rc.N): they must also equal X.Y.Z. R package
#   versions cannot carry a pre-release suffix, so the suffix is ignored.
set -euo pipefail

root="${ROOT:-$(cd "$(dirname "$0")/../.." && pwd)}"

r_version="$(sed -n 's/^Version:[[:space:]]*//p' "$root/DESCRIPTION")"
py_version="$(sed -n 's/^__version__[[:space:]]*=[[:space:]]*"\([^"]*\)".*/\1/p' "$root/python/th2fc/__init__.py")"

if [ -z "$r_version" ] || [ -z "$py_version" ]; then
  echo "::error::Could not read a version (DESCRIPTION: '$r_version', python/th2fc/__init__.py: '$py_version')." >&2
  exit 1
fi

if [ "$r_version" != "$py_version" ]; then
  echo "::error::DESCRIPTION says $r_version but python/th2fc/__init__.py says $py_version. Bump both together." >&2
  exit 1
fi

if [ $# -ge 1 ] && [ -n "$1" ]; then
  tag_version="${1#v}"
  tag_version="${tag_version%%-*}"
  if [ "$tag_version" != "$r_version" ]; then
    echo "::error::Release $1 does not match the version in the sources ($r_version). Bump DESCRIPTION and python/th2fc/__init__.py before releasing." >&2
    exit 1
  fi
fi

echo "Versions agree: $r_version"
