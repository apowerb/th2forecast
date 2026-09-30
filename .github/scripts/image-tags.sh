#!/usr/bin/env bash
# Prints the Docker tags to publish for a release, one per line.
#
#   image-tags.sh <release tag or version> <prerelease: true|false>
#
#   v1.2.3        false -> 1.2.3, 1.2, latest
#   v1.2.3-rc.1   false -> 1.2.3-rc.1
#   v1.2.3        true  -> 1.2.3
#
# `X.Y` and `latest` are shared tags: only a stable release may move them, so
# a pre-release (semver suffix or GitHub "pre-release" flag) gets its exact
# version only. Anything that is not a version is refused rather than
# published as an image tag.
set -euo pipefail

ref="${1:-}"
prerelease="${2:-}"

version="${ref#v}"
if [[ ! "$version" =~ ^([0-9]+)\.([0-9]+)\.([0-9]+)(-[0-9A-Za-z.-]+)?$ ]]; then
  echo "refusing to publish an image tagged '$ref': expected vX.Y.Z or vX.Y.Z-suffix" >&2
  exit 1
fi
case "$prerelease" in
  true|false) ;;
  *) echo "prerelease must be true or false, got '$prerelease'" >&2; exit 1 ;;
esac

echo "$version"
if [ -z "${BASH_REMATCH[4]}" ] && [ "$prerelease" = "false" ]; then
  echo "${BASH_REMATCH[1]}.${BASH_REMATCH[2]}"
  echo "latest"
fi
