#!/usr/bin/env bash
# Offline audio regression; optional argument saves before/after whistle WAVs.
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
scratch="$(mktemp -d)"
trap 'rm -rf "$scratch"' EXIT
nice -n 8 xcrun swiftc -O -suppress-warnings -parse-as-library \
  "$root/Sources/MenuBand/MelodicHeadroom.swift" \
  "$root/bin/check-melodic-headroom.swift" -o "$scratch/checks"
"$scratch/checks" "$@"
