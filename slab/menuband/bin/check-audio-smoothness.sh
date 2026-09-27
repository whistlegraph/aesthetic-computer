#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
scratch="$(mktemp -d)"
trap 'rm -rf "$scratch"' EXIT
src="$root/Sources/MenuBand"
nice -n 8 xcrun swiftc -O -parse-as-library \
  "$src/MonitorReadCursor.swift" "$src/InterfaceResetCoordinator.swift" \
  "$src/MenuBandTape.swift" "$src/TapeCoverArt.swift" \
  "$src/TakeMetadata.swift" "$src/MidiFile.swift" "$src/TapeFilePromise.swift" \
  "$root/bin/check-audio-smoothness.swift" -o "$scratch/checks"
"$scratch/checks"
