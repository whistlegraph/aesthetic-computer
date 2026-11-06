#!/bin/bash
# Read-only regression probe for Terminal's stale window references.
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
if [[ "$(osascript -e 'application "Terminal" is running')" != true ]]; then
  echo 'SKIP: Terminal is not running'
  exit 0
fi
probe="$(mktemp /tmp/slab-decor-windows.XXXXXX)"
trap 'rm -f "$probe"' EXIT
python3 - "$root" "$probe" <<'PY'
import json, pathlib, re, sys
source = pathlib.Path(sys.argv[1], 'Sources/SlabMenubar/AppDelegate.swift').read_text()
chunk = source[source.index('        var tm: [String] = ['):
               source.index('            TerminalReadability.bootstrapScript(profileURL: readableURL),')]
lines = [json.loads(match.group(1)) for line in chunk.splitlines()
         if (match := re.match(r'\s*(".*"),?$', line))]
snapshot = '\n'.join(lines) + '\n  end tell\nend if\n'
# missing value fails on id/bounds just like Terminal's dangling references.
probe = (snapshot + 'set expectedIds to _slabDecorIds\nset expectedBounds to _slabDecorBounds\n'
         + snapshot.replace('in windows', 'in ({missing value} & windows)')
         + 'if expectedIds is not _slabDecorIds then error "Stale window changed target IDs"\n'
         + 'if expectedBounds is not _slabDecorBounds then error "Stale window changed target bounds"\n'
         + 'return "PASS: invalid window skipped; IDs and bounds preserved for " & (count _slabDecorIds) & " windows"\n')
pathlib.Path(sys.argv[2]).write_text(probe)
PY
osascript "$probe"
