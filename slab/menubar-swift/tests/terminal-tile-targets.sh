#!/bin/bash
# Compile the real generated font/reset/settle script and check target selection
# against Terminal's current live and tabless windows without changing them.
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
probe_dir="$(mktemp -d /tmp/slab-tile-targets.XXXXXX)"
trap 'rm -rf "$probe_dir"' EXIT
osascript > "$probe_dir/closed-ids" <<'APPLESCRIPT'
set closedIds to {}
if application "Terminal" is running then
  tell application "Terminal"
    repeat with w in every window
      if (count tabs of w) is 0 then set end of closedIds to id of w
    end repeat
  end tell
end if
return closedIds
APPLESCRIPT
cp "$root/Sources/SlabMenubar/TerminalTileScript.swift" "$probe_dir/check.swift"
cat >> "$probe_dir/check.swift" <<'SWIFT'
let dir = CommandLine.arguments[1]
let closedIDs = try String(contentsOfFile: "\(dir)/closed-ids", encoding: .utf8)
    .split(separator: ",").compactMap { UInt32($0.trimmingCharacters(in: .whitespacesAndNewlines)) }
let placements = (closedIDs + [4294967295]).map {
    TerminalTileScript.Placement(id: $0,
        bounds: (left: 10, top: 20, right: 600, bottom: 500))
}
assert(TerminalTileScript.make(placements: [], fontSize: 12, resetZoom: true).isEmpty)
for reset in [false, true] {
    let script = TerminalTileScript.make(placements: placements, fontSize: 12, resetZoom: reset)
    try script.write(toFile: "\(dir)/tile-\(reset).applescript", atomically: true, encoding: .utf8)
}
let probe = TerminalTileScript.liveWindowHandler + "\n" + """
set accepted to 0
set rejected to 0
if application "Terminal" is running then
  tell application "Terminal" to set candidates to id of every window
  repeat with wid in candidates
    tell application "Terminal"
      set w to first window whose id is (contents of wid)
      set shouldAccept to ((count tabs of w) > 0 and not miniaturized of w)
    end tell
    set didAccept to false
    try
      my slabTileWindow(contents of wid)
      set didAccept to true
    end try
    if didAccept is not shouldAccept then error "Incorrect target validation"
    if didAccept then
      set accepted to accepted + 1
    else
      set rejected to rejected + 1
    end if
  end repeat
end if
try
  my slabTileWindow(4294967295)
  set missingAccepted to true
on error
  set missingAccepted to false
end try
if missingAccepted then error "Missing target accepted"
return "PASS live=" & accepted & " excluded=" & rejected & " missing target rejected"
"""
try probe.write(toFile: "\(dir)/probe.applescript", atomically: true, encoding: .utf8)
try TerminalTileScript.liveWindowIDs.write(toFile: "\(dir)/ids.applescript", atomically: true, encoding: .utf8)
SWIFT
swift "$probe_dir/check.swift" "$probe_dir"
for script in "$probe_dir"/*.applescript; do
  osacompile -o "$probe_dir/compiled.scpt" "$script"
done
osascript "$probe_dir/probe.applescript"
# Preserve all frame evidence so a resurrected ghost is a test failure.
snapshot() {
  osascript <<'APPLESCRIPT'
if application "Terminal" is not running then return "stopped"
tell application "Terminal"
  set report to ""
  repeat with w in every window
    set report to report & (id of w as text) & ":" & (bounds of w as text) & linefeed
  end repeat
  return report
end tell
APPLESCRIPT
}
snapshot > "$probe_dir/before"
# Executing a stale transaction must be harmless, even with explicit zoom reset.
osascript "$probe_dir/tile-false.applescript"
osascript "$probe_dir/tile-true.applescript"
osascript "$probe_dir/ids.applescript"
snapshot > "$probe_dir/after"
diff -u "$probe_dir/before" "$probe_dir/after"
echo "PASS stale transactions left all window frames unchanged"
