#!/bin/zsh
# Disposable live test for Slab's Terminal tiler.
#
# Usage:
#   ./test-tiling-smoke.zsh auto 4   # rely on population auto-tiling
#   ./test-tiling-smoke.zsh burst 4  # fire five rapid Cmd-Option-T requests
#   ./test-tiling-smoke.zsh churn 4  # minimize + restore one live window
#
# Safety: refuses to run when Terminal has any pre-existing window (including
# minimized ones) and closes only the exact ids it created. iTerm is untouched.

set -euo pipefail

mode="${1:-auto}"
count="${2:-4}"
if [[ "$mode" != auto && "$mode" != burst && "$mode" != churn ]]; then
  print -u2 "mode must be auto, burst, or churn"
  exit 2
fi
if [[ ! "$count" =~ '^[1-9][0-9]*$' || "$count" -gt 12 ]]; then
  print -u2 "window count must be 1..12"
  exit 2
fi

existing="$(osascript <<'APPLESCRIPT'
if application "Terminal" is not running then return ""
tell application "Terminal"
  set liveIds to {}
  repeat with w in every window
    try
      set ignoredTab to selected tab of w
      set end of liveIds to id of w
    end try
  end repeat
  return liveIds
end tell
APPLESCRIPT
)"
if [[ -n "${existing//[ ,]/}" ]]; then
  print -u2 "refusing: Terminal already has windows: $existing"
  exit 3
fi

typeset -a test_ids
prefix="SLAB-TILE-TEST-$$"

close_test_window() {
  local wid="$1"
  osascript - "$wid" <<'APPLESCRIPT' >/dev/null 2>&1 || true
on run argv
  set wid to (item 1 of argv) as integer
  set targetName to ""
  tell application "Terminal"
    if exists (first window whose id is wid) then
      set targetWindow to first window whose id is wid
      set targetName to name of targetWindow
      close targetWindow saving no
    end if
  end tell
  delay 0.15
  if targetName is not "" then
    tell application "System Events"
      tell process "Terminal"
        if exists (first window whose name is targetName) then
          tell first window whose name is targetName
            if (count of sheets) > 0 then click button "Close" of sheet 1
          end tell
        end if
      end tell
    end tell
  end if
end run
APPLESCRIPT
}

cleanup() {
  local wid
  for wid in $test_ids; do close_test_window "$wid"; done
  # The preflight guarantees the app contained no user window. Stop the now
  # test-only process after cleanup to clear AX objects that can linger for
  # several seconds after their real Window Server windows disappear.
  pkill -x Terminal >/dev/null 2>&1 || true
}
trap cleanup EXIT INT TERM

log_path=/tmp/slab-menubar.err
log_start=0
[[ -f "$log_path" ]] && log_start="$(stat -f %z "$log_path")"

for i in {$count..1}; do
  label="$prefix-$i"
  if (( ${#test_ids} == 0 )); then
    wid="$(osascript - "$label" <<'APPLESCRIPT'
on run argv
  set label to item 1 of argv
  tell application "Terminal"
    do script "exec zsh -l"
    set custom title of selected tab of front window to label
    return id of front window
  end tell
end run
APPLESCRIPT
)"
  else
    # Terminal can be configured so `do script` opens a tab. Cmd-N is the
    # invariant New Window action, so use it after the launch window exists.
    wid="$(osascript <<'APPLESCRIPT'
tell application "Terminal" to activate
tell application "System Events" to key code 45 using {command down}
delay 0.12
tell application "Terminal" to return id of front window
APPLESCRIPT
)"
  fi
  if [[ ! "$wid" =~ '^[0-9]+$' ]]; then
    print -u2 "could not create test window: $wid"
    exit 4
  fi
  test_ids+=("$wid")
  # Terminal's first Apple Event can return before launch finishes. Requests
  # sent in that gap all target the same new window and create tabs, which is
  # not a valid window-population test. Once the first id is established, a
  # short inter-window beat still keeps the remaining creation burst tight.
  if (( ${#test_ids} == 1 )); then sleep 0.5; else sleep 0.06; fi
done

if [[ "$mode" == burst ]]; then
  # Separate population auto-tiling from the hotkey transaction count. Older
  # builds do not auto-tile these ordinary windows; newer builds may emit one
  # or more population passes while Terminal is still creating them.
  sleep 1.2
  [[ -f "$log_path" ]] && log_start="$(stat -f %z "$log_path")"
  # One AppleScript process emits the whole burst so the requests arrive
  # inside the tiler's coalescing window rather than process-spawn intervals.
  osascript <<'APPLESCRIPT' >/dev/null
tell application "System Events"
  repeat 5 times
    key code 17 using {command down, option down}
  end repeat
end tell
APPLESCRIPT
fi

if [[ "$mode" == churn ]]; then
  # Exercise the minimized-window exclusion path and its two membership
  # transitions. This is where a stale AX object used to be double-counted or
  # a restoring Terminal could be missed depending on callback ordering.
  sleep 1.4
  churn_id="${test_ids[1]}"
  osascript - "$churn_id" <<'APPLESCRIPT' >/dev/null
on run argv
  set wid to (item 1 of argv) as integer
  tell application "Terminal" to set miniaturized of (first window whose id is wid) to true
end run
APPLESCRIPT
  sleep 1.4
  osascript - "$churn_id" <<'APPLESCRIPT' >/dev/null
on run argv
  set wid to (item 1 of argv) as integer
  tell application "Terminal" to set miniaturized of (first window whose id is wid) to false
end run
APPLESCRIPT
fi

snapshot() {
  local wid bounds
  for wid in $test_ids; do
    bounds="$(osascript - "$wid" <<'APPLESCRIPT'
on run argv
  set wid to (item 1 of argv) as integer
  tell application "Terminal"
    if not (exists (first window whose id is wid)) then return "missing"
    return bounds of first window whose id is wid
  end tell
end run
APPLESCRIPT
)"
    print -- "$wid=${bounds// /}"
  done
}

# Give auto population confirmation and Terminal's font reflow time to finish,
# then require two identical late snapshots. Burst mode is deliberately long
# enough to catch a queued second normalization transaction in older builds.
sleep 4
late_one="$(snapshot)"
sleep 0.5
late_two="$(snapshot)"
if [[ "$late_one" != "$late_two" ]]; then
  print -u2 "FAIL: frames still moving after settle"
  diff -u <(print -r -- "$late_one") <(print -r -- "$late_two") || true
  exit 5
fi

# Validate positive, unique, pairwise non-overlapping frames. Cascaded default
# Terminal windows fail the overlap check, so auto mode proves actual tiling.
if ! print -r -- "$late_two" | awk -F'[=,]' '
  NF != 5 || $2 == "missing" { exit 1 }
  {
    if ($4 <= $2 || $5 <= $3) exit 1
    key = $2 "," $3 "," $4 "," $5
    if (seen[key]++) exit 1
    left[NR]=$2; top[NR]=$3; right[NR]=$4; bottom[NR]=$5
  }
  END {
    for (i=1; i<=NR; i++) for (j=i+1; j<=NR; j++) {
      # Every window assigned to one row must end on the same pixel boundary.
      # This catches Terminal character-cell quantization making one pane a
      # text row taller even when pairwise overlap remains small.
      if (top[i] == top[j] && bottom[i] != bottom[j]) exit 1
      overlapX = (right[i] < right[j] ? right[i] : right[j]) - (left[i] > left[j] ? left[i] : left[j])
      overlapY = (bottom[i] < bottom[j] ? bottom[i] : bottom[j]) - (top[i] > top[j] ? top[i] : top[j])
      # Terminal frame chrome intentionally shares up to two pixels at cell
      # seams. Anything beyond that is a real stacked/cascaded overlap.
      if (overlapX > 3 && overlapY > 3) exit 1
    }
  }
'; then
  print -u2 "FAIL: missing, duplicate, invalid, or overlapping final frames"
  print -u2 -r -- "$late_two"
  exit 6
fi

tile_logs=0
if [[ -f "$log_path" ]]; then
  tile_logs="$(tail -c +$((log_start + 1)) "$log_path" | grep -c '\[tile\]' || true)"
fi

print "PASS mode=$mode windows=$count tile_log_lines=$tile_logs"
print -r -- "$late_two"
