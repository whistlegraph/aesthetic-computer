#!/bin/bash
# ableton-sync.sh — copy Ableton Live's settings from one fleet Mac onto others. Idempotent.
#
#   bash ableton-sync.sh <host> [<host> ...]   # neo -> each host
#   bash ableton-sync.sh all                   # neo -> blueberry chicken panda
#   SOURCE=chicken bash ableton-sync.sh neo    # override the source of truth
#
# Live keeps its settings in ~/Library/Preferences/Ableton/Live <version>/. The file that
# matters is Preferences.cfg — an opaque binary blob holding everything set in Live's
# Preferences window: theme, UI zoom, audio device, warp/record/launch defaults, plug-in
# paths. It can't be merged, only copied whole. That's safe here because the fleet Macs are
# the same model (Mac17,5), so even the Core Audio device selection lands correctly.
#
# Live rewrites Preferences.cfg *on quit*. A Live left open on the target would therefore
# overwrite whatever we copy in, the moment it closes. This script refuses to touch a host
# with Live running rather than silently losing the sync.
#
# Copied:     Preferences.cfg, Library.cfg, User Remote Scripts/, ~/Music/Ableton/User Library/
# Not copied: Log.txt, Crash/, Undo/, CrashRecoveryInfo.cfg, Indexer.txt, PluginScanner.txt,
#             AddOns.txt — per-machine state (crash logs, plug-in scan results, which Packs
#             are installed) that each Mac should own.
#
# The target's existing settings folder is backed up to Live <version>.bak-<timestamp>
# before anything is written, so a bad sync is one `mv` away from undone.
#
# COLOUR IDENTITY: the per-machine Live tint (Color Hue / Intensity) lives inside
# Preferences.cfg, so this sync carries the SOURCE machine's hue to the target. Re-run
# `node ableton-theme.mjs <host>` afterwards to restore the target's own accent-matched tint;
# the script prints that reminder at the end.
set -euo pipefail

SOURCE="${SOURCE:-neo}"
PREFS_ROOT='~/Library/Preferences/Ableton'
USER_LIB='~/Music/Ableton/User Library'

hosts=("$@")
[ "${hosts[0]:-}" = "all" ] && hosts=(blueberry chicken panda)
[ ${#hosts[@]} -eq 0 ] && { echo "usage: bash ableton-sync.sh <host> [host...] | all"; exit 1; }

# The settings folder is versioned; sync the newest Live the source has actually run.
version="$(ssh -o ConnectTimeout=8 "$SOURCE" "ls $PREFS_ROOT 2>/dev/null | grep '^Live ' | sort -V | tail -1")"
[ -n "$version" ] || { echo "no Live settings folder found on $SOURCE"; exit 1; }
echo "source: $SOURCE — $version"

for host in "${hosts[@]}"; do
  echo "=== $host ==="
  [ "$host" = "$SOURCE" ] && { echo "  is the source, skipping"; continue; }

  # Ask the window server, not pgrep: `pgrep -f "Ableton Live"` matches the very shell ssh
  # spawns to run it (its own command line contains the pattern), so it always reports a hit.
  if [ "$(ssh -o ConnectTimeout=8 "$host" \
      'osascript -e "tell application \"System Events\" to return (exists process \"Live\")"')" = "true" ]; then
    echo "  ✗ Ableton Live is RUNNING on $host — quit it first (it rewrites its prefs on exit"
    echo "    and would clobber this sync). Skipping."
    continue
  fi

  # Back up whatever is there, then lay down the source's settings.
  ssh -o ConnectTimeout=8 "$host" bash -s -- "$version" <<'REMOTE'
set -euo pipefail
version="$1"
dir="$HOME/Library/Preferences/Ableton/$version"
if [ -d "$dir" ]; then
  backup="$dir.bak-$(date +%Y%m%d-%H%M%S)"
  cp -a "$dir" "$backup"
  echo "  backed up -> $(basename "$backup")"
else
  echo "  no existing '$version' folder — creating (target hasn't run this Live yet)"
fi
mkdir -p "$dir" "$HOME/Music/Ableton/User Library"
REMOTE

  # tar-pipe through this host: dependency-free, and merges rather than mirrors, so
  # anything the target has that the source doesn't (its own presets) survives.
  ssh -o ConnectTimeout=8 "$SOURCE" \
    "cd $PREFS_ROOT/'$version' && tar cf - Preferences.cfg Library.cfg 'User Remote Scripts' 2>/dev/null" \
    | ssh -o ConnectTimeout=8 "$host" "cd $PREFS_ROOT/'$version' && tar xf -"
  echo "  ✓ Preferences.cfg, Library.cfg, User Remote Scripts"

  ssh -o ConnectTimeout=8 "$SOURCE" "cd $USER_LIB && tar cf - . 2>/dev/null" \
    | ssh -o ConnectTimeout=8 "$host" "cd $USER_LIB && tar xf -"
  echo "  ✓ User Library (merged — the target's own presets are kept)"

  echo "  ↻ now: node ableton-theme.mjs $host   (restore $host's own accent-matched Live tint)"
done
