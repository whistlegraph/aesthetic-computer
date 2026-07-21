#!/bin/bash
# Cleaner — safe, user-level macOS disk inventory/cleanup for the AC fleet Macs.
# Default is report-only; known regenerable caches require --apply.
set -euo pipefail

APPLY=0
INSTALL=0
THIN_SNAPSHOTS=0

usage() {
  cat <<'EOF'
Usage: cleaner [--apply] [--thin-snapshots] [--install]

  (no args)          Report disk use and cleanup candidates; change nothing.
  --apply            Clear known regenerable user caches when their app is idle.
  --thin-snapshots   With --apply, ask tmutil to reclaim local snapshots.
  --install          Install in ~/.local/bin and enable a weekly LaunchAgent.

Protected: repositories, node_modules, Downloads, Documents/Shelf, model
weights, Codex/Claude state, mail archives, application data, and caches owned
by a currently running application.
EOF
}

while (($#)); do
  case "$1" in
    --apply) APPLY=1 ;;
    --install) INSTALL=1 ;;
    --thin-snapshots) THIN_SNAPSHOTS=1 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

human_size() {
  if [[ -e "$1" ]]; then
    du -sh -- "$1" 2>/dev/null | awk '{print $1}' || echo "?"
  else
    echo "0"
  fi
}

data_volume() {
  [[ -d /System/Volumes/Data ]] && echo /System/Volumes/Data || echo /
}

disk_used_kb() {
  df -k "$(data_volume)" | awk 'NR == 2 {print $3}'
}

report() {
  local volume snapshots
  volume=$(data_volume)
  snapshots=$(tmutil listlocalsnapshots / 2>/dev/null | grep -c '^com\.apple\.' || true)

  echo "Cleaner on $(scutil --get LocalHostName 2>/dev/null || hostname) — $(date '+%Y-%m-%dT%H:%M:%S%z')"
  echo
  df -h "$volume" | awk 'NR == 1 || NR == 2'
  echo "Local snapshots: $snapshots"
  echo
  printf '%-35s %9s  %s\n' "Path/category" "Size" "Policy"
  printf '%-35s %9s  %s\n' "-----------------------------------" "---------" "------"
  printf '%-35s %9s  %s\n' "aesthetic-computer/.git" "$(human_size "$HOME/aesthetic-computer/.git")" "PROTECTED"
  printf '%-35s %9s  %s\n' "aesthetic-computer/node_modules" "$(human_size "$HOME/aesthetic-computer/node_modules")" "PROTECTED"
  printf '%-35s %9s  %s\n' "Documents/Shelf" "$(human_size "$HOME/Documents/Shelf")" "PROTECTED; curate manually"
  printf '%-35s %9s  %s\n' "Chrome cache" "$(human_size "$HOME/Library/Caches/Google")" "safe when Chrome idle"
  printf '%-35s %9s  %s\n' "Puppeteer cache" "$(human_size "$HOME/.cache/puppeteer")" "safe when browser tooling idle"
  printf '%-35s %9s  %s\n' "Spotify cache" "$(human_size "$HOME/Library/Caches/com.spotify.client")" "safe when Spotify idle"
  printf '%-35s %9s  %s\n' "pip cache" "$(human_size "$HOME/Library/Caches/pip")" "safe when pip idle"
  printf '%-35s %9s  %s\n' "Messages cache" "$(human_size "$HOME/Library/Messages/Caches")" "safe when Messages idle"
  printf '%-35s %9s  %s\n' "npm content cache" "$(human_size "$HOME/.npm/_cacache")" "safe when node/npm idle"
  printf '%-35s %9s  %s\n' "Slab recordings" "$(human_size "$HOME/.local/share/slab/sessions")" "keeps last 7 days"
  printf '%-35s %9s  %s\n' "Trash" "$(human_size "$HOME/.Trash")" "safe with --apply"
  printf '%-35s %9s  %s\n' "CoreSimulator volumes" "$(human_size /Library/Developer/CoreSimulator/Volumes)" "report only"
  echo
  if ((APPLY == 0)); then
    echo "Report only. Run 'cleaner --apply' to clear safe caches."
  fi
}

clean_contents() {
  [[ -d "$1" ]] || return 0
  find "$1" -mindepth 1 -maxdepth 1 -exec rm -rf -- {} +
}

skip() {
  echo "  skip: $1"
}

apply_cleanup() {
  local before after reclaimed
  before=$(disk_used_kb)
  echo "Applying safe user-cache cleanup..."

  if pgrep -f '/Google Chrome( |$)|Google Chrome Helper' >/dev/null; then
    skip "Chrome is active; Chrome and Puppeteer caches kept"
  else
    clean_contents "$HOME/Library/Caches/Google"
    clean_contents "$HOME/.cache/puppeteer"
  fi

  if pgrep -x Spotify >/dev/null; then
    skip "Spotify is active; its cache kept"
  else
    clean_contents "$HOME/Library/Caches/com.spotify.client"
  fi

  if pgrep -x Messages >/dev/null; then
    skip "Messages is active; its cache kept"
  else
    clean_contents "$HOME/Library/Messages/Caches"
  fi

  if pgrep -f '(^|/)(pip|pip3)( |$)' >/dev/null; then
    skip "pip is active; its cache kept"
  else
    clean_contents "$HOME/Library/Caches/pip"
  fi

  if pgrep -f '(^|/)(node|npm|pnpm|yarn)( |$)' >/dev/null; then
    skip "node/npm workload is active; npm cache kept"
  else
    clean_contents "$HOME/.npm/_cacache"
  fi

  if pgrep -f '(^|/)(swift|swiftc|xcodebuild)( |$)' >/dev/null; then
    skip "Swift/Xcode build is active; .build directories kept"
  elif [[ -d "$HOME/aesthetic-computer" ]]; then
    find "$HOME/aesthetic-computer" -name .build -type d -prune -exec rm -rf -- {} + 2>/dev/null || true
  fi

  if [[ -d "$HOME/.local/share/slab/sessions" ]]; then
    find "$HOME/.local/share/slab/sessions" -maxdepth 1 -type f -mtime +7 -delete
  fi
  clean_contents "$HOME/.Trash"

  if ! pgrep -x brew >/dev/null; then
    if [[ -x /opt/homebrew/bin/brew ]]; then
      /opt/homebrew/bin/brew cleanup --prune=all >/dev/null 2>&1 || true
    elif [[ -x /usr/local/bin/brew ]]; then
      /usr/local/bin/brew cleanup --prune=all >/dev/null 2>&1 || true
    fi
  fi

  if ((THIN_SNAPSHOTS)); then
    tmutil thinlocalsnapshots / 10000000000 4 >/dev/null 2>&1 || skip "tmutil could not thin snapshots without administrator approval"
  fi

  after=$(disk_used_kb)
  reclaimed=$((before - after))
  ((reclaimed < 0)) && reclaimed=0
  echo "Cleanup complete: $(awk -v kb="$reclaimed" 'BEGIN {printf "%.1f MiB", kb / 1024}') reclaimed."
  df -h "$(data_volume)" | awk 'NR == 1 || NR == 2'
}

install_utility() {
  local bin_dir agent_dir plist source uid
  bin_dir="$HOME/.local/bin"
  agent_dir="$HOME/Library/LaunchAgents"
  plist="$agent_dir/computer.aesthetic.cleaner.plist"
  source=$(cd "$(dirname "$0")" && pwd)/$(basename "$0")
  uid=$(id -u)
  mkdir -p "$bin_dir" "$agent_dir" "$HOME/Library/Logs"
  if [[ "$source" != "$bin_dir/cleaner" ]]; then
    install -m 0755 "$source" "$bin_dir/cleaner"
  else
    chmod 0755 "$bin_dir/cleaner"
  fi
  ln -sfn cleaner "$bin_dir/ac-disk-clean"

  cat >"$plist" <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0"><dict>
  <key>Label</key><string>computer.aesthetic.cleaner</string>
  <key>ProgramArguments</key><array>
    <string>$bin_dir/cleaner</string><string>--apply</string>
  </array>
  <key>StartCalendarInterval</key><dict>
    <key>Weekday</key><integer>1</integer><key>Hour</key><integer>3</integer><key>Minute</key><integer>15</integer>
  </dict>
  <key>StandardOutPath</key><string>$HOME/Library/Logs/cleaner.log</string>
  <key>StandardErrorPath</key><string>$HOME/Library/Logs/cleaner.log</string>
</dict></plist>
EOF

  plutil -lint "$plist"
  launchctl bootout "gui/$uid/computer.aesthetic.cleaner" >/dev/null 2>&1 || true
  launchctl bootstrap "gui/$uid" "$plist"
  echo "Installed $bin_dir/cleaner (compatibility alias: ac-disk-clean) and weekly LaunchAgent."
}

if ((INSTALL)); then
  install_utility
  exit 0
fi

report
if ((APPLY)); then
  echo
  apply_cleanup
fi
