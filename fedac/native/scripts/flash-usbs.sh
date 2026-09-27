#!/bin/bash
# flash-usbs.sh — flash every attached external physical disk with the cached
# AC OS release in /tmp/ac-os-pull. Dry run by default: lists the disks it
# would WIPE. Pass --go to flash them all, one after another.
# Disks over 256 GB are skipped unless --big is also passed (backup-drive guard).
set -uo pipefail
SRC=/tmp/ac-os-pull
FLASH=/Users/jas/aesthetic-computer/fedac/native/scripts/flash-mac.sh
LOG="${LOG:-$HOME/.ac-os/flash-six.log}"
mkdir -p "$(dirname "$LOG")"
GO=0; BIG=0
for a in "$@"; do [ "$a" = "--go" ] && GO=1; [ "$a" = "--big" ] && BIG=1; done
[ -f "$SRC/vmlinuz" ] && [ -f "$SRC/initramfs.cpio.gz" ] || { echo "cache missing in $SRC — run: ac-os pull"; exit 1; }
DISKS=$(diskutil list external physical 2>/dev/null | awk '/^\/dev\/disk/{print $1}')
[ -z "$DISKS" ] && { echo "no external disks attached"; exit 1; }
TARGETS=()
for d in $DISKS; do
  info=$(diskutil info "$d")
  name=$(echo "$info" | awk -F': *' '/Device \/ Media Name/{print $2}')
  bytes=$(echo "$info" | awk -F'[()]' '/Disk Size/{print $2}' | awk '{print $1}')
  gb=$(( ${bytes:-0} / 1000000000 ))
  removable=$(echo "$info" | awk -F': *' '/Removable Media/{print $2}')
  vols=$(diskutil list "$d" | awk 'NR>2 && $NF ~ /disk/ {print $(NF-3)}' | tr '\n' ',' | sed 's/,$//')
  flag=""
  if [ "$gb" -gt 256 ] && [ "$BIG" -ne 1 ]; then flag="  SKIP (>256 GB, pass --big)"; else TARGETS+=("$d"); fi
  printf '%-12s %-28s %5s GB  removable=%-3s vols=%s%s\n' "$d" "$name" "$gb" "$removable" "$vols" "$flag"
done
[ "$GO" -eq 1 ] || { echo; echo "dry run — pass --go to ERASE and flash the ${#TARGETS[@]} disk(s) above"; exit 0; }
ok=0; bad=0
for d in "${TARGETS[@]}"; do
  echo; echo "=== $(date '+%H:%M:%S') flashing $d"
  if sudo "$FLASH" "$d" "$SRC" >>"$LOG" 2>&1; then
    echo "$(date '+%H:%M:%S') DONE $d"; ok=$((ok+1))
    echo "$(date -u +%FT%TZ) DONE $d" >> "$LOG"
  else
    echo "$(date '+%H:%M:%S') FAILED $d — see $LOG"; bad=$((bad+1)); tail -8 "$LOG"
  fi
done
echo; echo "flashed $ok, failed $bad, log: $LOG"
[ "$bad" -eq 0 ]
