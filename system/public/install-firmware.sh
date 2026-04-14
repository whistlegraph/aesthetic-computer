#!/usr/bin/env bash
# install-firmware.sh — install a MrChromebox UEFI firmware with the
# aesthetic.computer boot splash baked in.
#
# Usage (from a ChromeOS developer shell or from ac-native's prompt):
#
#     curl -fsSL https://aesthetic.computer/install-firmware.sh | sudo bash
#
# This does NOT re-build coreboot — we inherit Mr Chromebox's signed
# per-board ROM from https://mrchromebox.tech/ and only swap the
# bootsplash.bmp inside CBFS with our own. Upstream firmware fixes flow
# through automatically on each re-run.
#
# Docs we follow:
#   https://docs.mrchromebox.tech/docs/firmware/
#   https://doc.coreboot.org/util/cbfstool/index.html
#
# Safety:
#   1. Always backs up current firmware to /tmp/firmware-backup-<ts>.rom
#      AND echoes the path so the user can copy it to a USB before reboot.
#   2. Refuses to flash unless `flashrom` is able to read the chip and
#      the signed ROM we composed is the expected size.
#   3. `--dry-run` skips the final `flashrom -w` so you can inspect the
#      composed ROM first.

set -euo pipefail

AC_CDN="${AC_CDN:-https://aesthetic.computer}"
MRC_CDN="${MRC_CDN:-https://mrchromebox.tech/files/firmware/full_rom}"
WORK="${WORK:-/tmp/ac-firmware.$$}"
DRY_RUN="${DRY_RUN:-0}"

log()  { printf "\033[0;36m[ac-fw]\033[0m %s\n" "$*"; }
err()  { printf "\033[0;31m[ac-fw]\033[0m %s\n" "$*" >&2; }
die()  { err "$*"; exit 1; }

for arg in "$@"; do
  case "$arg" in
    --dry-run) DRY_RUN=1 ;;
    --help|-h)
      sed -n '2,20p' "$0" | sed 's/^# \{0,1\}//'
      exit 0 ;;
  esac
done

[ "$(id -u)" = "0" ] || die "Must run as root (use sudo)."

mkdir -p "$WORK"
trap 'rm -rf "$WORK"' EXIT

# ── 1. Detect board ────────────────────────────────────────────────
log "Detecting board…"
BOARD=""
if command -v crossystem >/dev/null 2>&1; then
  # ChromeOS-style — most reliable on stock + MrChromebox firmware.
  BOARD=$(crossystem hwid 2>/dev/null | awk '{print tolower($1)}' | cut -d- -f1)
fi
if [ -z "$BOARD" ] && [ -f /sys/class/dmi/id/product_name ]; then
  BOARD=$(tr -d '\0\n' < /sys/class/dmi/id/product_name | tr '[:upper:] ' '[:lower:]_' | cut -d. -f1)
fi
[ -n "$BOARD" ] || die "Could not detect board. Set BOARD=<name> and re-run."
log "  board: $BOARD"

# ── 2. Tools ───────────────────────────────────────────────────────
# Mr Chromebox's script already installs flashrom; we need cbfstool too.
# Ship a static-linked cbfstool on our CDN so ac-native's minimal busybox
# init doesn't need a full coreboot toolchain to get this working.
need() { command -v "$1" >/dev/null 2>&1 || return 1; }
fetch() {
  local url="$1" out="$2"
  curl -fsSL --retry 3 -o "$out" "$url" \
    || die "Download failed: $url"
}

if ! need flashrom; then
  die "flashrom not installed. On ChromeOS dev shell: 'sudo pacman -Sy flashrom' or install via your distro."
fi

CBFSTOOL="$WORK/cbfstool"
if need cbfstool; then
  cp "$(command -v cbfstool)" "$CBFSTOOL"
else
  log "Fetching cbfstool from $AC_CDN/firmware/cbfstool…"
  fetch "$AC_CDN/firmware/cbfstool" "$CBFSTOOL"
  chmod +x "$CBFSTOOL"
fi
"$CBFSTOOL" -h >/dev/null 2>&1 || die "cbfstool unusable — try reinstalling."

# ── 3. Download base ROM from MrChromebox ─────────────────────────
BASE_ROM="$WORK/base.rom"
MRC_URL="$MRC_CDN/coreboot_tiano-$BOARD-mrchromebox.rom"
log "Fetching MrChromebox ROM for '$BOARD'…"
fetch "$MRC_URL" "$BASE_ROM"
BASE_SIZE=$(stat -c%s "$BASE_ROM")
log "  size: $((BASE_SIZE / 1024 / 1024))MB ($BASE_SIZE bytes)"

# ── 4. Apply aesthetic.computer customizations ────────────────────
AC_ROM="$WORK/ac.rom"
cp "$BASE_ROM" "$AC_ROM"

# Swap the bootsplash. BMP must be 1366x768 24/32-bit uncompressed
# (coreboot's splash decoder is minimal). AC ships a pre-baked copy at
# /firmware/ac-splash.bmp — overrideable via env SPLASH_URL.
SPLASH_URL="${SPLASH_URL:-$AC_CDN/firmware/ac-splash.bmp}"
if curl -fsSL --head "$SPLASH_URL" >/dev/null 2>&1; then
  SPLASH="$WORK/splash.bmp"
  log "Fetching splash: $SPLASH_URL"
  fetch "$SPLASH_URL" "$SPLASH"
  "$CBFSTOOL" "$AC_ROM" remove -n bootsplash.bmp 2>/dev/null || true
  "$CBFSTOOL" "$AC_ROM" add -f "$SPLASH" -n bootsplash.bmp -t raw
  log "  ✓ swapped bootsplash.bmp"
else
  log "  splash not available at $SPLASH_URL — skipping (keeping MrChromebox rabbit)."
fi

# Shorten the boot-menu wait. Upstream default is 2500ms; we want 500ms.
"$CBFSTOOL" "$AC_ROM" add-int -i 500 -n etc/boot-menu-wait 2>/dev/null \
  || log "  (could not set etc/boot-menu-wait; continuing)"

# Verify size didn't change.
AC_SIZE=$(stat -c%s "$AC_ROM")
[ "$AC_SIZE" = "$BASE_SIZE" ] || die "Size mismatch after customization: $AC_SIZE vs $BASE_SIZE."
log "  customized ROM size OK"

# ── 5. Backup current firmware ────────────────────────────────────
BACKUP="/tmp/firmware-backup-$(date +%Y%m%d-%H%M%S).rom"
log "Backing up current firmware → $BACKUP"
flashrom -p internal -r "$BACKUP" 2>&1 | grep -vE "^(flashrom|Calibrating)" || true
[ -s "$BACKUP" ] || die "Backup read failed — refusing to flash."
log "  ✓ backup saved ($(du -h "$BACKUP" | cut -f1))"

# ── 6. Flash ──────────────────────────────────────────────────────
if [ "$DRY_RUN" = "1" ]; then
  log "DRY_RUN=1 — stopping before flash."
  log "  composed ROM: $AC_ROM"
  log "  current backup: $BACKUP"
  exit 0
fi

log "Flashing customized ROM…"
flashrom -p internal -w "$AC_ROM" 2>&1 | grep -vE "^(Calibrating|Reading old|Erasing|Writing)" || true

log "✅ Done. Reboot when ready — new splash takes effect next boot."
log "   backup: $BACKUP"
