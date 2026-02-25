#!/bin/bash
# make-kiosk-piece-usb.sh — Create a FedAC kiosk USB with a bundled KidLisp piece
#
# Pipeline: fetch piece bundle from oven → inject into Fedora live rootfs → flash to USB
#
# Boot flow:
#   Power on → GRUB (instant) → PALS Plymouth splash → mutter (no desktop)
#   → Firefox fullscreen → bundled piece (offline, no WiFi needed)
#
# Usage:
#   sudo bash fedac/scripts/make-kiosk-piece-usb.sh <piece-code> [<device>] [options]
#
# Examples:
#   sudo bash fedac/scripts/make-kiosk-piece-usb.sh roz /dev/sdb
#   sudo bash fedac/scripts/make-kiosk-piece-usb.sh '$cow' --image /tmp/fedac-roz.img
#   sudo bash fedac/scripts/make-kiosk-piece-usb.sh roz /dev/sdb --image /tmp/fedac-roz.img
#
# Options:
#   --rootfs <dir>    Use existing extracted rootfs (skip ISO download + extract)
#   --iso <path>      Use local ISO instead of downloading
#   --image <path>    Build a bootable disk image file (.img)
#   --image-size <g>  Image size in GiB (default: 16)
#   --density <n>     Default pack/runtime density query value (default: 8)
#   --work-base <dir> Directory for large temp work files (default: /tmp)
#   --no-eject        Don't eject when done
#   --yes             Skip confirmation prompts
#
# Requirements:
#   erofs-utils (mkfs.erofs), curl, dracut (for initrd rebuild)
#   ~5GB free temp space (or less if using --rootfs)

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
PINK='\033[38;5;205m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FEDAC_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
OVERLAY_DIR="$FEDAC_DIR/overlays/kiosk"
REPO_ROOT="$(cd "$FEDAC_DIR/.." && pwd)"

# Fedora 43 Workstation
FEDORA_VERSION="43"
FEDORA_RELEASE="43-1.6"
FEDORA_ISO_NAME="Fedora-Workstation-Live-${FEDORA_RELEASE}.x86_64.iso"
FEDORA_URL="https://dl.fedoraproject.org/pub/fedora/linux/releases/${FEDORA_VERSION}/Workstation/x86_64/iso/${FEDORA_ISO_NAME}"
FEDORA_SHA256="2a4a16c009244eb5ab2198700eb04103793b62407e8596f30a3e0cc8ac294d77"
CACHE_DIR="${HOME}/Downloads"

# Oven pack/bundle endpoints (pack is preferred; bundle kept as fallback)
OVEN_PACK_URL="https://oven.aesthetic.computer/pack-html"
OVEN_BUNDLE_URL="https://oven.aesthetic.computer/bundle-html"
PACK_DENSITY_DEFAULT="8"

usage() {
  echo -e "${PINK}FedAC Kiosk Piece USB Creator${NC}"
  echo ""
  echo "Creates a live USB that boots into a fullscreen KidLisp piece (offline)."
  echo ""
  echo "Usage: sudo $0 <piece-code> [<device>] [options]"
  echo ""
  echo "  <piece-code>    KidLisp piece code (e.g., roz, \$cow)"
  echo "  <device>        Optional USB block device (e.g., /dev/sdb)"
  echo ""
  echo "Options:"
  echo "  --rootfs <dir>  Use existing extracted rootfs directory"
  echo "  --iso <path>    Use existing ISO instead of downloading"
  echo "  --density <n>   Default pack/runtime density query value (default: 8)"
  echo "  --no-eject      Don't eject the USB when done"
  echo "  --yes           Skip confirmation prompts"
  echo "  --help          Show this help"
  exit 1
}

cleanup() {
  echo -e "\n${YELLOW}Cleaning up mounts...${NC}"
  for mp in "${ISO_MOUNT:-}" "${ROOTFS_MOUNT:-}" "${EFI_MOUNT:-}" "${LIVE_MOUNT:-}"; do
    [ -n "$mp" ] && mountpoint -q "$mp" 2>/dev/null && umount "$mp" 2>/dev/null || true
  done
  if [ -n "${LOOP_DEV:-}" ]; then
    losetup -d "$LOOP_DEV" 2>/dev/null || true
    LOOP_DEV=""
  fi
}
trap cleanup EXIT

# ── Parse args ──
PIECE_CODE=""
DEVICE=""
ROOTFS_DIR=""
ISO_PATH=""
IMAGE_PATH=""
IMAGE_SIZE_GB="16"
WORK_BASE="/tmp"
DO_EJECT=true
SKIP_CONFIRM=false
PACK_DENSITY="$PACK_DENSITY_DEFAULT"

while [ $# -gt 0 ]; do
  case "$1" in
    --rootfs) ROOTFS_DIR="$2"; shift 2 ;;
    --iso) ISO_PATH="$2"; shift 2 ;;
    --image) IMAGE_PATH="$2"; shift 2 ;;
    --image-size) IMAGE_SIZE_GB="$2"; shift 2 ;;
    --density) PACK_DENSITY="$2"; shift 2 ;;
    --work-base) WORK_BASE="$2"; shift 2 ;;
    --no-eject) DO_EJECT=false; shift ;;
    --yes) SKIP_CONFIRM=true; shift ;;
    --help|-h) usage ;;
    /dev/*) DEVICE="$1"; shift ;;
    *) if [ -z "$PIECE_CODE" ]; then PIECE_CODE="$1"; shift; else echo -e "${RED}Unknown arg: $1${NC}"; usage; fi ;;
  esac
done

[ -n "$PIECE_CODE" ] || { echo -e "${RED}Error: No piece code specified${NC}"; usage; }
[ -n "$DEVICE" ] || [ -n "$IMAGE_PATH" ] || {
  echo -e "${RED}Error: Specify a USB device and/or --image path${NC}"
  usage
}
[ -z "$DEVICE" ] || [ -b "$DEVICE" ] || { echo -e "${RED}Error: $DEVICE is not a block device${NC}"; exit 1; }
[ -z "$IMAGE_PATH" ] || [[ "$IMAGE_SIZE_GB" =~ ^[0-9]+$ ]] || {
  echo -e "${RED}Error: --image-size must be an integer GiB value${NC}"
  exit 1
}
[[ "$PACK_DENSITY" =~ ^[1-9][0-9]*$ ]] || {
  echo -e "${RED}Error: --density must be a positive integer${NC}"
  exit 1
}
[ -d "$WORK_BASE" ] || mkdir -p "$WORK_BASE"
[ -d "$WORK_BASE" ] || {
  echo -e "${RED}Error: --work-base '$WORK_BASE' is not accessible${NC}"
  exit 1
}

if [ "$(id -u)" -ne 0 ]; then
  echo -e "${RED}Error: Must run as root (sudo)${NC}"
  exit 1
fi

# Safety: refuse system disks
if [ -n "$DEVICE" ]; then
  case "$DEVICE" in
    /dev/nvme*|/dev/vda|/dev/xvda)
      echo -e "${RED}REFUSED: $DEVICE looks like a system disk.${NC}"
      exit 1
      ;;
    /dev/sda)
      RM_FLAG="$(lsblk -dn -o RM "$DEVICE" 2>/dev/null || echo "")"
      if [ "$RM_FLAG" != "1" ]; then
        echo -e "${RED}REFUSED: $DEVICE looks like a system disk.${NC}"
        exit 1
      fi
      echo -e "${YELLOW}Warning: allowing /dev/sda because lsblk reports RM=1 (removable).${NC}"
      ;;
  esac
fi

# Check tools
for tool in mkfs.erofs curl; do
  command -v "$tool" &>/dev/null || { echo -e "${RED}Error: $tool not found${NC}"; exit 1; }
done
if [ -n "$IMAGE_PATH" ]; then
  for tool in losetup; do
    command -v "$tool" &>/dev/null || { echo -e "${RED}Error: $tool not found${NC}"; exit 1; }
  done
fi

echo -e "${PINK}╔═══════════════════════════════════════════════╗${NC}"
echo -e "${PINK}║     FedAC Kiosk Piece USB Creator             ║${NC}"
echo -e "${PINK}║  Piece: ${CYAN}${PIECE_CODE}${PINK} → offline bootable USB       ║${NC}"
echo -e "${PINK}╚═══════════════════════════════════════════════╝${NC}"
echo ""

WORK_DIR=$(mktemp -d "$WORK_BASE/fedac-kiosk-XXXX")
echo -e "Work dir: $WORK_DIR"
echo ""

# ══════════════════════════════════════════
# Step 1: Fetch piece bundle from oven
# ══════════════════════════════════════════
echo -e "${CYAN}[1/6] Fetching piece bundle: ${PIECE_CODE}...${NC}"

BUNDLE_PATH="$WORK_DIR/piece.html"
HTTP_CODE="000"
USED_ENDPOINT=""
USED_MODE=""

if [[ "$PIECE_CODE" == \$* ]]; then
  BUNDLE_MODES=(code piece)
else
  BUNDLE_MODES=(piece code)
fi

for endpoint in "$OVEN_PACK_URL" "$OVEN_BUNDLE_URL"; do
  for mode in "${BUNDLE_MODES[@]}"; do
    BUNDLE_URL="${endpoint}?${mode}=${PIECE_CODE}&nocompress=1&density=${PACK_DENSITY}"
    echo -e "  URL: $BUNDLE_URL"
    HTTP_CODE="$(curl -s -o "$BUNDLE_PATH" -w "%{http_code}" "$BUNDLE_URL" || echo "000")"
    if [ "$HTTP_CODE" = "200" ]; then
      USED_ENDPOINT="$endpoint"
      USED_MODE="$mode"
      break 2
    fi
    echo -e "  ${YELLOW}${mode} via ${endpoint##*/} failed (HTTP ${HTTP_CODE})${NC}"
  done
done

if [ "$HTTP_CODE" != "200" ]; then
  echo -e "${RED}Failed to fetch bundle (HTTP $HTTP_CODE)${NC}"
  echo "  Tried pack-html and bundle-html with code/piece modes."
  echo "  Check that '$PIECE_CODE' is a valid piece code/piece slug."
  exit 1
fi

BUNDLE_SIZE=$(stat -c%s "$BUNDLE_PATH")
echo -e "  ${GREEN}Bundle fetched: $(numfmt --to=iec $BUNDLE_SIZE)${NC}"
echo -e "  ${GREEN}Source: ${USED_ENDPOINT##*/} (${USED_MODE}), density=${PACK_DENSITY}${NC}"

# Quick sanity check — bundle should contain HTML
if ! head -1 "$BUNDLE_PATH" | grep -qi "<!DOCTYPE\|<html"; then
  echo -e "${YELLOW}Warning: Bundle doesn't look like HTML. First line:${NC}"
  head -1 "$BUNDLE_PATH"
fi

# ══════════════════════════════════════════
# Step 2: Get or prepare rootfs
# ══════════════════════════════════════════
echo -e "${CYAN}[2/6] Preparing rootfs...${NC}"

OWN_ROOTFS=false
if [ -n "$ROOTFS_DIR" ]; then
  echo -e "  Using existing rootfs: ${GREEN}$ROOTFS_DIR${NC}"
  if [ ! -d "$ROOTFS_DIR/usr" ]; then
    echo -e "${RED}Error: $ROOTFS_DIR doesn't look like a rootfs (no /usr)${NC}"
    exit 1
  fi
else
  # Need to extract from ISO
  OWN_ROOTFS=true
  ROOTFS_DIR="$WORK_DIR/rootfs"

  if [ -z "$ISO_PATH" ]; then
    ISO_PATH="${CACHE_DIR}/${FEDORA_ISO_NAME}"
    if [ -f "$ISO_PATH" ]; then
      echo -e "  Found cached ISO: ${GREEN}$ISO_PATH${NC}"
    else
      echo -e "  Downloading Fedora ${FEDORA_VERSION} ISO (~2.6 GB)..."
      mkdir -p "$CACHE_DIR"
      curl -L --progress-bar -o "$ISO_PATH" "$FEDORA_URL"
    fi
  fi

  echo -e "  Extracting rootfs from ISO (takes a few minutes)..."
  ISO_MOUNT=$(mktemp -d /tmp/fedac-iso-XXXX)
  mount -o loop,ro "$ISO_PATH" "$ISO_MOUNT"

  SQUASH_IMG="$ISO_MOUNT/LiveOS/squashfs.img"
  [ -f "$SQUASH_IMG" ] || { echo -e "${RED}No LiveOS/squashfs.img in ISO${NC}"; exit 1; }

  # Mount the EROFS/squashfs image
  ROOTFS_MOUNT=$(mktemp -d /tmp/fedac-rootfs-mount-XXXX)
  mount -o loop,ro "$SQUASH_IMG" "$ROOTFS_MOUNT"

  # Copy rootfs to work dir
  echo -e "  Copying rootfs (this takes a while)..."
  mkdir -p "$ROOTFS_DIR"
  cp -a "$ROOTFS_MOUNT"/* "$ROOTFS_DIR"/

  umount "$ROOTFS_MOUNT"
  rmdir "$ROOTFS_MOUNT"
  ROOTFS_MOUNT=""
  umount "$ISO_MOUNT"
  rmdir "$ISO_MOUNT"
  ISO_MOUNT=""
  echo -e "  ${GREEN}Rootfs extracted${NC}"
fi

# ══════════════════════════════════════════
# Step 3: Inject kiosk config into rootfs
# ══════════════════════════════════════════
echo -e "${CYAN}[3/6] Injecting kiosk config...${NC}"

# 3a. Place the piece bundle
mkdir -p "$ROOTFS_DIR/usr/local/share/kiosk"
cp "$BUNDLE_PATH" "$ROOTFS_DIR/usr/local/share/kiosk/piece.html"
echo -e "  ${GREEN}Piece bundle installed${NC}"
KIOSK_PIECE_URL="file:///usr/local/share/kiosk/piece.html?density=${PACK_DENSITY}"

# Ensure cage is available (black, no-animation kiosk compositor).
# If install fails (offline builds), mutter fallback is used in kiosk-session.sh.
if [ ! -x "$ROOTFS_DIR/usr/bin/cage" ] && command -v dnf >/dev/null 2>&1; then
  echo -e "  Installing cage into rootfs..."
  dnf -y --installroot="$ROOTFS_DIR" --releasever="$FEDORA_VERSION" install cage >/dev/null 2>&1 || true
fi

# Ensure actkbd is available to map hardware volume keys to system volume.
if [ ! -x "$ROOTFS_DIR/usr/sbin/actkbd" ] && [ ! -x "$ROOTFS_DIR/usr/bin/actkbd" ] && command -v dnf >/dev/null 2>&1; then
  echo -e "  Installing actkbd into rootfs (volume keys)..."
  dnf -y --installroot="$ROOTFS_DIR" --releasever="$FEDORA_VERSION" install actkbd >/dev/null 2>&1 || true
fi

# 3b. Kiosk session script
cat > "$ROOTFS_DIR/usr/local/bin/kiosk-session.sh" << 'SESSEOF'
#!/bin/bash
# FedAC Kiosk Session — prefer cage (black background, no window animations).
set -euo pipefail
export XDG_SESSION_TYPE=wayland
export XDG_RUNTIME_DIR="/run/user/$(id -u)"
export MOZ_ENABLE_WAYLAND=1
export MOZ_DBUS_REMOTE=0
export NO_AT_BRIDGE=1
export CLUTTER_DISABLE_ANIMATIONS=1
PROFILE="/home/liveuser/.mozilla/firefox/kiosk"

mkdir -p "$PROFILE/chrome"
cat > "$PROFILE/user.js" << 'USERJSEOF'
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
user_pref("browser.sessionstore.resume_from_crash", false);
user_pref("browser.tabs.drawInTitlebar", false);
user_pref("browser.tabs.inTitlebar", 0);
user_pref("browser.startup.homepage", "__KIOSK_PIECE_URL__");
user_pref("browser.startup.page", 1);
USERJSEOF

cat > "$PROFILE/chrome/userChrome.css" << 'CHROMEEOF'
#TabsToolbar,
#titlebar,
#toolbar-menubar,
#nav-bar,
#PersonalToolbar {
  visibility: collapse !important;
  min-height: 0 !important;
  max-height: 0 !important;
}
CHROMEEOF

# Start PipeWire audio stack (cage doesn't launch a full desktop session).
# PipeWire needs XDG_RUNTIME_DIR which we set above.
mkdir -p "$XDG_RUNTIME_DIR"
if command -v pipewire >/dev/null 2>&1; then
  pipewire &
  sleep 0.3
  command -v wireplumber >/dev/null 2>&1 && wireplumber &
  command -v pipewire-pulse >/dev/null 2>&1 && pipewire-pulse &
  sleep 0.2
fi

if command -v cage >/dev/null 2>&1; then
  TTY_DEV="$(tty 2>/dev/null || true)"
  [ -n "$TTY_DEV" ] && printf '\033[2J\033[3J\033[H\033[?25l' >"$TTY_DEV" 2>/dev/null || true
  exec cage -- firefox --kiosk --no-remote --new-instance \
    --profile "$PROFILE" --private-window \
    __KIOSK_PIECE_URL__
fi

exec mutter --wayland --no-x11 --sm-disable -- firefox --kiosk --no-remote \
  --new-instance --profile "$PROFILE" --private-window \
  __KIOSK_PIECE_URL__
SESSEOF
sed -i "s|__KIOSK_PIECE_URL__|$KIOSK_PIECE_URL|g" "$ROOTFS_DIR/usr/local/bin/kiosk-session.sh"
chmod +x "$ROOTFS_DIR/usr/local/bin/kiosk-session.sh"
echo -e "  ${GREEN}Kiosk session script installed${NC}"

# 3c. Disable GDM and force tty1 autologin -> kiosk-session.sh
# This path is resilient on Fedora live images even when graphical.target isn't reached.
mkdir -p "$ROOTFS_DIR/etc/systemd/system/getty@tty1.service.d"

# Mask GDM so it never starts
ln -sf /dev/null "$ROOTFS_DIR/etc/systemd/system/gdm.service"
ln -sf /dev/null "$ROOTFS_DIR/etc/systemd/system/display-manager.service"

cat > "$ROOTFS_DIR/etc/systemd/system/getty@tty1.service.d/autologin.conf" << 'AGEOF'
[Service]
ExecStart=
ExecStart=-/sbin/agetty --noissue --nonewline --autologin liveuser --noclear %I $TERM
Type=idle
AGEOF

# Some minimal roots don't ship a liveuser account; create it for tty1 autologin.
if ! chroot "$ROOTFS_DIR" /usr/bin/getent passwd liveuser >/dev/null 2>&1; then
  chroot "$ROOTFS_DIR" /usr/sbin/useradd -m -s /bin/bash liveuser >/dev/null 2>&1 || true
fi
chroot "$ROOTFS_DIR" /usr/sbin/usermod -s /bin/bash liveuser >/dev/null 2>&1 || true
chroot "$ROOTFS_DIR" /usr/bin/passwd -d liveuser >/dev/null 2>&1 || true

# 3d. Hardware volume key handling (VolumeUp/Down/Mute) via actkbd.
cat > "$ROOTFS_DIR/usr/local/bin/kiosk-volume-key" << 'VOLKEYEOF'
#!/bin/bash
set -euo pipefail

ACTION="${1:-}"
USER_NAME="liveuser"
USER_UID="$(id -u "$USER_NAME" 2>/dev/null || echo 1000)"
RUNTIME_DIR="/run/user/${USER_UID}"
BUS_ADDR="unix:path=${RUNTIME_DIR}/bus"

run_user_cmd() {
  if command -v runuser >/dev/null 2>&1; then
    runuser -u "$USER_NAME" -- "$@"
  else
    "$@"
  fi
}

with_user_audio_env() {
  run_user_cmd env \
    XDG_RUNTIME_DIR="$RUNTIME_DIR" \
    DBUS_SESSION_BUS_ADDRESS="$BUS_ADDR" \
    "$@"
}

case "$ACTION" in
  up)
    if command -v wpctl >/dev/null 2>&1; then
      with_user_audio_env wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+ || true
    elif command -v pactl >/dev/null 2>&1; then
      with_user_audio_env pactl set-sink-volume @DEFAULT_SINK@ +5% || true
    elif command -v amixer >/dev/null 2>&1; then
      amixer -q set Master 5%+ unmute || true
    fi
    ;;
  down)
    if command -v wpctl >/dev/null 2>&1; then
      with_user_audio_env wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%- || true
    elif command -v pactl >/dev/null 2>&1; then
      with_user_audio_env pactl set-sink-volume @DEFAULT_SINK@ -5% || true
    elif command -v amixer >/dev/null 2>&1; then
      amixer -q set Master 5%- unmute || true
    fi
    ;;
  mute)
    if command -v wpctl >/dev/null 2>&1; then
      with_user_audio_env wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle || true
    elif command -v pactl >/dev/null 2>&1; then
      with_user_audio_env pactl set-sink-mute @DEFAULT_SINK@ toggle || true
    elif command -v amixer >/dev/null 2>&1; then
      amixer -q set Master toggle || true
    fi
    ;;
  *)
    exit 0
    ;;
esac
VOLKEYEOF
chmod +x "$ROOTFS_DIR/usr/local/bin/kiosk-volume-key"

cat > "$ROOTFS_DIR/etc/actkbd.conf" << 'ACTKBDEOF'
# Format: <keycode>:<event_type>:<attributes>:<command>
# Fedora/Kernel media key codes:
# 113 = KEY_MUTE, 114 = KEY_VOLUMEDOWN, 115 = KEY_VOLUMEUP
113:key:exec:/usr/local/bin/kiosk-volume-key mute
114:key:exec:/usr/local/bin/kiosk-volume-key down
115:key:exec:/usr/local/bin/kiosk-volume-key up
ACTKBDEOF

# Create actkbd systemd service if the package didn't ship one.
if [ ! -f "$ROOTFS_DIR/usr/lib/systemd/system/actkbd.service" ] && [ ! -f "$ROOTFS_DIR/etc/systemd/system/actkbd.service" ]; then
  ACTKBD_BIN=""
  [ -x "$ROOTFS_DIR/usr/bin/actkbd" ] && ACTKBD_BIN="/usr/bin/actkbd"
  [ -x "$ROOTFS_DIR/usr/sbin/actkbd" ] && ACTKBD_BIN="/usr/sbin/actkbd"
  if [ -n "$ACTKBD_BIN" ]; then
    cat > "$ROOTFS_DIR/etc/systemd/system/actkbd.service" << SVCEOF
[Unit]
Description=actkbd - keyboard shortcut daemon
After=systemd-udevd.service

[Service]
Type=forking
ExecStart=${ACTKBD_BIN} -D -q -c /etc/actkbd.conf
Restart=on-failure

[Install]
WantedBy=multi-user.target
SVCEOF
  fi
fi

if [ -f "$ROOTFS_DIR/usr/lib/systemd/system/actkbd.service" ] || [ -f "$ROOTFS_DIR/etc/systemd/system/actkbd.service" ]; then
  chroot "$ROOTFS_DIR" /usr/bin/systemctl enable actkbd.service >/dev/null 2>&1 || true
  echo -e "  ${GREEN}Volume keys mapped (actkbd)${NC}"
else
  echo -e "  ${YELLOW}actkbd binary not found; volume key mapping skipped${NC}"
fi

mkdir -p "$ROOTFS_DIR/home/liveuser"
cat > "$ROOTFS_DIR/home/liveuser/.bash_profile" << 'BPROFEOF'
#!/bin/bash
TTY="$(tty 2>/dev/null || true)"
if [ -z "${DISPLAY:-}" ] && { [ "${XDG_VTNR:-}" = "1" ] || [ "$TTY" = "/dev/tty1" ]; }; then
  [ -n "$TTY" ] && printf '\033[2J\033[3J\033[H\033[?25l' >"$TTY" 2>/dev/null || true
  exec /usr/local/bin/kiosk-session.sh </dev/null >/dev/null 2>&1
fi
BPROFEOF
chmod 644 "$ROOTFS_DIR/home/liveuser/.bash_profile"
chown liveuser:liveuser "$ROOTFS_DIR/home/liveuser/.bash_profile" 2>/dev/null || true
echo -e "  ${GREEN}GDM masked, tty1 autologin configured${NC}"

# 3d. Slim livesys-gnome — no GDM config, just cleanup + schema compile
cat > "$ROOTFS_DIR/usr/libexec/livesys/sessions.d/livesys-gnome" << 'LSEOF'
#!/usr/bin/sh
# livesys-gnome: FedAC Kiosk — minimal live session cleanup

# Suppress GNOME initial setup / welcome screens
mkdir -p ~liveuser/.config
echo yes > ~liveuser/.config/gnome-initial-setup-done
rm -f /etc/xdg/autostart/gnome-initial-setup*.desktop
rm -f /etc/xdg/autostart/org.gnome.Tour.desktop
rm -f ~liveuser/.config/autostart/org.fedoraproject.welcome-screen.desktop
rm -f ~liveuser/.config/autostart/fedora-welcome.desktop

# Compile glib schemas (mutter reads these at startup)
glib-compile-schemas /usr/share/glib-2.0/schemas 2>/dev/null || true

chown -R liveuser:liveuser ~liveuser/ 2>/dev/null || true
LSEOF
chmod +x "$ROOTFS_DIR/usr/libexec/livesys/sessions.d/livesys-gnome"
echo -e "  ${GREEN}livesys-gnome trimmed (no GDM)${NC}"

# 3e. Disable unnecessary services for faster boot
for svc in \
  abrtd abrt-journal-core abrt-oops abrt-vmcore abrt-xorg \
  avahi-daemon cups cups-browsed \
  flatpak-system-update \
  ModemManager \
  sssd sssd-kcm \
  thermald tuned \
  rsyslog \
  mdmonitor mcelog smartd atd \
  NetworkManager-wait-online; do
  ln -sf /dev/null "$ROOTFS_DIR/etc/systemd/system/${svc}.service" 2>/dev/null || true
done
echo -e "  ${GREEN}Unnecessary services masked${NC}"

# 3f. Firefox policies (no default tabs, no welcome)
mkdir -p "$ROOTFS_DIR/usr/lib64/firefox/distribution"
cat > "$ROOTFS_DIR/usr/lib64/firefox/distribution/policies.json" << 'FPEOF'
{
  "policies": {
    "OverrideFirstRunPage": "",
    "OverridePostUpdatePage": "",
    "Homepage": {
      "URL": "__KIOSK_PIECE_URL__",
      "StartPage": "homepage"
    },
    "NewTabPage": false,
    "DisableProfileImport": true,
    "DontCheckDefaultBrowser": true,
    "NoDefaultBookmarks": true,
    "DisableTelemetry": true,
    "DisableFirefoxStudies": true,
    "DisablePocket": true,
    "DisplayBookmarksToolbar": "never",
    "DisplayMenuBar": "never",
    "SearchBar": "none"
  }
}
FPEOF
sed -i "s|__KIOSK_PIECE_URL__|$KIOSK_PIECE_URL|g" "$ROOTFS_DIR/usr/lib64/firefox/distribution/policies.json"

cat > "$ROOTFS_DIR/usr/lib64/firefox/defaults/pref/autoconfig.js" << 'ACEOF'
pref("general.config.filename", "firefox.cfg");
pref("general.config.obscure_value", 0);
ACEOF

cat > "$ROOTFS_DIR/usr/lib64/firefox/firefox.cfg" << 'CFGEOF'
// FedAC Kiosk Firefox config
defaultPref("browser.sessionstore.resume_from_crash", false);
defaultPref("browser.startup.homepage_override.mstone", "ignore");
defaultPref("browser.startup.page", 1);
defaultPref("browser.startup.homepage", "__KIOSK_PIECE_URL__");
defaultPref("browser.startup.blankWindow", false);
defaultPref("browser.display.background_color", "#000000");
defaultPref("browser.tabs.warnOnClose", false);
defaultPref("browser.tabs.drawInTitlebar", false);
defaultPref("browser.tabs.inTitlebar", 0);
defaultPref("browser.shell.checkDefaultBrowser", false);
defaultPref("datareporting.policy.dataSubmissionEnabled", false);
defaultPref("toolkit.telemetry.reportingpolicy.firstRun", false);
defaultPref("browser.newtabpage.enabled", false);
defaultPref("browser.aboutwelcome.enabled", false);
defaultPref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
// Allow Web Audio API autoplay (kiosk — no user gesture needed)
defaultPref("media.autoplay.default", 0);
defaultPref("media.autoplay.blocking_policy", 0);
defaultPref("media.autoplay.allow-extension-background-pages", true);
CFGEOF
sed -i "s|__KIOSK_PIECE_URL__|$KIOSK_PIECE_URL|g" "$ROOTFS_DIR/usr/lib64/firefox/firefox.cfg"
echo -e "  ${GREEN}Firefox policies installed${NC}"

# 3g. Plymouth PALS logo
PALS_LOGO="$REPO_ROOT/fedac/plymouth/pals.png"
if [ -f "$PALS_LOGO" ]; then
  cp "$PALS_LOGO" "$ROOTFS_DIR/usr/share/plymouth/themes/spinner/watermark.png"
  # Center the logo and set black background
  sed -i 's/^WatermarkVerticalAlignment=.*/WatermarkVerticalAlignment=.5/' \
    "$ROOTFS_DIR/usr/share/plymouth/themes/spinner/spinner.plymouth"
  cat > "$ROOTFS_DIR/etc/plymouth/plymouthd.conf" << 'PLYEOF'
[Daemon]
Theme=spinner
ShowDelay=0
PLYEOF
  echo -e "  ${GREEN}PALS Plymouth theme installed${NC}"
fi

# 3h. dconf kiosk settings
mkdir -p "$ROOTFS_DIR/etc/dconf/db/local.d" "$ROOTFS_DIR/etc/dconf/profile"
cp "$OVERLAY_DIR/00-kiosk" "$ROOTFS_DIR/etc/dconf/db/local.d/00-kiosk" 2>/dev/null || true
cp "$OVERLAY_DIR/dconf-profile-user" "$ROOTFS_DIR/etc/dconf/profile/user" 2>/dev/null || true

echo -e "  ${GREEN}All kiosk config injected${NC}"

# ══════════════════════════════════════════
# Step 4: Build EROFS + initrd
# ══════════════════════════════════════════
echo -e "${CYAN}[4/6] Building EROFS image...${NC}"

EROFS_PATH="$WORK_DIR/squashfs.img"
mkfs.erofs -zlzma -C65536 "$EROFS_PATH" "$ROOTFS_DIR/"
EROFS_SIZE=$(stat -c%s "$EROFS_PATH")
echo -e "  ${GREEN}EROFS built: $(numfmt --to=iec $EROFS_SIZE)${NC}"

# Build initrd with PALS Plymouth theme
echo -e "  Building initrd with PALS Plymouth..."
KVER=$(ls "$ROOTFS_DIR/lib/modules/" | head -1)

mount --bind /proc "$ROOTFS_DIR/proc"
mount --bind /sys "$ROOTFS_DIR/sys"
mount --bind /dev "$ROOTFS_DIR/dev"

chroot "$ROOTFS_DIR" dracut --force --no-hostonly \
  --add "dmsquash-live plymouth" \
  /tmp/live-initrd.img "$KVER" 2>/dev/null || true

cp "$ROOTFS_DIR/tmp/live-initrd.img" "$WORK_DIR/initrd"
cp "$ROOTFS_DIR/boot/vmlinuz-$KVER" "$WORK_DIR/linux"
rm -f "$ROOTFS_DIR/tmp/live-initrd.img"

umount "$ROOTFS_DIR/proc" "$ROOTFS_DIR/sys" "$ROOTFS_DIR/dev"
echo -e "  ${GREEN}Initrd built with PALS theme${NC}"

build_target() {
  local target="$1"
  local label="$2"

  # Unmount existing partitions
  for part in "${target}"*; do
    umount "$part" 2>/dev/null || true
  done

  # Wipe and partition: 400MB FAT32 EFI + rest ext4
  echo -e "  Creating partition table on ${label}..."
  wipefs -a "$target" >/dev/null 2>&1
  parted -s "$target" mklabel gpt
  parted -s "$target" mkpart '"EFI"' fat32 1MiB 401MiB
  parted -s "$target" set 1 esp on
  parted -s "$target" mkpart '"LIVE"' ext4 401MiB 100%

  sleep 2
  partprobe "$target" 2>/dev/null || true
  sleep 2

  # Detect partition names
  local p1="${target}1"
  local p2="${target}2"
  [ -b "$p1" ] || p1="${target}p1"
  [ -b "$p2" ] || p2="${target}p2"

  # Format
  mkfs.vfat -F 32 -n BOOT "$p1" >/dev/null
  mkfs.ext4 -L FEDAC-LIVE -q "$p2"
  echo -e "  ${GREEN}${label}: partitions created${NC}"

  # Flash EROFS
  LIVE_MOUNT=$(mktemp -d /tmp/fedac-live-XXXX)
  mount "$p2" "$LIVE_MOUNT"
  mkdir -p "$LIVE_MOUNT/LiveOS"
  cp "$EROFS_PATH" "$LIVE_MOUNT/LiveOS/squashfs.img"
  sync
  umount "$LIVE_MOUNT"
  rmdir "$LIVE_MOUNT"
  LIVE_MOUNT=""
  echo -e "  ${GREEN}${label}: EROFS written${NC}"

  # Set up EFI partition
  EFI_MOUNT=$(mktemp -d /tmp/fedac-efi-XXXX)
  mount "$p1" "$EFI_MOUNT"

  mkdir -p "$EFI_MOUNT/EFI/BOOT"
  if [ -f "$ROOTFS_DIR/boot/efi/EFI/fedora/grubx64.efi" ]; then
    cp "$ROOTFS_DIR/boot/efi/EFI/fedora/grubx64.efi" "$EFI_MOUNT/EFI/BOOT/BOOTX64.EFI"
  elif [ -f "$ROOTFS_DIR/usr/lib/grub/x86_64-efi/monolithic/grubx64.efi" ]; then
    cp "$ROOTFS_DIR/usr/lib/grub/x86_64-efi/monolithic/grubx64.efi" "$EFI_MOUNT/EFI/BOOT/BOOTX64.EFI"
  fi

  mkdir -p "$EFI_MOUNT/loader"
  cp "$WORK_DIR/linux" "$EFI_MOUNT/loader/linux"
  cp "$WORK_DIR/initrd" "$EFI_MOUNT/loader/initrd"

  cat > "$EFI_MOUNT/EFI/BOOT/grub.cfg" << 'GRUBEOF'
set default=0
set timeout=0
insmod all_video
insmod gzio
insmod part_gpt
insmod fat
set gfxmode=auto
set gfxpayload=keep
terminal_input console
terminal_output gfxterm
menuentry "FedAC Kiosk" --class fedora {
  linux /loader/linux quiet splash rhgb loglevel=0 systemd.log_level=emerg systemd.show_status=false rd.systemd.show_status=false rd.udev.log_level=3 udev.log_priority=3 rd.plymouth=1 plymouth.ignore-serial-consoles vt.handoff=7 vt.global_cursor_default=0 logo.nologo root=live:LABEL=FEDAC-LIVE rd.live.image mitigations=off
  initrd /loader/initrd
}
GRUBEOF

  mkdir -p "$EFI_MOUNT/EFI/fedora"
  cp "$EFI_MOUNT/EFI/BOOT/grub.cfg" "$EFI_MOUNT/EFI/fedora/grub.cfg"

  sync
  umount "$EFI_MOUNT"
  rmdir "$EFI_MOUNT"
  EFI_MOUNT=""
  echo -e "  ${GREEN}${label}: EFI configured${NC}"
}

# ══════════════════════════════════════════
# Step 5: Build image and/or flash USB
# ══════════════════════════════════════════
echo -e "${CYAN}[5/6] Building output media...${NC}"

if [ -n "$IMAGE_PATH" ]; then
  echo -e "  Building disk image: ${GREEN}${IMAGE_PATH}${NC} (${IMAGE_SIZE_GB}GiB)"
  truncate -s "${IMAGE_SIZE_GB}G" "$IMAGE_PATH"
  LOOP_DEV=$(losetup --find --show --partscan "$IMAGE_PATH")
  build_target "$LOOP_DEV" "Disk image"
  losetup -d "$LOOP_DEV"
  LOOP_DEV=""
  echo -e "  ${GREEN}Disk image ready${NC}"
fi

if [ -n "$DEVICE" ]; then
  echo ""
  echo -e "USB target: ${YELLOW}$DEVICE${NC}"
  lsblk "$DEVICE" 2>/dev/null || true
  echo ""

  if [ "$SKIP_CONFIRM" = false ]; then
    echo -e "${RED}ALL DATA ON $DEVICE WILL BE DESTROYED${NC}"
    read -p "Continue? [y/N] " confirm
    [ "$confirm" = "y" ] || [ "$confirm" = "Y" ] || { echo "Aborted."; exit 0; }
  fi

  if [ -n "$IMAGE_PATH" ]; then
    echo -e "  Flashing image to USB..."
    dd if="$IMAGE_PATH" of="$DEVICE" bs=4M status=progress conv=fsync
    sync
    echo -e "  ${GREEN}USB flashed from image${NC}"
  else
    build_target "$DEVICE" "USB"
  fi
fi

# ══════════════════════════════════════════
# Step 6: Finalize
# ══════════════════════════════════════════
echo -e "${CYAN}[6/6] Finalizing...${NC}"
sync

if [ "$DO_EJECT" = true ] && [ -n "$DEVICE" ]; then
  eject "$DEVICE" 2>/dev/null || true
  echo -e "  ${GREEN}Ejected${NC}"
fi

# Clean up work dir (but not external rootfs)
if [ "$OWN_ROOTFS" = true ]; then
  echo -e "  Cleaning work dir..."
  rm -rf "$WORK_DIR"
else
  rm -f "$WORK_DIR/piece.html" "$WORK_DIR/squashfs.img" "$WORK_DIR/initrd" "$WORK_DIR/linux"
  rmdir "$WORK_DIR" 2>/dev/null || true
fi

echo ""
echo -e "${GREEN}╔═══════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║       FedAC Kiosk Build Ready                 ║${NC}"
echo -e "${GREEN}║  Piece: ${CYAN}${PIECE_CODE}${GREEN} (offline)                     ║${NC}"
echo -e "${GREEN}╚═══════════════════════════════════════════════╝${NC}"
echo ""
if [ -n "$IMAGE_PATH" ]; then
  echo -e "Image:"
  echo -e "  ${CYAN}${IMAGE_PATH}${NC}"
fi
if [ -n "$DEVICE" ]; then
  echo -e "USB:"
  echo -e "  ${CYAN}${DEVICE}${NC}"
fi
echo ""
echo -e "Boot flow:"
echo -e "  Power on → GRUB (instant) → ${PINK}PALS splash${NC}"
echo -e "  → ${PINK}Firefox fullscreen → ${PIECE_CODE}${NC} (offline)"
echo ""
echo "Plug into target machine → boot from USB → done."
