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
#   --base-image      Build base image without a specific piece (placeholder only)
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
BASE_IMAGE_MODE=false
PACK_DENSITY="$PACK_DENSITY_DEFAULT"

while [ $# -gt 0 ]; do
  case "$1" in
    --rootfs) ROOTFS_DIR="$2"; shift 2 ;;
    --iso) ISO_PATH="$2"; shift 2 ;;
    --image) IMAGE_PATH="$2"; shift 2 ;;
    --image-size) IMAGE_SIZE_GB="$2"; shift 2 ;;
    --density) PACK_DENSITY="$2"; shift 2 ;;
    --work-base) WORK_BASE="$2"; shift 2 ;;
    --base-image) BASE_IMAGE_MODE=true; shift ;;
    --no-eject) DO_EJECT=false; shift ;;
    --yes) SKIP_CONFIRM=true; shift ;;
    --help|-h) usage ;;
    /dev/*) DEVICE="$1"; shift ;;
    *) if [ -z "$PIECE_CODE" ]; then PIECE_CODE="$1"; shift; else echo -e "${RED}Unknown arg: $1${NC}"; usage; fi ;;
  esac
done

if [ "$BASE_IMAGE_MODE" = true ]; then
  [ -n "$PIECE_CODE" ] || PIECE_CODE="__base__"
else
  [ -n "$PIECE_CODE" ] || { echo -e "${RED}Error: No piece code specified${NC}"; usage; }
fi
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
BUNDLE_PATH="$WORK_DIR/piece.html"

if [ "$BASE_IMAGE_MODE" = true ]; then
  echo -e "${CYAN}[1/6] Base image mode — generating placeholder piece...${NC}"
  cat > "$BUNDLE_PATH" << 'PLACEHOLDER_EOF'
<!DOCTYPE html><html><head><meta charset="utf-8"><title>FedAC Base</title></head>
<body style="background:#000;color:#fff;display:flex;align-items:center;justify-content:center;height:100vh;margin:0;font-family:monospace">
<p>No piece loaded. Build with aesthetic.computer/os</p>
</body></html>
PLACEHOLDER_EOF
  echo -e "  ${GREEN}Placeholder piece.html generated${NC}"
else
  echo -e "${CYAN}[1/6] Fetching piece bundle: ${PIECE_CODE}...${NC}"

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
fi

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

  # Mount LiveOS image when possible; otherwise extract directly.
  ROOTFS_MOUNT=$(mktemp -d /tmp/fedac-rootfs-mount-XXXX)
  mkdir -p "$ROOTFS_DIR"
  if mount -o loop,ro "$SQUASH_IMG" "$ROOTFS_MOUNT" 2>/tmp/fedac-rootfs-mount.err; then
    # Copy rootfs to work dir
    echo -e "  Copying rootfs (this takes a while)..."
    cp -a "$ROOTFS_MOUNT"/* "$ROOTFS_DIR"/

    umount "$ROOTFS_MOUNT"
    rmdir "$ROOTFS_MOUNT"
    ROOTFS_MOUNT=""
  else
    MOUNT_ERR="$(cat /tmp/fedac-rootfs-mount.err 2>/dev/null || true)"
    rm -f /tmp/fedac-rootfs-mount.err
    echo -e "  ${YELLOW}Mounting LiveOS image failed (${MOUNT_ERR})${NC}"
    echo -e "  ${CYAN}Falling back to direct extraction...${NC}"
    rmdir "$ROOTFS_MOUNT" 2>/dev/null || true
    ROOTFS_MOUNT=""

    IMG_INFO="$(file -b "$SQUASH_IMG" 2>/dev/null || true)"
    if echo "$IMG_INFO" | grep -qi "erofs"; then
      command -v fsck.erofs >/dev/null 2>&1 || {
        echo -e "${RED}Error: fsck.erofs not found for EROFS extraction${NC}"
        exit 1
      }
      fsck.erofs --extract="$ROOTFS_DIR" --force --overwrite "$SQUASH_IMG" >/tmp/fedac-erofs-extract.log 2>&1
      tail -n 20 /tmp/fedac-erofs-extract.log || true
      rm -f /tmp/fedac-erofs-extract.log
    else
      command -v unsquashfs >/dev/null 2>&1 || {
        echo -e "${RED}Error: unsquashfs not found for squashfs extraction${NC}"
        exit 1
      }
      unsquashfs -f -d "$ROOTFS_DIR" "$SQUASH_IMG"
    fi
  fi

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
# The oven runs Ubuntu (no host dnf), so use chroot + the rootfs's own dnf5.
if [ ! -x "$ROOTFS_DIR/usr/bin/cage" ]; then
  echo -e "  Installing cage into rootfs..."
  if command -v dnf >/dev/null 2>&1; then
    # Host has dnf (Fedora build machine)
    dnf -y --installroot="$ROOTFS_DIR" --releasever="$FEDORA_VERSION" install cage >/dev/null 2>&1 || true
  elif [ -x "$ROOTFS_DIR/usr/bin/dnf" ]; then
    # Chroot into the Fedora rootfs and use its own package manager
    mount --bind /dev "$ROOTFS_DIR/dev" 2>/dev/null || true
    mount --bind /proc "$ROOTFS_DIR/proc" 2>/dev/null || true
    mount --bind /sys "$ROOTFS_DIR/sys" 2>/dev/null || true
    mount -t tmpfs tmpfs "$ROOTFS_DIR/tmp" 2>/dev/null || true
    cp /etc/resolv.conf "$ROOTFS_DIR/etc/resolv.conf" 2>/dev/null || true
    chroot "$ROOTFS_DIR" /usr/bin/dnf -y install cage 2>&1 | tail -5 || true
    umount "$ROOTFS_DIR/tmp" 2>/dev/null || true
    umount "$ROOTFS_DIR/sys" 2>/dev/null || true
    umount "$ROOTFS_DIR/proc" 2>/dev/null || true
    umount "$ROOTFS_DIR/dev" 2>/dev/null || true
  fi
  if [ -x "$ROOTFS_DIR/usr/bin/cage" ]; then
    echo -e "  ${GREEN}cage installed${NC}"
  else
    echo -e "  ${YELLOW}cage not available — will fall back to mutter${NC}"
  fi
fi

# Volume key daemon is pure Python — no external packages needed.

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
user_pref("accessibility.typeaheadfind", false);
user_pref("accessibility.typeaheadfind.manual", false);
user_pref("accessibility.typeaheadfind.linksonly", false);
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

# Check for piece on FEDAC-PIECE partition (overlay for base images)
PIECE_PART=$(blkid -L FEDAC-PIECE 2>/dev/null || true)
if [ -n "$PIECE_PART" ]; then
  mkdir -p /mnt/piece
  mount -o ro "$PIECE_PART" /mnt/piece 2>/dev/null || true
  if [ -f /mnt/piece/piece.html ]; then
    ln -sf /mnt/piece/piece.html /usr/local/share/kiosk/piece.html
  fi
fi

# Paint PALS logo to framebuffer (visible during PipeWire/cage startup)
if [ -x /usr/local/bin/pals-fb-splash ]; then
  /usr/local/bin/pals-fb-splash 2>/dev/null || true
fi

# Start PipeWire audio stack (cage doesn't launch a full desktop session).
# PipeWire needs XDG_RUNTIME_DIR which we set above.
mkdir -p "$XDG_RUNTIME_DIR"
if command -v pipewire >/dev/null 2>&1; then
  pipewire &
  sleep 0.1
  command -v wireplumber >/dev/null 2>&1 && wireplumber &
  command -v pipewire-pulse >/dev/null 2>&1 && pipewire-pulse &
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

# Disable SELinux — the erofs live rootfs lacks correct xattr labels, causing
# pam_unix to fail with "User not known to the underlying authentication module".
# For an offline kiosk, SELinux provides no benefit and adds boot complexity.
if [ -f "$ROOTFS_DIR/etc/selinux/config" ]; then
  sed -i 's/^SELINUX=enforcing/SELINUX=disabled/' "$ROOTFS_DIR/etc/selinux/config"
  echo -e "  ${GREEN}SELinux disabled in rootfs config${NC}"
fi

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

# Ensure PAM allows passwordless autologin for liveuser.
# Fedora 43 pam_unix.so rejects empty-password accounts unless "nullok" is present.
# Use authselect (Fedora's PAM manager) so the change survives boot-time regeneration,
# then also patch the files directly as a belt-and-suspenders fallback.
if chroot "$ROOTFS_DIR" /usr/bin/authselect current >/dev/null 2>&1; then
  # Get current profile and add with-nullok feature
  CURRENT_PROFILE=$(chroot "$ROOTFS_DIR" /usr/bin/authselect current -r 2>/dev/null || echo "sssd")
  chroot "$ROOTFS_DIR" /usr/bin/authselect select "$CURRENT_PROFILE" with-nullok --force >/dev/null 2>&1 || true
  echo -e "  ${GREEN}authselect: enabled with-nullok on profile $CURRENT_PROFILE${NC}"
fi
# Belt-and-suspenders: also patch the files directly in case authselect isn't available.
for pamfile in "$ROOTFS_DIR/etc/pam.d/system-auth" "$ROOTFS_DIR/etc/pam.d/password-auth"; do
  if [ -f "$pamfile" ]; then
    sed -i '/pam_unix\.so/ { /nullok/! s/pam_unix\.so/pam_unix.so nullok/ }' "$pamfile"
  fi
done
echo -e "  ${GREEN}PAM nullok configured for passwordless autologin${NC}"

# 3d. Hardware volume key daemon — pure Python, reads raw Linux input events.
# No actkbd or external packages needed. Runs as root, talks to liveuser's PipeWire.
cat > "$ROOTFS_DIR/usr/local/bin/kiosk-volume-keyd" << 'VKDEOF'
#!/usr/bin/python3
"""FedAC kiosk volume key daemon.

Reads raw input_event structs from all keyboard /dev/input/event* devices,
intercepts KEY_MUTE (113), KEY_VOLUMEDOWN (114), KEY_VOLUMEUP (115),
and adjusts the PipeWire/PulseAudio/ALSA volume for liveuser.

Zero external dependencies — stdlib only.
"""
import glob, os, select, struct, subprocess, sys

# Linux input_event: struct timeval (16 bytes on 64-bit), __u16 type, __u16 code, __s32 value
EVENT_SIZE = struct.calcsize("llHHi")
EV_KEY = 0x01
KEY_DOWN = 1  # value=1 is key press

KEY_MUTE = 113
KEY_VOLUMEDOWN = 114
KEY_VOLUMEUP = 115

USER = "liveuser"
UID = None
RUNTIME_DIR = None

def find_uid():
    global UID, RUNTIME_DIR
    try:
        import pwd
        UID = pwd.getpwnam(USER).pw_uid
    except (KeyError, ImportError):
        UID = 1000
    RUNTIME_DIR = f"/run/user/{UID}"

def run_as_user(cmd):
    """Run a command as liveuser with their audio env."""
    env = os.environ.copy()
    env["XDG_RUNTIME_DIR"] = RUNTIME_DIR
    env["DBUS_SESSION_BUS_ADDRESS"] = f"unix:path={RUNTIME_DIR}/bus"
    try:
        subprocess.run(
            ["runuser", "-u", USER, "--"] + cmd,
            env=env, timeout=5,
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
        )
    except Exception:
        pass

def vol_up():
    run_as_user(["wpctl", "set-volume", "-l", "1.5", "@DEFAULT_AUDIO_SINK@", "5%+"])

def vol_down():
    run_as_user(["wpctl", "set-volume", "@DEFAULT_AUDIO_SINK@", "5%-"])

def vol_mute():
    run_as_user(["wpctl", "set-mute", "@DEFAULT_AUDIO_SINK@", "toggle"])

ACTIONS = {KEY_MUTE: vol_mute, KEY_VOLUMEDOWN: vol_down, KEY_VOLUMEUP: vol_up}

def is_keyboard(evdev_path):
    """Check if device has EV_KEY capability (bit 1 in capabilities/ev)."""
    sysfs = f"/sys/class/input/{os.path.basename(evdev_path)}/device/capabilities/ev"
    try:
        caps = int(open(sysfs).read().strip(), 16)
        return bool(caps & (1 << EV_KEY))
    except Exception:
        return False

def main():
    find_uid()
    # Open all keyboard-capable input devices
    fds = {}
    for path in sorted(glob.glob("/dev/input/event*")):
        if is_keyboard(path):
            try:
                fd = os.open(path, os.O_RDONLY | os.O_NONBLOCK)
                fds[fd] = path
            except OSError:
                pass

    if not fds:
        print("kiosk-volume-keyd: no keyboard devices found", file=sys.stderr)
        sys.exit(1)

    print(f"kiosk-volume-keyd: monitoring {len(fds)} device(s)", file=sys.stderr)

    poll = select.poll()
    for fd in fds:
        poll.register(fd, select.POLLIN)

    while True:
        try:
            events = poll.poll(5000)
        except (KeyboardInterrupt, SystemExit):
            break
        for fd, mask in events:
            if mask & select.POLLIN:
                try:
                    data = os.read(fd, EVENT_SIZE * 16)
                except OSError:
                    continue
                for off in range(0, len(data) - EVENT_SIZE + 1, EVENT_SIZE):
                    _sec, _usec, ev_type, code, value = struct.unpack_from("llHHi", data, off)
                    if ev_type == EV_KEY and value == KEY_DOWN and code in ACTIONS:
                        ACTIONS[code]()

    for fd in fds:
        os.close(fd)

if __name__ == "__main__":
    main()
VKDEOF
chmod +x "$ROOTFS_DIR/usr/local/bin/kiosk-volume-keyd"

# Systemd service for the volume key daemon
cat > "$ROOTFS_DIR/etc/systemd/system/kiosk-volume-keyd.service" << 'SVCEOF'
[Unit]
Description=FedAC kiosk volume key daemon
After=systemd-udevd.service

[Service]
Type=simple
ExecStart=/usr/local/bin/kiosk-volume-keyd
Restart=on-failure
RestartSec=2

[Install]
WantedBy=multi-user.target
SVCEOF
chroot "$ROOTFS_DIR" /usr/bin/systemctl enable kiosk-volume-keyd.service >/dev/null 2>&1 || true
echo -e "  ${GREEN}Volume keys mapped (kiosk-volume-keyd)${NC}"

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
defaultPref("accessibility.typeaheadfind", false);
defaultPref("accessibility.typeaheadfind.manual", false);
defaultPref("accessibility.typeaheadfind.linksonly", false);
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
# Step 3i: Strip bloat from rootfs (kiosk only needs cage + Firefox + audio)
# ══════════════════════════════════════════
echo -e "${CYAN}  Stripping unnecessary packages from rootfs...${NC}"
ROOTFS_BEFORE=$(du -sb "$ROOTFS_DIR" 2>/dev/null | awk '{print $1}')

# Mount /dev, /proc, /sys for chroot dnf operations
mount --bind /dev "$ROOTFS_DIR/dev" 2>/dev/null || true
mount --bind /proc "$ROOTFS_DIR/proc" 2>/dev/null || true
mount --bind /sys "$ROOTFS_DIR/sys" 2>/dev/null || true

# Remove large unnecessary packages via dnf (resolves dependencies cleanly)
STRIP_PKGS=(
  # Office suite (~300MB)
  "libreoffice*"
  # Java (~248MB)
  "java-*-openjdk*"
  # Virtualization (~110MB)
  "qemu*" "edk2-ovmf" "gnome-boxes" "libvirt*" "podman*"
  # LLVM (~145MB)
  "llvm-libs"
  # GNOME apps we don't need (~150MB+)
  "gnome-software*" "gnome-maps" "gnome-weather" "gnome-contacts"
  "gnome-calendar" "gnome-clocks" "gnome-connections" "gnome-characters"
  "gnome-font-viewer" "gnome-logs" "gnome-calculator" "gnome-text-editor"
  "gnome-tour" "gnome-photos" "gnome-music" "gnome-user-docs"
  "simple-scan" "baobab" "totem*" "cheese" "rhythmbox" "shotwell"
  "evince*" "yelp*" "loupe" "snapshot" "gnome-console"
  # Crash reporter (~30MB)
  "abrt*" "gnome-abrt"
  # IBus input methods (~156MB) — kiosk doesn't need multilingual input
  "ibus*"
  # WebKit (not needed, Firefox is our browser) (~180MB)
  "webkit2gtk*" "webkitgtk*" "javascriptcoregtk*"
  # Container tools
  "buildah" "skopeo" "toolbox" "containers-common*"
  # Evolution data server (email/calendar backend)
  "evolution*"
  # Flatpak
  "flatpak*"
  # Printing
  "cups*"
  # Misc heavy packages
  "unicode-ucd" "cldr-emoji-annotation"
  "anaconda*" "livecd-tools" "liveinst*" "mediawriter"
)
chroot "$ROOTFS_DIR" /usr/bin/dnf -y remove "${STRIP_PKGS[@]}" --setopt=protected_packages= 2>&1 | tail -3 || true

# Remove non-English locales (~220MB)
echo -e "  Stripping non-English locales..."
find "$ROOTFS_DIR/usr/share/locale" -mindepth 1 -maxdepth 1 -type d \
  ! -name "en" ! -name "en_US" ! -name "en_GB" -exec rm -rf {} + 2>/dev/null || true

# Remove docs, man pages, help (~250MB)
rm -rf "$ROOTFS_DIR/usr/share/doc"
rm -rf "$ROOTFS_DIR/usr/share/man"
rm -rf "$ROOTFS_DIR/usr/share/help"
rm -rf "$ROOTFS_DIR/usr/share/gtk-doc"
rm -rf "$ROOTFS_DIR/usr/share/info"

# Remove Firefox langpacks (~43MB) — kiosk is English-only
rm -rf "$ROOTFS_DIR/usr/lib64/firefox/langpacks" 2>/dev/null || true
chroot "$ROOTFS_DIR" /usr/bin/dnf -y remove "firefox-langpacks" 2>&1 | tail -1 || true

# Remove all langpacks except English (~238MB)
chroot "$ROOTFS_DIR" /usr/bin/dnf -y remove "glibc-all-langpacks" 2>&1 | tail -1 || true
chroot "$ROOTFS_DIR" /usr/bin/dnf -y install "glibc-langpack-en" 2>&1 | tail -1 || true

# Remove CJK fonts (~57MB) — not needed for kiosk
rm -rf "$ROOTFS_DIR/usr/share/fonts/google-noto-serif-cjk-vf-fonts" 2>/dev/null || true
rm -rf "$ROOTFS_DIR/usr/share/fonts/google-noto-sans-cjk-vf-fonts" 2>/dev/null || true

# Trim firmware — keep only common wifi + GPU, remove exotic/unused (~300MB savings)
FIRMWARE_KEEP="intel|iwlwifi|amd|nvidia|ath|rtl|brcm|realtek|i915|radeon|amdgpu|mediatek"
for fw_dir in "$ROOTFS_DIR/usr/lib/firmware"/*/; do
  fw_name=$(basename "$fw_dir")
  if ! echo "$fw_name" | grep -qiE "$FIRMWARE_KEEP"; then
    rm -rf "$fw_dir" 2>/dev/null || true
  fi
done
# Remove large individual firmware blobs we don't need
rm -rf "$ROOTFS_DIR/usr/lib/firmware/liquidio" 2>/dev/null || true
rm -rf "$ROOTFS_DIR/usr/lib/firmware/netronome" 2>/dev/null || true
rm -rf "$ROOTFS_DIR/usr/lib/firmware/mellanox" 2>/dev/null || true
rm -rf "$ROOTFS_DIR/usr/lib/firmware/dpaa2" 2>/dev/null || true
rm -rf "$ROOTFS_DIR/usr/lib/firmware/qcom" 2>/dev/null || true
rm -rf "$ROOTFS_DIR/usr/lib/firmware/cnm" 2>/dev/null || true
rm -rf "$ROOTFS_DIR/usr/lib/firmware/qed" 2>/dev/null || true
rm -rf "$ROOTFS_DIR/usr/lib/firmware/mrvl" 2>/dev/null || true
rm -rf "$ROOTFS_DIR/usr/lib/firmware/cxgb4" 2>/dev/null || true

# Clean dnf cache inside rootfs
chroot "$ROOTFS_DIR" /usr/bin/dnf clean all 2>/dev/null || true
rm -rf "$ROOTFS_DIR/var/cache/dnf" 2>/dev/null || true
rm -rf "$ROOTFS_DIR/var/log/"* 2>/dev/null || true

# Unmount chroot binds
umount "$ROOTFS_DIR/sys" 2>/dev/null || true
umount "$ROOTFS_DIR/proc" 2>/dev/null || true
umount "$ROOTFS_DIR/dev" 2>/dev/null || true

ROOTFS_AFTER=$(du -sb "$ROOTFS_DIR" 2>/dev/null | awk '{print $1}')
SAVED=$(( (ROOTFS_BEFORE - ROOTFS_AFTER) / 1048576 ))
echo -e "  ${GREEN}Stripped ${SAVED}MB from rootfs ($(numfmt --to=iec $ROOTFS_BEFORE) → $(numfmt --to=iec $ROOTFS_AFTER))${NC}"

# ══════════════════════════════════════════
# Step 4: Build EROFS + initrd
# ══════════════════════════════════════════
echo -e "${CYAN}[4/6] Building EROFS image...${NC}"

EROFS_PATH="$WORK_DIR/squashfs.img"
mkfs.erofs -zzstd,level=19 -C65536 "$EROFS_PATH" "$ROOTFS_DIR/"
EROFS_SIZE=$(stat -c%s "$EROFS_PATH")
echo -e "  ${GREEN}EROFS built: $(numfmt --to=iec $EROFS_SIZE)${NC}"

# ── Framebuffer splash: paint PALS logo to /dev/fb0 ──────────────────
# Pre-convert PALS PNG → raw BGRA for fast framebuffer blitting.
# This is used both in the initrd (pre-Plymouth) and in kiosk-session.sh.
PALS_FB_DIR="$ROOTFS_DIR/usr/local/share/pals-fb"
mkdir -p "$PALS_FB_DIR"

if [ -f "$REPO_ROOT/fedac/plymouth/pals.png" ]; then
  python3 - "$REPO_ROOT/fedac/plymouth/pals.png" "$PALS_FB_DIR/logo.bgra" << 'PYEOF'
import struct, sys, zlib

def read_png(path):
    """Minimal PNG reader → RGBA pixels. No PIL needed."""
    with open(path, 'rb') as f:
        sig = f.read(8)
        assert sig == b'\x89PNG\r\n\x1a\n', "Not a PNG"
        chunks = {}
        idat = b''
        while True:
            hdr = f.read(8)
            if len(hdr) < 8: break
            length = struct.unpack('>I', hdr[:4])[0]
            ctype = hdr[4:8]
            data = f.read(length)
            f.read(4)  # crc
            if ctype == b'IHDR':
                w, h, bd, ct = struct.unpack('>IIBB', data[:10])
                chunks['IHDR'] = (w, h, bd, ct)
            elif ctype == b'IDAT':
                idat += data
            elif ctype == b'IEND':
                break
    w, h, bd, ct = chunks['IHDR']
    raw = zlib.decompress(idat)
    # ct=6 is RGBA, ct=2 is RGB
    bpp = 4 if ct == 6 else 3
    stride = 1 + w * bpp  # filter byte + pixel data
    pixels = bytearray(w * h * 4)
    prev_row = bytearray(w * bpp)
    for y in range(h):
        off = y * stride
        filt = raw[off]
        row = bytearray(raw[off+1:off+1+w*bpp])
        if filt == 1:  # Sub
            for i in range(bpp, len(row)): row[i] = (row[i] + row[i-bpp]) & 0xFF
        elif filt == 2:  # Up
            for i in range(len(row)): row[i] = (row[i] + prev_row[i]) & 0xFF
        elif filt == 3:  # Average
            for i in range(len(row)):
                a = row[i-bpp] if i >= bpp else 0
                row[i] = (row[i] + (a + prev_row[i]) // 2) & 0xFF
        elif filt == 4:  # Paeth
            for i in range(len(row)):
                a = row[i-bpp] if i >= bpp else 0
                b = prev_row[i]
                c = prev_row[i-bpp] if i >= bpp else 0
                p = a + b - c
                pa, pb, pc = abs(p-a), abs(p-b), abs(p-c)
                pr = a if pa <= pb and pa <= pc else (b if pb <= pc else c)
                row[i] = (row[i] + pr) & 0xFF
        for x in range(w):
            si = x * bpp
            r, g, b_val = row[si], row[si+1], row[si+2]
            a = row[si+3] if bpp == 4 else 255
            di = (y * w + x) * 4
            pixels[di:di+4] = bytes([r, g, b_val, a])
        prev_row = row
    return w, h, bytes(pixels)

src, dst = sys.argv[1], sys.argv[2]
w, h, rgba = read_png(src)
# Convert RGBA → BGRA for Linux framebuffer
bgra = bytearray(len(rgba))
for i in range(0, len(rgba), 4):
    bgra[i]   = rgba[i+2]  # B
    bgra[i+1] = rgba[i+1]  # G
    bgra[i+2] = rgba[i]    # R
    bgra[i+3] = rgba[i+3]  # A
with open(dst, 'wb') as f:
    f.write(struct.pack('<II', w, h))  # 8-byte header: width, height (LE)
    f.write(bgra)
print(f"Converted {w}x{h} PNG → BGRA ({len(bgra)+8} bytes)")
PYEOF
fi

# Framebuffer splash script (used in initrd hook AND kiosk-session.sh)
cat > "$ROOTFS_DIR/usr/local/bin/pals-fb-splash" << 'FBEOF'
#!/usr/bin/python3
"""Paint PALS logo centered on dark purple to /dev/fb0."""
import os, struct, sys

LOGO_PATH = "/usr/local/share/pals-fb/logo.bgra"
BG_COLOR = (0x2e, 0x0a, 0x1a, 0xFF)  # BGRA for #1a0a2e

def main():
    if not os.path.exists("/dev/fb0"):
        return
    try:
        sz = open("/sys/class/graphics/fb0/virtual_size").read().strip().split(",")
        sw, sh = int(sz[0]), int(sz[1])
    except Exception:
        return
    bpp_bits = 32
    try:
        bpp_bits = int(open("/sys/class/graphics/fb0/bits_per_pixel").read().strip())
    except Exception:
        pass
    if bpp_bits != 32:
        return
    stride = sw * 4

    # Fill screen with dark purple
    row = bytes(BG_COLOR) * sw
    with open("/dev/fb0", "wb") as fb:
        for _ in range(sh):
            fb.write(row)

    # Blit PALS logo centered
    if not os.path.exists(LOGO_PATH):
        return
    with open(LOGO_PATH, "rb") as f:
        lw, lh = struct.unpack("<II", f.read(8))
        logo_data = f.read()

    ox = max(0, (sw - lw) // 2)
    oy = max(0, (sh - lh) // 2)

    with open("/dev/fb0", "r+b") as fb:
        for y in range(lh):
            if oy + y >= sh:
                break
            fb.seek((oy + y) * stride + ox * 4)
            src_off = y * lw * 4
            row_data = logo_data[src_off:src_off + lw * 4]
            # Alpha-blend each pixel over background
            blended = bytearray(len(row_data))
            for x in range(lw):
                if ox + x >= sw:
                    break
                pi = x * 4
                b, g, r, a = row_data[pi], row_data[pi+1], row_data[pi+2], row_data[pi+3]
                if a == 255:
                    blended[pi:pi+4] = bytes([b, g, r, 255])
                elif a == 0:
                    blended[pi:pi+4] = bytes(BG_COLOR)
                else:
                    inv = 255 - a
                    blended[pi]   = (b * a + BG_COLOR[0] * inv + 127) // 255
                    blended[pi+1] = (g * a + BG_COLOR[1] * inv + 127) // 255
                    blended[pi+2] = (r * a + BG_COLOR[2] * inv + 127) // 255
                    blended[pi+3] = 255
            fb.write(blended[:min(lw, sw - ox) * 4])

if __name__ == "__main__":
    main()
FBEOF
chmod +x "$ROOTFS_DIR/usr/local/bin/pals-fb-splash"

# Create dracut module for early fb splash (runs before Plymouth)
DRACUT_MOD="$ROOTFS_DIR/usr/lib/dracut/modules.d/00pals-fb"
mkdir -p "$DRACUT_MOD"
cat > "$DRACUT_MOD/module-setup.sh" << 'DRACEOF'
#!/bin/bash
check() { return 0; }
depends() { return 0; }
install() {
    inst_simple /usr/local/bin/pals-fb-splash
    inst_simple /usr/local/share/pals-fb/logo.bgra
    inst_hook pre-trigger 00 "$moddir/pals-fb-hook.sh"
}
DRACEOF
cat > "$DRACUT_MOD/pals-fb-hook.sh" << 'HOOKEOF'
#!/bin/sh
# Paint PALS logo to framebuffer before Plymouth starts.
# Runs in initrd with minimal environment.
if [ -x /usr/local/bin/pals-fb-splash ] && [ -e /dev/fb0 ]; then
    /usr/local/bin/pals-fb-splash 2>/dev/null &
fi
HOOKEOF
chmod +x "$DRACUT_MOD/module-setup.sh" "$DRACUT_MOD/pals-fb-hook.sh"
echo -e "  ${GREEN}Framebuffer splash installed (initrd + runtime)${NC}"

# Build initrd with PALS Plymouth theme + fb splash
echo -e "  Building initrd with PALS Plymouth + fb splash..."
KVER=$(ls "$ROOTFS_DIR/lib/modules/" | head -1)

mount --bind /proc "$ROOTFS_DIR/proc"
mount --bind /sys "$ROOTFS_DIR/sys"
mount --bind /dev "$ROOTFS_DIR/dev"

chroot "$ROOTFS_DIR" dracut --force --no-hostonly \
  --add "dmsquash-live plymouth pals-fb" \
  /tmp/live-initrd.img "$KVER" 2>/dev/null || true

cp "$ROOTFS_DIR/tmp/live-initrd.img" "$WORK_DIR/initrd"
cp "$ROOTFS_DIR/boot/vmlinuz-$KVER" "$WORK_DIR/linux"
rm -f "$ROOTFS_DIR/tmp/live-initrd.img"

umount "$ROOTFS_DIR/proc" "$ROOTFS_DIR/sys" "$ROOTFS_DIR/dev"
echo -e "  ${GREEN}Initrd built with PALS theme + fb splash${NC}"

build_target() {
  local target="$1"
  local label="$2"

  # Unmount existing partitions
  for part in "${target}"*; do
    umount "$part" 2>/dev/null || true
  done

  # Wipe and partition: 400MB FAT32 EFI + main ext4 + 20MB FEDAC-PIECE
  echo -e "  Creating partition table on ${label}..."
  wipefs -a "$target" >/dev/null 2>&1
  # Compute LIVE partition end to avoid negative offsets (which confuse
  # parted on some versions / loop devices).
  local dev_bytes
  dev_bytes=$(blockdev --getsize64 "$target" 2>/dev/null || stat -c%s "$target" 2>/dev/null)
  local dev_mib=$((dev_bytes / 1048576))
  local live_end=$((dev_mib - 20))  # leave 20 MiB for PIECE partition

  parted -s "$target" mklabel gpt
  parted -s "$target" mkpart '"EFI"' fat32 1MiB 401MiB
  parted -s "$target" set 1 esp on
  parted -s "$target" mkpart '"LIVE"' ext4 401MiB "${live_end}MiB"
  parted -s "$target" mkpart '"PIECE"' ext4 "${live_end}MiB" 100%

  sleep 2
  partprobe "$target" 2>/dev/null || true
  sleep 2

  # Detect partition names
  local p1="${target}1"
  local p2="${target}2"
  local p3="${target}3"
  [ -b "$p1" ] || p1="${target}p1"
  [ -b "$p2" ] || p2="${target}p2"
  [ -b "$p3" ] || p3="${target}p3"

  # Format
  mkfs.vfat -F 32 -n BOOT "$p1" >/dev/null
  mkfs.ext4 -L FEDAC-LIVE -q "$p2"
  mkfs.ext4 -L FEDAC-PIECE -q "$p3"
  echo -e "  ${GREEN}${label}: partitions created (EFI + LIVE + PIECE)${NC}"

  # Write piece.html to FEDAC-PIECE partition
  local PIECE_MOUNT
  PIECE_MOUNT=$(mktemp -d /tmp/fedac-piece-XXXX)
  mount "$p3" "$PIECE_MOUNT"
  cp "$BUNDLE_PATH" "$PIECE_MOUNT/piece.html"
  sync
  umount "$PIECE_MOUNT"
  rmdir "$PIECE_MOUNT"
  echo -e "  ${GREEN}${label}: piece.html written to FEDAC-PIECE${NC}"

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
  linux /loader/linux quiet splash rhgb loglevel=0 systemd.log_level=emerg systemd.show_status=false rd.systemd.show_status=false rd.udev.log_level=3 udev.log_priority=3 rd.plymouth=1 plymouth.ignore-serial-consoles vt.handoff=7 vt.global_cursor_default=0 logo.nologo root=live:LABEL=FEDAC-LIVE rd.live.image mitigations=off selinux=0
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

# Generate manifest for base images (used by oven /os endpoint)
if [ "$BASE_IMAGE_MODE" = true ] && [ -n "$IMAGE_PATH" ] && [ -f "$IMAGE_PATH" ]; then
  MANIFEST_PATH="${IMAGE_PATH%.img}-manifest.json"
  IMG_SIZE=$(stat -c%s "$IMAGE_PATH")
  IMG_SHA256=$(sha256sum "$IMAGE_PATH" | awk '{print $1}')
  # Get FEDAC-PIECE partition offset from the loop device or image
  PIECE_OFFSET=$(python3 -c "
import subprocess, json, re
out = subprocess.check_output(['fdisk', '-l', '$IMAGE_PATH'], text=True)
for line in out.splitlines():
    if 'PIECE' in line or line.strip().endswith('Linux filesystem') and '3' in line.split()[0]:
        # Sector-based offset: start_sector * 512
        parts = line.split()
        print(int(parts[1]) * 512)
        break
" 2>/dev/null || echo "0")
  PIECE_SIZE=$((20 * 1024 * 1024))
  cat > "$MANIFEST_PATH" << MANIFEST_EOF
{
  "version": "$(date +%Y-%m-%d)",
  "fedora": "${FEDORA_RELEASE}",
  "piecePartitionOffset": ${PIECE_OFFSET},
  "piecePartitionSize": ${PIECE_SIZE},
  "totalSize": ${IMG_SIZE},
  "sha256": "${IMG_SHA256}"
}
MANIFEST_EOF
  echo -e "  ${GREEN}Manifest written: ${MANIFEST_PATH}${NC}"
fi

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
  if [ "$BASE_IMAGE_MODE" = true ]; then
    echo -e "Manifest:"
    echo -e "  ${CYAN}${MANIFEST_PATH}${NC}"
    echo ""
    echo -e "Upload to DO Spaces for oven /os endpoint:"
    echo -e "  ${YELLOW}aws s3 cp ${IMAGE_PATH} s3://assets-aesthetic-computer/os/fedac-base-latest.img --endpoint-url https://sfo3.digitaloceanspaces.com --acl public-read${NC}"
    echo -e "  ${YELLOW}aws s3 cp ${MANIFEST_PATH} s3://assets-aesthetic-computer/os/fedac-base-manifest.json --endpoint-url https://sfo3.digitaloceanspaces.com --acl public-read${NC}"
  fi
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
