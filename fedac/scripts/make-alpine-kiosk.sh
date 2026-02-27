#!/bin/bash
# make-alpine-kiosk.sh — Create an Alpine Linux kiosk USB with a bundled piece
#
# Builds a minimal Alpine rootfs from scratch using apk.static, producing a
# ~400MB bootable image (vs ~4GB Fedora). Same piece injection / FEDAC-PIECE
# partition layout as the Fedora script for oven compatibility.
#
# Boot flow:
#   Power on → GRUB (instant) → linux-lts kernel → OpenRC → Cage → Chromium
#   → bundled piece (offline, no WiFi needed)
#
# Usage:
#   sudo bash fedac/scripts/make-alpine-kiosk.sh <piece-code> [<device>] [options]
#
# Options:
#   --image <path>    Build a bootable disk image file (.img)
#   --image-size <g>  Image size in GiB (default: 1)
#   --base-image      Build base image without a specific piece (placeholder only)
#   --density <n>     Default pack/runtime density query value (default: 8)
#   --work-base <dir> Directory for large temp work files (default: /tmp)
#   --no-eject        Don't eject when done
#   --yes             Skip confirmation prompts
#
# Requirements:
#   erofs-utils (mkfs.erofs), curl, parted, e2fsprogs, dosfstools

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

# Alpine 3.21 (edge for latest Chromium)
ALPINE_VERSION="3.21"
ALPINE_MIRROR="https://dl-cdn.alpinelinux.org/alpine"
ALPINE_REPO_MAIN="${ALPINE_MIRROR}/v${ALPINE_VERSION}/main"
ALPINE_REPO_COMMUNITY="${ALPINE_MIRROR}/v${ALPINE_VERSION}/community"
APK_STATIC_URL="${ALPINE_MIRROR}/v${ALPINE_VERSION}/main/x86_64/apk-tools-static-2.14.6-r3.apk"
APK_CACHE_DIR="${HOME}/.cache/alpine-apk"

PACK_DENSITY_DEFAULT="8"

usage() {
  echo -e "${PINK}Alpine Kiosk Piece USB Creator${NC}"
  echo ""
  echo "Creates a minimal Alpine Linux USB that boots into a fullscreen piece (offline)."
  echo ""
  echo "Usage: sudo $0 <piece-code> [<device>] [options]"
  echo ""
  echo "  <piece-code>    Piece code (e.g., notepat, \$cow)"
  echo "  <device>        Optional USB block device (e.g., /dev/sdb)"
  echo ""
  echo "Options:"
  echo "  --image <path>    Build a bootable disk image file (.img)"
  echo "  --image-size <g>  Image size in GiB (default: 1)"
  echo "  --base-image      Build base image (placeholder piece)"
  echo "  --density <n>     Runtime density (default: 8)"
  echo "  --work-base <dir> Temp directory (default: /tmp)"
  echo "  --no-eject        Don't eject the USB when done"
  echo "  --yes             Skip confirmation prompts"
  echo "  --help            Show this help"
  exit 1
}

cleanup() {
  echo -e "\n${YELLOW}Cleaning up mounts...${NC}"
  for mp in "${EFI_MOUNT:-}" "${LIVE_MOUNT:-}" "${PIECE_MOUNT_TMP:-}"; do
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
IMAGE_PATH=""
IMAGE_SIZE_GB="1"
WORK_BASE="/tmp"
DO_EJECT=true
SKIP_CONFIRM=false
BASE_IMAGE_MODE=false
PACK_DENSITY="$PACK_DENSITY_DEFAULT"

while [ $# -gt 0 ]; do
  case "$1" in
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
      ;;
  esac
fi

# Check tools
for tool in mkfs.erofs curl parted mkfs.vfat mkfs.ext4 losetup; do
  command -v "$tool" &>/dev/null || { echo -e "${RED}Error: $tool not found${NC}"; exit 1; }
done

echo -e "${PINK}╔═══════════════════════════════════════════════╗${NC}"
echo -e "${PINK}║     Alpine Kiosk Piece USB Creator             ║${NC}"
echo -e "${PINK}║  Piece: ${CYAN}${PIECE_CODE}${PINK} → offline bootable USB       ║${NC}"
echo -e "${PINK}╚═══════════════════════════════════════════════╝${NC}"
echo ""

WORK_DIR=$(mktemp -d "$WORK_BASE/alpine-kiosk-XXXX")
echo -e "Work dir: $WORK_DIR"
echo ""

OWN_ROOTFS=true
ROOTFS_DIR="$WORK_DIR/rootfs"
EFI_MOUNT=""
LIVE_MOUNT=""
PIECE_MOUNT_TMP=""
LOOP_DEV=""

# ══════════════════════════════════════════
# Step 1: Fetch piece bundle
# ══════════════════════════════════════════
BUNDLE_PATH="$WORK_DIR/piece.html"

if [ "$BASE_IMAGE_MODE" = true ]; then
  echo -e "${CYAN}[1/6] Base image mode — generating placeholder piece...${NC}"
  cat > "$BUNDLE_PATH" << 'PLACEHOLDER_EOF'
<!DOCTYPE html><html><head><meta charset="utf-8"><title>FedOS Base</title></head>
<body style="background:#000;color:#fff;display:flex;align-items:center;justify-content:center;height:100vh;margin:0;font-family:monospace">
<p>No piece loaded. Build with aesthetic.computer/os</p>
</body></html>
PLACEHOLDER_EOF
else
  echo -e "${CYAN}[1/6] Fetching piece bundle from oven...${NC}"
  OVEN_URL="https://oven.aesthetic.computer/pack-html"
  if [[ "$PIECE_CODE" == \$* ]]; then
    FETCH_URL="${OVEN_URL}?code=${PIECE_CODE}&density=${PACK_DENSITY}"
  else
    FETCH_URL="${OVEN_URL}?piece=${PIECE_CODE}&density=${PACK_DENSITY}"
  fi
  echo -e "  Fetching: ${FETCH_URL}"
  HTTP_CODE=$(curl -sSL -w '%{http_code}' -o "$BUNDLE_PATH" "$FETCH_URL" 2>/dev/null)
  if [ "$HTTP_CODE" != "200" ]; then
    echo -e "${RED}Error: Oven returned HTTP ${HTTP_CODE}${NC}"
    exit 1
  fi
  BUNDLE_SIZE=$(stat -c%s "$BUNDLE_PATH")
  echo -e "  ${GREEN}Bundle fetched: $(numfmt --to=iec $BUNDLE_SIZE)${NC}"
fi

# ══════════════════════════════════════════
# Step 2: Bootstrap Alpine rootfs
# ══════════════════════════════════════════
echo -e "${CYAN}[2/6] Building Alpine rootfs...${NC}"

# 2a. Get apk.static (runs on any Linux host)
mkdir -p "$APK_CACHE_DIR"
APK_STATIC="$APK_CACHE_DIR/apk.static"
if [ ! -x "$APK_STATIC" ]; then
  echo -e "  Downloading apk-tools-static..."
  APK_TMP="$APK_CACHE_DIR/apk-tools-static.apk"
  curl -sSL -o "$APK_TMP" "$APK_STATIC_URL"
  # apk.static is inside the .apk at sbin/apk.static (it's a tar.gz)
  tar -xzf "$APK_TMP" -C "$APK_CACHE_DIR" sbin/apk.static 2>/dev/null || \
    tar -xf "$APK_TMP" -C "$APK_CACHE_DIR" sbin/apk.static 2>/dev/null
  mv "$APK_CACHE_DIR/sbin/apk.static" "$APK_STATIC"
  rmdir "$APK_CACHE_DIR/sbin" 2>/dev/null || true
  rm -f "$APK_TMP"
  chmod +x "$APK_STATIC"
  echo -e "  ${GREEN}apk.static ready${NC}"
fi

# 2b. Bootstrap the rootfs
mkdir -p "$ROOTFS_DIR"

echo -e "  Installing Alpine base + kiosk packages..."
$APK_STATIC \
  --root "$ROOTFS_DIR" \
  --initdb \
  --allow-untrusted \
  --no-progress \
  --repository "$ALPINE_REPO_MAIN" \
  --repository "$ALPINE_REPO_COMMUNITY" \
  add \
    alpine-base \
    busybox \
    openrc \
    linux-lts \
    linux-firmware-intel \
    linux-firmware-amdgpu \
    mesa-dri-gallium \
    mesa-egl \
    mesa-gl \
    mesa-gbm \
    libdrm \
    chromium \
    cage \
    wlroots \
    pipewire \
    wireplumber \
    pipewire-pulse \
    alsa-lib \
    alsa-plugins-pulse \
    eudev \
    dbus \
    font-noto \
    ttf-dejavu \
    python3 \
    seatd \
    xwayland

echo -e "  ${GREEN}Alpine packages installed${NC}"

# 2c. Configure Alpine repos inside rootfs
mkdir -p "$ROOTFS_DIR/etc/apk"
cat > "$ROOTFS_DIR/etc/apk/repositories" << EOF
${ALPINE_REPO_MAIN}
${ALPINE_REPO_COMMUNITY}
EOF

# 2d. Configure hostname and basic system
echo "fedos" > "$ROOTFS_DIR/etc/hostname"
cat > "$ROOTFS_DIR/etc/hosts" << EOF
127.0.0.1   localhost fedos
::1         localhost fedos
EOF

# 2e. Create kiosk user
echo -e "  Creating kioskuser..."
chroot "$ROOTFS_DIR" /usr/sbin/adduser -D -s /bin/sh -h /home/kioskuser kioskuser 2>/dev/null || true
chroot "$ROOTFS_DIR" /usr/sbin/addgroup kioskuser video 2>/dev/null || true
chroot "$ROOTFS_DIR" /usr/sbin/addgroup kioskuser audio 2>/dev/null || true
chroot "$ROOTFS_DIR" /usr/sbin/addgroup kioskuser input 2>/dev/null || true
chroot "$ROOTFS_DIR" /usr/sbin/addgroup kioskuser seat 2>/dev/null || true

# 2f. Configure inittab for autologin on tty1
cat > "$ROOTFS_DIR/etc/inittab" << 'EOF'
::sysinit:/sbin/openrc sysinit
::sysinit:/sbin/openrc boot
::wait:/sbin/openrc default
tty1::respawn:/bin/login -f kioskuser
tty2::askfirst:/bin/login
::ctrlaltdel:/sbin/reboot
::shutdown:/sbin/openrc shutdown
EOF

# 2g. Profile script — auto-start kiosk session on tty1
mkdir -p "$ROOTFS_DIR/home/kioskuser"
cat > "$ROOTFS_DIR/home/kioskuser/.profile" << 'PROFILEEOF'
# Auto-start kiosk session on tty1 only
if [ "$(tty)" = "/dev/tty1" ]; then
  exec /usr/local/bin/kiosk-session.sh
fi
PROFILEEOF
chown -R 1000:1000 "$ROOTFS_DIR/home/kioskuser" 2>/dev/null || true

echo -e "  ${GREEN}Alpine rootfs configured${NC}"

# ══════════════════════════════════════════
# Step 3: Inject kiosk config
# ══════════════════════════════════════════
echo -e "${CYAN}[3/6] Injecting kiosk config...${NC}"

# 3a. Place the piece bundle
mkdir -p "$ROOTFS_DIR/usr/local/share/kiosk"
cp "$BUNDLE_PATH" "$ROOTFS_DIR/usr/local/share/kiosk/piece.html"
echo -e "  ${GREEN}Piece bundle installed${NC}"

KIOSK_PIECE_URL="http://localhost:8080/piece.html?density=${PACK_DENSITY}"

# 3b. Kiosk session script (adapted for Alpine/OpenRC)
cat > "$ROOTFS_DIR/usr/local/bin/kiosk-session.sh" << 'SESSEOF'
#!/bin/sh
# Alpine Kiosk Session — Cage + Chromium, PipeWire audio
# Write logs to persistent FEDAC-PIECE partition
PIECE_LOG=""
PIECE_DEV=$(blkid -L FEDAC-PIECE 2>/dev/null || true)
if [ -n "$PIECE_DEV" ]; then
  mkdir -p /mnt/piece
  mount "$PIECE_DEV" /mnt/piece 2>/dev/null || true
  if mountpoint -q /mnt/piece 2>/dev/null; then
    PIECE_LOG="/mnt/piece/kiosk.log"
  fi
fi
LOG="${PIECE_LOG:-/tmp/kiosk.log}"
exec > "$LOG" 2>&1
echo "[kiosk] $(date) — kiosk-session.sh starting (Alpine)"
echo "[kiosk] log file: $LOG"

export XDG_SESSION_TYPE=wayland
export XDG_RUNTIME_DIR="/run/user/$(id -u)"
mkdir -p "$XDG_RUNTIME_DIR"

# Link piece files from FEDAC-PIECE if available
if [ -f /mnt/piece/piece.html ]; then
  for f in /mnt/piece/*.html; do
    [ -f "$f" ] && ln -sf "$f" "/usr/local/share/kiosk/$(basename "$f")"
  done
  echo "[kiosk] linked piece files from FEDAC-PIECE partition"
  ls -la /usr/local/share/kiosk/*.html 2>&1
fi

# Start PipeWire audio stack
if command -v pipewire >/dev/null 2>&1; then
  pipewire &
  sleep 0.2
  command -v wireplumber >/dev/null 2>&1 && wireplumber &
  command -v pipewire-pulse >/dev/null 2>&1 && pipewire-pulse &
fi

echo "[kiosk] XDG_RUNTIME_DIR=$XDG_RUNTIME_DIR"
echo "[kiosk] KIOSK_PIECE_URL=__KIOSK_PIECE_URL__"

# Wait for piece server (port 8080)
for i in $(seq 1 30); do
  if python3 -c "import socket; s=socket.socket(); s.settimeout(0.5); s.connect(('127.0.0.1',8080)); s.close()" 2>/dev/null; then
    echo "[kiosk] piece server ready (attempt $i)"
    break
  fi
  echo "[kiosk] waiting for piece server... ($i/30)"
  sleep 0.5
done

echo "[kiosk] launching cage + chromium"
# Cage launches a single Wayland app fullscreen with black background
exec cage -- chromium-browser \
  --no-first-run \
  --disable-translate \
  --disable-infobars \
  --disable-suggestions-service \
  --disable-save-password-bubble \
  --disable-session-crashed-bubble \
  --disable-component-update \
  --no-default-browser-check \
  --autoplay-policy=no-user-gesture-required \
  --kiosk \
  --disable-pinch \
  --overscroll-history-navigation=0 \
  --enable-features=OverlayScrollbar \
  --force-device-scale-factor=1 \
  --disable-background-networking \
  --disable-sync \
  --metrics-recording-only \
  --disable-default-apps \
  --mute-audio=false \
  --no-sandbox \
  --disable-gpu-sandbox \
  --enable-gpu-rasterization \
  --enable-zero-copy \
  --ignore-gpu-blocklist \
  --enable-features=VaapiVideoDecoder,VaapiVideoEncoder \
  "__KIOSK_PIECE_URL__"
SESSEOF

# Replace placeholder URLs
sed -i "s|__KIOSK_PIECE_URL__|${KIOSK_PIECE_URL}|g" "$ROOTFS_DIR/usr/local/bin/kiosk-session.sh"
chmod +x "$ROOTFS_DIR/usr/local/bin/kiosk-session.sh"
echo -e "  ${GREEN}Kiosk session script installed${NC}"

# 3c. Install piece server (Python HTTP + volume API)
cp "$OVERLAY_DIR/kiosk-piece-server.py" "$ROOTFS_DIR/usr/local/bin/kiosk-piece-server.py"
chmod +x "$ROOTFS_DIR/usr/local/bin/kiosk-piece-server.py"

# Adapt piece server for Alpine: use kioskuser instead of liveuser
sed -i 's/USER = "liveuser"/USER = "kioskuser"/' "$ROOTFS_DIR/usr/local/bin/kiosk-piece-server.py"

# 3d. OpenRC service for piece server (instead of systemd)
cat > "$ROOTFS_DIR/etc/init.d/kiosk-piece-server" << 'OPENRCEOF'
#!/sbin/openrc-run
description="FedOS Kiosk Piece Server"
command="/usr/bin/python3"
command_args="/usr/local/bin/kiosk-piece-server.py"
pidfile="/run/${RC_SVCNAME}.pid"
command_background=true

depend() {
    need localmount
    after bootmisc
}
OPENRCEOF
chmod +x "$ROOTFS_DIR/etc/init.d/kiosk-piece-server"

# 3e. Enable OpenRC services
for svc in devfs dmesg mdev hwdrivers seatd dbus kiosk-piece-server; do
  chroot "$ROOTFS_DIR" /sbin/rc-update add "$svc" default 2>/dev/null || \
    chroot "$ROOTFS_DIR" /sbin/rc-update add "$svc" boot 2>/dev/null || true
done
# seatd is needed for cage to access DRM
# dbus is needed for PipeWire

# 3f. Configure seatd for kioskuser
mkdir -p "$ROOTFS_DIR/etc/seatd"
echo 'kioskuser' > "$ROOTFS_DIR/etc/seatd/seatd.conf" 2>/dev/null || true

# 3g. Basic kernel modules config
mkdir -p "$ROOTFS_DIR/etc/modules-load.d"
cat > "$ROOTFS_DIR/etc/modules-load.d/kiosk.conf" << EOF
# GPU drivers
i915
amdgpu
# Sound
snd_hda_intel
snd_hda_codec_hdmi
# Input
uinput
EOF

echo -e "  ${GREEN}Kiosk config injected${NC}"

# ══════════════════════════════════════════
# Step 4: Build EROFS + prepare boot files
# ══════════════════════════════════════════
echo -e "${CYAN}[4/6] Building EROFS image...${NC}"

# 4a. Copy kernel and initramfs out before EROFS compression
KERNEL_SRC=$(ls "$ROOTFS_DIR/boot/vmlinuz-"*lts* 2>/dev/null | head -1)
INITRD_SRC=$(ls "$ROOTFS_DIR/boot/initramfs-"*lts* 2>/dev/null | head -1)

if [ -z "$KERNEL_SRC" ]; then
  echo -e "${RED}Error: No kernel found in rootfs${NC}"
  ls "$ROOTFS_DIR/boot/" 2>&1
  exit 1
fi

cp "$KERNEL_SRC" "$WORK_DIR/vmlinuz"
echo -e "  Kernel: $(basename $KERNEL_SRC)"

if [ -n "$INITRD_SRC" ] && [ -f "$INITRD_SRC" ]; then
  cp "$INITRD_SRC" "$WORK_DIR/initramfs"
  echo -e "  Initramfs: $(basename $INITRD_SRC)"
else
  # Generate a minimal initramfs if not provided
  echo -e "  ${YELLOW}No initramfs found, generating...${NC}"
  if command -v mkinitfs >/dev/null 2>&1; then
    KVER=$(basename "$KERNEL_SRC" | sed 's/vmlinuz-//')
    chroot "$ROOTFS_DIR" mkinitfs -o /boot/initramfs "$KVER" 2>/dev/null || true
    INITRD_SRC=$(ls "$ROOTFS_DIR/boot/initramfs"* 2>/dev/null | head -1)
    [ -f "$INITRD_SRC" ] && cp "$INITRD_SRC" "$WORK_DIR/initramfs"
  fi
  if [ ! -f "$WORK_DIR/initramfs" ]; then
    echo -e "${RED}Error: Could not generate initramfs${NC}"
    exit 1
  fi
fi

# 4b. Remove boot directory from rootfs (kernel/initramfs go on EFI partition)
rm -rf "$ROOTFS_DIR/boot"
mkdir -p "$ROOTFS_DIR/boot"

# 4c. Clean up to minimize image size
rm -rf "$ROOTFS_DIR/var/cache/apk"/*
rm -rf "$ROOTFS_DIR/usr/share/doc"/*
rm -rf "$ROOTFS_DIR/usr/share/man"/*
rm -rf "$ROOTFS_DIR/usr/share/info"/*
find "$ROOTFS_DIR/usr/share/locale" -mindepth 1 -maxdepth 1 ! -name 'en*' -exec rm -rf {} + 2>/dev/null || true

ROOTFS_SIZE=$(du -sm "$ROOTFS_DIR" | awk '{print $1}')
echo -e "  Rootfs size before compression: ${ROOTFS_SIZE}MB"

# 4d. Build EROFS
EROFS_PATH="$WORK_DIR/rootfs.erofs"
mkfs.erofs -zlz4hc,9 -C65536 "$EROFS_PATH" "$ROOTFS_DIR/"
EROFS_SIZE=$(stat -c%s "$EROFS_PATH")
echo -e "  ${GREEN}EROFS built: $(numfmt --to=iec $EROFS_SIZE)${NC}"

# ══════════════════════════════════════════
# Step 5: Build image and/or flash USB
# ══════════════════════════════════════════
echo -e "${CYAN}[5/6] Building output media...${NC}"

build_target() {
  local target="$1"
  local label="$2"

  # Unmount existing partitions
  for part in "${target}"*; do
    umount "$part" 2>/dev/null || true
  done

  # Wipe and partition: EFI + ROOT (EROFS) + PIECE
  echo -e "  Creating partition table on ${label}..."
  wipefs -a "$target" >/dev/null 2>&1

  local dev_bytes
  dev_bytes=$(blockdev --getsize64 "$target" 2>/dev/null || stat -c%s "$target" 2>/dev/null)
  local dev_mib=$((dev_bytes / 1048576))

  # EFI: 128MB (kernel + initramfs + grub), ROOT: bulk, PIECE: 20MB at end
  local efi_end=129  # 1MiB start + 128MiB
  local piece_start=$((dev_mib - 20))

  parted -s "$target" mklabel gpt
  parted -s "$target" mkpart '"EFI"' fat32 1MiB "${efi_end}MiB"
  parted -s "$target" set 1 esp on
  parted -s "$target" mkpart '"ROOT"' ext4 "${efi_end}MiB" "${piece_start}MiB"
  parted -s "$target" mkpart '"PIECE"' ext4 "${piece_start}MiB" 100%

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
  mkfs.ext4 -L ALPINE-ROOT -q "$p2"
  mkfs.ext4 -L FEDAC-PIECE -q "$p3"
  echo -e "  ${GREEN}${label}: partitions created (EFI + ROOT + PIECE)${NC}"

  # Write piece files to FEDAC-PIECE partition
  PIECE_MOUNT_TMP=$(mktemp -d /tmp/alpine-piece-XXXX)
  mount "$p3" "$PIECE_MOUNT_TMP"
  cp "$BUNDLE_PATH" "$PIECE_MOUNT_TMP/piece.html"
  sync
  umount "$PIECE_MOUNT_TMP"
  rmdir "$PIECE_MOUNT_TMP"
  PIECE_MOUNT_TMP=""
  echo -e "  ${GREEN}${label}: piece.html written to FEDAC-PIECE${NC}"

  # Write EROFS rootfs to ROOT partition
  LIVE_MOUNT=$(mktemp -d /tmp/alpine-root-XXXX)
  mount "$p2" "$LIVE_MOUNT"
  mkdir -p "$LIVE_MOUNT/LiveOS"
  cp "$EROFS_PATH" "$LIVE_MOUNT/LiveOS/rootfs.erofs"
  sync
  umount "$LIVE_MOUNT"
  rmdir "$LIVE_MOUNT"
  LIVE_MOUNT=""
  echo -e "  ${GREEN}${label}: EROFS rootfs written${NC}"

  # Set up EFI partition
  EFI_MOUNT=$(mktemp -d /tmp/alpine-efi-XXXX)
  mount "$p1" "$EFI_MOUNT"

  # Copy kernel and initramfs
  mkdir -p "$EFI_MOUNT/boot"
  cp "$WORK_DIR/vmlinuz" "$EFI_MOUNT/boot/vmlinuz"
  cp "$WORK_DIR/initramfs" "$EFI_MOUNT/boot/initramfs"

  # Install GRUB EFI
  mkdir -p "$EFI_MOUNT/EFI/BOOT"

  # Try to get grub from rootfs or host
  if [ -f "$ROOTFS_DIR/usr/lib/grub/x86_64-efi/monolithic/grubx64.efi" ]; then
    cp "$ROOTFS_DIR/usr/lib/grub/x86_64-efi/monolithic/grubx64.efi" "$EFI_MOUNT/EFI/BOOT/BOOTX64.EFI"
  elif command -v grub2-mkimage >/dev/null 2>&1; then
    grub2-mkimage -O x86_64-efi -o "$EFI_MOUNT/EFI/BOOT/BOOTX64.EFI" \
      normal linux fat part_gpt efi_gop efi_uga search search_label all_video gzio 2>/dev/null || true
  elif command -v grub-mkimage >/dev/null 2>&1; then
    grub-mkimage -O x86_64-efi -o "$EFI_MOUNT/EFI/BOOT/BOOTX64.EFI" \
      normal linux fat part_gpt efi_gop efi_uga search search_label all_video gzio 2>/dev/null || true
  fi

  # If we still don't have a GRUB EFI binary, try copying from host
  if [ ! -f "$EFI_MOUNT/EFI/BOOT/BOOTX64.EFI" ]; then
    for candidate in \
      /boot/efi/EFI/fedora/grubx64.efi \
      /usr/lib/grub/x86_64-efi/monolithic/grubx64.efi \
      /usr/share/grub/x86_64-efi/grubx64.efi; do
      if [ -f "$candidate" ]; then
        cp "$candidate" "$EFI_MOUNT/EFI/BOOT/BOOTX64.EFI"
        break
      fi
    done
  fi

  if [ ! -f "$EFI_MOUNT/EFI/BOOT/BOOTX64.EFI" ]; then
    echo -e "${RED}Warning: No GRUB EFI binary found — image may not boot${NC}"
  fi

  # GRUB config — Alpine uses direct kernel boot (no live image)
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
menuentry "FedOS Alpine" {
  linux /boot/vmlinuz root=LABEL=ALPINE-ROOT rootfstype=ext4 ro quiet loglevel=0 mitigations=off
  initrd /boot/initramfs
}
GRUBEOF

  # Also put grub.cfg where some firmware looks
  mkdir -p "$EFI_MOUNT/EFI/fedora"
  cp "$EFI_MOUNT/EFI/BOOT/grub.cfg" "$EFI_MOUNT/EFI/fedora/grub.cfg"

  sync
  umount "$EFI_MOUNT"
  rmdir "$EFI_MOUNT"
  EFI_MOUNT=""
  echo -e "  ${GREEN}${label}: EFI configured${NC}"
}

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
  # Get FEDAC-PIECE partition offset
  PIECE_OFFSET=$(python3 -c "
import subprocess, json, re
out = subprocess.check_output(['fdisk', '-l', '$IMAGE_PATH'], text=True)
for line in out.splitlines():
    if 'PIECE' in line or line.strip().endswith('Linux filesystem') and '3' in line.split()[0]:
        parts = line.split()
        print(int(parts[1]) * 512)
        break
" 2>/dev/null || echo "0")
  PIECE_SIZE=$((20 * 1024 * 1024))
  cat > "$MANIFEST_PATH" << MANIFEST_EOF
{
  "version": "$(date +%Y-%m-%d)",
  "flavor": "alpine",
  "alpine": "${ALPINE_VERSION}",
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

# Clean up work dir
if [ "$OWN_ROOTFS" = true ]; then
  echo -e "  Cleaning work dir..."
  rm -rf "$WORK_DIR"
fi

echo ""
echo -e "${GREEN}╔═══════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║      Alpine Kiosk Build Ready                  ║${NC}"
echo -e "${GREEN}║  Piece: ${CYAN}${PIECE_CODE}${GREEN} (offline, ~400MB)             ║${NC}"
echo -e "${GREEN}╚═══════════════════════════════════════════════╝${NC}"
[ -n "$IMAGE_PATH" ] && echo -e "  Image: ${GREEN}${IMAGE_PATH}${NC}"
[ -n "$DEVICE" ] && echo -e "  USB: ${GREEN}${DEVICE}${NC}"
echo ""
