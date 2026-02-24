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
#   sudo bash fedac/scripts/make-kiosk-piece-usb.sh <piece-code> <device>
#
# Examples:
#   sudo bash fedac/scripts/make-kiosk-piece-usb.sh roz /dev/sdb
#   sudo bash fedac/scripts/make-kiosk-piece-usb.sh '$cow' /dev/sdb
#   sudo bash fedac/scripts/make-kiosk-piece-usb.sh roz /dev/sdb --rootfs /path/to/rootfs
#
# Options:
#   --rootfs <dir>    Use existing extracted rootfs (skip ISO download + extract)
#   --iso <path>      Use local ISO instead of downloading
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

# Oven bundle endpoint
OVEN_URL="https://oven.aesthetic.computer/bundle-html"

usage() {
  echo -e "${PINK}FedAC Kiosk Piece USB Creator${NC}"
  echo ""
  echo "Creates a live USB that boots into a fullscreen KidLisp piece (offline)."
  echo ""
  echo "Usage: sudo $0 <piece-code> <device> [options]"
  echo ""
  echo "  <piece-code>    KidLisp piece code (e.g., roz, \$cow)"
  echo "  <device>        USB block device (e.g., /dev/sdb)"
  echo ""
  echo "Options:"
  echo "  --rootfs <dir>  Use existing extracted rootfs directory"
  echo "  --iso <path>    Use existing ISO instead of downloading"
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
}
trap cleanup EXIT

# ── Parse args ──
PIECE_CODE=""
DEVICE=""
ROOTFS_DIR=""
ISO_PATH=""
DO_EJECT=true
SKIP_CONFIRM=false

while [ $# -gt 0 ]; do
  case "$1" in
    --rootfs) ROOTFS_DIR="$2"; shift 2 ;;
    --iso) ISO_PATH="$2"; shift 2 ;;
    --no-eject) DO_EJECT=false; shift ;;
    --yes) SKIP_CONFIRM=true; shift ;;
    --help|-h) usage ;;
    /dev/*) DEVICE="$1"; shift ;;
    *) if [ -z "$PIECE_CODE" ]; then PIECE_CODE="$1"; shift; else echo -e "${RED}Unknown arg: $1${NC}"; usage; fi ;;
  esac
done

[ -n "$PIECE_CODE" ] || { echo -e "${RED}Error: No piece code specified${NC}"; usage; }
[ -n "$DEVICE" ] || { echo -e "${RED}Error: No device specified${NC}"; usage; }
[ -b "$DEVICE" ] || { echo -e "${RED}Error: $DEVICE is not a block device${NC}"; exit 1; }

if [ "$(id -u)" -ne 0 ]; then
  echo -e "${RED}Error: Must run as root (sudo)${NC}"
  exit 1
fi

# Safety: refuse system disks
case "$DEVICE" in
  /dev/nvme*|/dev/vda|/dev/xvda|/dev/sda)
    echo -e "${RED}REFUSED: $DEVICE looks like a system disk.${NC}"
    exit 1
    ;;
esac

# Check tools
for tool in mkfs.erofs curl; do
  command -v "$tool" &>/dev/null || { echo -e "${RED}Error: $tool not found${NC}"; exit 1; }
done

echo -e "${PINK}╔═══════════════════════════════════════════════╗${NC}"
echo -e "${PINK}║     FedAC Kiosk Piece USB Creator             ║${NC}"
echo -e "${PINK}║  Piece: ${CYAN}${PIECE_CODE}${PINK} → offline bootable USB       ║${NC}"
echo -e "${PINK}╚═══════════════════════════════════════════════╝${NC}"
echo ""

WORK_DIR=$(mktemp -d /tmp/fedac-kiosk-XXXX)
echo -e "Work dir: $WORK_DIR"
echo ""

# ══════════════════════════════════════════
# Step 1: Fetch piece bundle from oven
# ══════════════════════════════════════════
echo -e "${CYAN}[1/6] Fetching piece bundle: ${PIECE_CODE}...${NC}"

BUNDLE_PATH="$WORK_DIR/piece.html"
BUNDLE_URL="${OVEN_URL}?code=${PIECE_CODE}&nocompress=1"

echo -e "  URL: $BUNDLE_URL"
HTTP_CODE=$(curl -s -o "$BUNDLE_PATH" -w "%{http_code}" "$BUNDLE_URL")

if [ "$HTTP_CODE" != "200" ]; then
  echo -e "${RED}Failed to fetch bundle (HTTP $HTTP_CODE)${NC}"
  echo "  Check that '$PIECE_CODE' is a valid piece code."
  exit 1
fi

BUNDLE_SIZE=$(stat -c%s "$BUNDLE_PATH")
echo -e "  ${GREEN}Bundle fetched: $(numfmt --to=iec $BUNDLE_SIZE)${NC}"

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

# 3b. Piece HTTP server
cat > "$ROOTFS_DIR/usr/local/bin/kiosk-piece-server.py" << 'PYEOF'
#!/usr/bin/env python3
"""FedAC Kiosk piece server — serves bundled HTML on localhost:8080."""
import http.server, socketserver

PORT = 8080
PIECE_PATH = "/usr/local/share/kiosk/piece.html"

class Handler(http.server.BaseHTTPRequestHandler):
    def log_message(self, fmt, *args): pass
    def do_GET(self):
        try:
            with open(PIECE_PATH, "rb") as f:
                data = f.read()
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)
        except Exception as e:
            self.send_response(500)
            self.end_headers()
            self.wfile.write(str(e).encode())

if __name__ == "__main__":
    socketserver.TCPServer.allow_reuse_address = True
    with socketserver.TCPServer(("0.0.0.0", PORT), Handler) as httpd:
        print(f"Piece server on http://0.0.0.0:{PORT}", flush=True)
        httpd.serve_forever()
PYEOF
chmod +x "$ROOTFS_DIR/usr/local/bin/kiosk-piece-server.py"

# 3c. Piece server systemd service
cat > "$ROOTFS_DIR/etc/systemd/system/kiosk-piece-server.service" << 'SVCEOF'
[Unit]
Description=FedAC Kiosk Piece Server
After=local-fs.target
DefaultDependencies=no

[Service]
Type=simple
ExecStart=/usr/bin/python3 /usr/local/bin/kiosk-piece-server.py
Restart=always
RestartSec=1

[Install]
WantedBy=multi-user.target
SVCEOF
mkdir -p "$ROOTFS_DIR/etc/systemd/system/multi-user.target.wants"
ln -sf /etc/systemd/system/kiosk-piece-server.service \
  "$ROOTFS_DIR/etc/systemd/system/multi-user.target.wants/kiosk-piece-server.service"
echo -e "  ${GREEN}Piece server + service installed${NC}"

# 3d. Kiosk session (mutter + Firefox, no GNOME Shell)
cat > "$ROOTFS_DIR/usr/local/bin/kiosk-session.sh" << 'SESSEOF'
#!/bin/bash
export XDG_SESSION_TYPE=wayland
for i in $(seq 1 30); do
  curl -s -o /dev/null http://127.0.0.1:8080 && break
  sleep 1
done
exec mutter --wayland --no-x11 -- firefox --kiosk --no-remote http://127.0.0.1:8080
SESSEOF
chmod +x "$ROOTFS_DIR/usr/local/bin/kiosk-session.sh"

cat > "$ROOTFS_DIR/usr/share/wayland-sessions/kiosk.desktop" << 'DESKEOF'
[Desktop Entry]
Name=FedAC Kiosk
Comment=Firefox Kiosk (no desktop)
Exec=/usr/local/bin/kiosk-session.sh
Type=Application
DesktopNames=GNOME
DESKEOF
echo -e "  ${GREEN}Kiosk wayland session installed${NC}"

# 3e. AccountsService — tell GDM to use kiosk session
mkdir -p "$ROOTFS_DIR/var/lib/AccountsService/users"
cat > "$ROOTFS_DIR/var/lib/AccountsService/users/liveuser" << 'ASEOF'
[User]
Session=kiosk
XSession=kiosk
SystemAccount=false
ASEOF

# 3f. Replace livesys-gnome (Fedora live boot script)
cat > "$ROOTFS_DIR/usr/libexec/livesys/sessions.d/livesys-gnome" << 'LSEOF'
#!/usr/bin/sh
# livesys-gnome: FedAC Kiosk setup

cat >> /usr/share/glib-2.0/schemas/org.gnome.software.gschema.override << FOE
[org.gnome.software]
allow-updates=false
download-updates=false
FOE
rm -f /etc/xdg/autostart/org.gnome.Software.desktop

mkdir -p ~liveuser/.config
echo yes > ~liveuser/.config/gnome-initial-setup-done

rm -f /etc/xdg/autostart/gnome-initial-setup*.desktop
rm -f /usr/share/applications/gnome-initial-setup.desktop
rm -f ~liveuser/.config/autostart/org.fedoraproject.welcome-screen.desktop
rm -f ~liveuser/.config/autostart/fedora-welcome.desktop
rm -f /etc/xdg/autostart/org.gnome.Tour.desktop
rm -f /usr/share/applications/anaconda.desktop
rm -f /usr/share/applications/liveinst.desktop

cat > /etc/gdm/custom.conf << FOE
[daemon]
AutomaticLoginEnable=True
AutomaticLogin=liveuser
DefaultSession=kiosk.desktop
FOE

mkdir -p /var/lib/AccountsService/users
cat > /var/lib/AccountsService/users/liveuser << FOE
[User]
Session=kiosk
XSession=kiosk
SystemAccount=false
FOE

cat >> /usr/share/glib-2.0/schemas/org.gnome.shell.gschema.override << FOE
[org.gnome.shell]
welcome-dialog-last-shown-version='4294967295'
favorite-apps=@as []
FOE

glib-compile-schemas /usr/share/glib-2.0/schemas
chown -R liveuser:liveuser ~liveuser/ 2>/dev/null || true
LSEOF
chmod +x "$ROOTFS_DIR/usr/libexec/livesys/sessions.d/livesys-gnome"
echo -e "  ${GREEN}livesys-gnome replaced${NC}"

# 3g. Firefox policies (no default tabs, no welcome)
mkdir -p "$ROOTFS_DIR/usr/lib64/firefox/distribution"
cat > "$ROOTFS_DIR/usr/lib64/firefox/distribution/policies.json" << 'FPEOF'
{
  "policies": {
    "OverrideFirstRunPage": "",
    "OverridePostUpdatePage": "",
    "Homepage": {
      "URL": "http://127.0.0.1:8080",
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

cat > "$ROOTFS_DIR/usr/lib64/firefox/defaults/pref/autoconfig.js" << 'ACEOF'
pref("general.config.filename", "firefox.cfg");
pref("general.config.obscure_value", 0);
ACEOF

cat > "$ROOTFS_DIR/usr/lib64/firefox/firefox.cfg" << 'CFGEOF'
// FedAC Kiosk Firefox config
defaultPref("browser.sessionstore.resume_from_crash", false);
defaultPref("browser.startup.homepage_override.mstone", "ignore");
defaultPref("browser.startup.page", 1);
defaultPref("browser.startup.homepage", "http://127.0.0.1:8080");
defaultPref("browser.tabs.warnOnClose", false);
defaultPref("browser.shell.checkDefaultBrowser", false);
defaultPref("datareporting.policy.dataSubmissionEnabled", false);
defaultPref("toolkit.telemetry.reportingpolicy.firstRun", false);
defaultPref("browser.newtabpage.enabled", false);
defaultPref("browser.aboutwelcome.enabled", false);
CFGEOF
echo -e "  ${GREEN}Firefox policies installed${NC}"

# 3h. Plymouth PALS logo
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

# 3i. dconf kiosk settings
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

# ══════════════════════════════════════════
# Step 5: Partition and flash USB
# ══════════════════════════════════════════
echo -e "${CYAN}[5/6] Partitioning and flashing USB...${NC}"

# Confirm
echo ""
echo -e "Target: ${YELLOW}$DEVICE${NC}"
lsblk "$DEVICE" 2>/dev/null || true
echo ""

if [ "$SKIP_CONFIRM" = false ]; then
  echo -e "${RED}ALL DATA ON $DEVICE WILL BE DESTROYED${NC}"
  read -p "Continue? [y/N] " confirm
  [ "$confirm" = "y" ] || [ "$confirm" = "Y" ] || { echo "Aborted."; exit 0; }
fi

# Unmount existing
for part in "${DEVICE}"*; do
  umount "$part" 2>/dev/null || true
done

# Wipe and partition: 400MB FAT32 EFI + rest ext4
echo -e "  Creating partition table..."
wipefs -a "$DEVICE" >/dev/null 2>&1
parted -s "$DEVICE" mklabel gpt
parted -s "$DEVICE" mkpart '"EFI"' fat32 1MiB 401MiB
parted -s "$DEVICE" set 1 esp on
parted -s "$DEVICE" mkpart '"LIVE"' ext4 401MiB 100%

sleep 2
partprobe "$DEVICE" 2>/dev/null || true
sleep 2

# Detect partition names
P1="${DEVICE}1"
P2="${DEVICE}2"
[ -b "$P1" ] || P1="${DEVICE}p1"
[ -b "$P2" ] || P2="${DEVICE}p2"

# Format
mkfs.vfat -F 32 -n BOOT "$P1" >/dev/null
mkfs.ext4 -L FEDAC-LIVE -q "$P2"
echo -e "  ${GREEN}Partitions created${NC}"

# Flash EROFS
LIVE_MOUNT=$(mktemp -d /tmp/fedac-live-XXXX)
mount "$P2" "$LIVE_MOUNT"
mkdir -p "$LIVE_MOUNT/LiveOS"
cp "$EROFS_PATH" "$LIVE_MOUNT/LiveOS/squashfs.img"
sync
umount "$LIVE_MOUNT"
rmdir "$LIVE_MOUNT"
LIVE_MOUNT=""
echo -e "  ${GREEN}EROFS written to USB${NC}"

# Set up EFI partition
EFI_MOUNT=$(mktemp -d /tmp/fedac-efi-XXXX)
mount "$P1" "$EFI_MOUNT"

# Copy GRUB EFI binary from rootfs
mkdir -p "$EFI_MOUNT/EFI/BOOT"
if [ -f "$ROOTFS_DIR/boot/efi/EFI/fedora/grubx64.efi" ]; then
  cp "$ROOTFS_DIR/boot/efi/EFI/fedora/grubx64.efi" "$EFI_MOUNT/EFI/BOOT/BOOTX64.EFI"
elif [ -f "$ROOTFS_DIR/usr/lib/grub/x86_64-efi/monolithic/grubx64.efi" ]; then
  cp "$ROOTFS_DIR/usr/lib/grub/x86_64-efi/monolithic/grubx64.efi" "$EFI_MOUNT/EFI/BOOT/BOOTX64.EFI"
fi

# Copy kernel + initrd
mkdir -p "$EFI_MOUNT/loader"
cp "$WORK_DIR/linux" "$EFI_MOUNT/loader/linux"
cp "$WORK_DIR/initrd" "$EFI_MOUNT/loader/initrd"

# GRUB config
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
  linux /loader/linux quiet rhgb root=live:LABEL=FEDAC-LIVE rd.live.image plymouth.splash-delay=0 vt.global_cursor_default=0
  initrd /loader/initrd
}
GRUBEOF

mkdir -p "$EFI_MOUNT/EFI/fedora"
cp "$EFI_MOUNT/EFI/BOOT/grub.cfg" "$EFI_MOUNT/EFI/fedora/grub.cfg"

sync
umount "$EFI_MOUNT"
rmdir "$EFI_MOUNT"
EFI_MOUNT=""
echo -e "  ${GREEN}EFI partition configured${NC}"

# ══════════════════════════════════════════
# Step 6: Finalize
# ══════════════════════════════════════════
echo -e "${CYAN}[6/6] Finalizing...${NC}"
sync

if [ "$DO_EJECT" = true ]; then
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
echo -e "${GREEN}║       FedAC Kiosk USB Ready                   ║${NC}"
echo -e "${GREEN}║  Piece: ${CYAN}${PIECE_CODE}${GREEN} (offline)                     ║${NC}"
echo -e "${GREEN}╚═══════════════════════════════════════════════╝${NC}"
echo ""
echo -e "Boot flow:"
echo -e "  Power on → GRUB (instant) → ${PINK}PALS splash${NC}"
echo -e "  → ${PINK}Firefox fullscreen → ${PIECE_CODE}${NC} (offline)"
echo ""
echo "Plug into target machine → boot from USB → done."
