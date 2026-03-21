#!/bin/bash
# install.sh — Set up FedAC on an existing Fedora Workstation installation
#
# Run as root (or via sudo) on a fresh Fedora install:
#   sudo bash fedac/scripts/install.sh
#
# What it does:
#   1. Installs dependencies (fish, fuse, etc.)
#   2. Downloads AC Electron AppImage
#   3. Configures GDM auto-login
#   4. Installs Plymouth boot splash (pals)
#   5. Sets up GNOME kiosk settings (no lock, no sleep)
#   6. Installs ac-setup TUI on tty2
#   7. Creates autostart entry for AC Electron

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FEDAC_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
AC_USER="${AC_USER:-me}"

RED='\033[0;31m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
NC='\033[0m'

step() { echo -e "\n${CYAN}[$1] $2${NC}"; }

# Must be root
if [ "$(id -u)" -ne 0 ]; then
  echo -e "${RED}Error: Run as root (sudo bash fedac/scripts/install.sh)${NC}"
  exit 1
fi

# Check Fedora
if ! [ -f /etc/fedora-release ]; then
  echo -e "${RED}Warning: This doesn't look like Fedora. Proceeding anyway...${NC}"
fi

# ── 1. Install packages ──
step 1 "Installing packages..."
dnf install -y \
  fish \
  git \
  htop \
  curl \
  wget \
  unzip \
  python3 \
  python3-pip \
  fuse \
  fuse-libs \
  plymouth \
  NetworkManager \
  NetworkManager-wifi \
  mesa-dri-drivers \
  mesa-vulkan-drivers \
  xdg-utils

# ── 2. Set fish as default shell ──
step 2 "Setting fish as default shell for $AC_USER..."
chsh -s /usr/bin/fish "$AC_USER" 2>/dev/null || true

# ── 3. Download AC Electron ──
step 3 "Installing AC Electron..."
mkdir -p /opt/ac/bin
if [ -f "$FEDAC_DIR/overlays/ac-electron-kiosk/install-electron.sh" ]; then
  bash "$FEDAC_DIR/overlays/ac-electron-kiosk/install-electron.sh"
else
  echo "install-electron.sh not found, skipping AppImage download."
  echo "Place AppImage at /opt/ac/bin/aesthetic-computer.AppImage manually."
fi

# ── 4. Configure GDM auto-login ──
step 4 "Configuring GDM auto-login..."
mkdir -p /etc/gdm
cat > /etc/gdm/custom.conf << GDMEOF
[daemon]
AutomaticLoginEnable=True
AutomaticLogin=$AC_USER
WaylandEnable=true

[security]
AllowRoot=false

[xdmcp]

[chooser]
GDMEOF

# ── 5. Install Plymouth theme ──
step 5 "Installing Plymouth boot splash..."
PLYMOUTH_DIR="/usr/share/plymouth/themes/fedac"
mkdir -p "$PLYMOUTH_DIR"

# Copy theme files
if [ -d "$FEDAC_DIR/plymouth" ]; then
  cp "$FEDAC_DIR/plymouth/fedac.plymouth" "$PLYMOUTH_DIR/"
  cp "$FEDAC_DIR/plymouth/fedac.script" "$PLYMOUTH_DIR/"

  # Copy pals.png (check multiple locations)
  if [ -f "$FEDAC_DIR/plymouth/pals.png" ]; then
    cp "$FEDAC_DIR/plymouth/pals.png" "$PLYMOUTH_DIR/"
  elif [ -f "$FEDAC_DIR/../system/public/assets/direct/pals.png" ]; then
    cp "$FEDAC_DIR/../system/public/assets/direct/pals.png" "$PLYMOUTH_DIR/"
  else
    echo "WARNING: pals.png not found. Plymouth theme will be incomplete."
  fi

  plymouth-set-default-theme fedac 2>/dev/null || true
  # Rebuild initramfs with new theme
  dracut -f 2>/dev/null || true
  echo "Plymouth theme installed."
else
  echo "Plymouth theme files not found, skipping."
fi

# ── 6. GNOME kiosk settings ──
step 6 "Configuring GNOME for kiosk mode..."

# dconf profile
mkdir -p /etc/dconf/profile
cat > /etc/dconf/profile/user << 'DCONFEOF'
user-db:user
system-db:local
DCONFEOF

# dconf system settings
mkdir -p /etc/dconf/db/local.d
cat > /etc/dconf/db/local.d/00-fedac << 'DCONFDBEOF'
[org/gnome/desktop/session]
idle-delay=uint32 0

[org/gnome/desktop/screensaver]
lock-enabled=false
idle-activation-enabled=false

[org/gnome/desktop/notifications]
show-banners=false

[org/gnome/settings-daemon/plugins/power]
sleep-inactive-ac-type='nothing'
sleep-inactive-battery-timeout=1800
ambient-enabled=false
DCONFDBEOF

dconf update 2>/dev/null || true

# ── 7. Autostart entry ──
step 7 "Installing autostart entry..."
AC_HOME=$(eval echo "~$AC_USER")
mkdir -p "$AC_HOME/.config/autostart"

if [ -f "$FEDAC_DIR/overlays/ac-electron-kiosk/ac-electron-kiosk.desktop" ]; then
  cp "$FEDAC_DIR/overlays/ac-electron-kiosk/ac-electron-kiosk.desktop" \
     "$AC_HOME/.config/autostart/"
else
  cat > "$AC_HOME/.config/autostart/ac-electron-kiosk.desktop" << 'DESKTOPEOF'
[Desktop Entry]
Type=Application
Name=Aesthetic Computer
Exec=/opt/ac/bin/aesthetic-computer.AppImage --no-sandbox --kiosk
Terminal=false
X-GNOME-Autostart-enabled=true
X-GNOME-Autostart-Delay=2
DESKTOPEOF
fi

# ── 8. Install TUI on tty2 ──
step 8 "Installing ac-setup TUI..."
if [ -f "$FEDAC_DIR/overlays/ac-setup/ac-setup.py" ]; then
  cp "$FEDAC_DIR/overlays/ac-setup/ac-setup.py" /opt/ac/bin/ac-setup
  chmod +x /opt/ac/bin/ac-setup
  ln -sf /opt/ac/bin/ac-setup /usr/local/bin/ac-setup

  # Systemd service to auto-run TUI on tty2
  if [ -f "$FEDAC_DIR/systemd/ac-setup-tty.service" ]; then
    cp "$FEDAC_DIR/systemd/ac-setup-tty.service" /etc/systemd/system/
    systemctl daemon-reload
    systemctl enable ac-setup-tty.service
  fi
fi

# ── 9. Fix ownership ──
step 9 "Fixing ownership..."
chown -R "$AC_USER:$AC_USER" "$AC_HOME"
chown -R "$AC_USER:$AC_USER" /opt/ac

# ── Done ──
echo ""
echo -e "${GREEN}=== FedAC installed ===${NC}"
echo ""
echo "Next steps:"
echo "  1. Reboot to see the pals boot splash"
echo "  2. System will auto-login as '$AC_USER'"
echo "  3. AC Electron will launch fullscreen"
echo "  4. Press Ctrl+Alt+F2 for the setup TUI"
echo "  5. Press Ctrl+B in AC to flip to terminal"
echo ""
echo "To re-run this script: sudo bash fedac/scripts/install.sh"
