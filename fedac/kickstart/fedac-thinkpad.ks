# FedAC ThinkPad Kickstart
# Automated Fedora install for Aesthetic Computer kiosk
#
# Usage: Boot Fedora live USB, press Tab at GRUB, append:
#   inst.ks=https://raw.githubusercontent.com/whistlegraph/aesthetic-computer/main/fedac/kickstart/fedac-thinkpad.ks
#
# Or for local kickstart on USB:
#   inst.ks=hd:LABEL=BOOT:/fedac.ks

# Text mode install (no 'install' directive — removed in Fedora 38+)
text

# Language and locale
lang en_US.UTF-8
keyboard us
timezone America/New_York --utc

# Network — use DHCP, activate on boot
network --bootproto=dhcp --activate --hostname=aesthetic

# Root password (disabled — use sudo via wheel group)
rootpw --lock

# User: 'me' with sudo, fish shell
user --name=me --password=aesthetic --plaintext --groups=wheel --shell=/usr/bin/fish

# Disk — wipe and auto-partition (UEFI + ext4)
# WARNING: This erases the entire disk
zerombr
clearpart --all --initlabel
autopart --type=plain --fstype=ext4

# Bootloader
bootloader --append="quiet splash"

# Boot into graphical target
firstboot --disable
xconfig --startxonboot

# Packages
%packages
@base-graphical
@fonts
@gnome-desktop
@hardware-support
@multimedia
gdm
gnome-shell
gnome-terminal
gnome-tweaks
NetworkManager
NetworkManager-wifi
fish
git
htop
curl
wget
unzip
python3
python3-pip
plymouth
plymouth-theme-spinner
fuse
fuse-libs
mesa-dri-drivers
mesa-vulkan-drivers
xdg-utils
%end

# Post-install script
%post --log=/root/fedac-post-install.log

# Set fish as default shell for 'me'
chsh -s /usr/bin/fish me

# Enable GDM
systemctl enable gdm

# Configure GDM auto-login for 'me'
mkdir -p /etc/gdm
cat > /etc/gdm/custom.conf << 'GDMEOF'
[daemon]
AutomaticLoginEnable=True
AutomaticLogin=me
WaylandEnable=true

[security]
AllowRoot=false

[xdmcp]

[chooser]
GDMEOF

# Disable lock screen and screen blanking for kiosk
mkdir -p /home/me/.config/dconf
# Will be set up by install.sh on first boot

# Create AC directories
mkdir -p /opt/ac/{bin,offline-pieces,config}
mkdir -p /home/me/.local/share/applications
mkdir -p /home/me/.config/autostart
mkdir -p /home/me/.config/systemd/user
mkdir -p /home/me/.state

# Download and install AC Electron AppImage (latest release)
echo "Downloading AC Electron..."
RELEASE_URL=$(curl -s https://api.github.com/repos/whistlegraph/aesthetic-computer/releases/latest | \
  python3 -c "import sys,json; r=json.load(sys.stdin); [print(a['browser_download_url']) for a in r.get('assets',[]) if a['name'].endswith('.AppImage')]" 2>/dev/null | head -1)

if [ -n "$RELEASE_URL" ]; then
  curl -L -o /opt/ac/bin/aesthetic-computer.AppImage "$RELEASE_URL"
  chmod +x /opt/ac/bin/aesthetic-computer.AppImage
  echo "AC Electron installed: /opt/ac/bin/aesthetic-computer.AppImage"
else
  echo "WARNING: Could not download AC Electron AppImage. Will need manual install."
fi

# Create .desktop autostart entry for Electron kiosk
cat > /home/me/.config/autostart/ac-electron-kiosk.desktop << 'DESKTOPEOF'
[Desktop Entry]
Type=Application
Name=Aesthetic Computer
Comment=Boot to Aesthetic Computer
Exec=/opt/ac/bin/aesthetic-computer.AppImage --no-sandbox --kiosk
Icon=aesthetic-computer
Terminal=false
X-GNOME-Autostart-enabled=true
X-GNOME-Autostart-Delay=2
DESKTOPEOF

# Create systemd user service for AC Electron kiosk
cat > /home/me/.config/systemd/user/ac-electron-kiosk.service << 'SVCEOF'
[Unit]
Description=Aesthetic Computer Electron Kiosk
After=graphical-session.target

[Service]
Type=simple
ExecStart=/opt/ac/bin/aesthetic-computer.AppImage --no-sandbox --kiosk
Restart=on-failure
RestartSec=5
Environment=DISPLAY=:0
Environment=WAYLAND_DISPLAY=wayland-0
Environment=XDG_RUNTIME_DIR=/run/user/1000

[Install]
WantedBy=default.target
SVCEOF

# Install Plymouth theme (pals boot splash)
# The actual theme files need to be on the USB or downloaded
mkdir -p /usr/share/plymouth/themes/fedac
# Placeholder — real files come from fedac/plymouth/
cat > /usr/share/plymouth/themes/fedac/fedac.plymouth << 'PLYMEOF'
[Plymouth Theme]
Name=FedAC
Description=Aesthetic Computer Boot
ModuleName=script

[script]
ImageDir=/usr/share/plymouth/themes/fedac
ScriptFile=/usr/share/plymouth/themes/fedac/fedac.script
PLYMEOF

# Set Plymouth theme
plymouth-set-default-theme fedac 2>/dev/null || true

# Set GNOME kiosk-friendly settings via dconf
mkdir -p /etc/dconf/profile
cat > /etc/dconf/profile/user << 'DCONFEOF'
user-db:user
system-db:local
DCONFEOF

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

dconf update

# Fix ownership
chown -R me:me /home/me
chown -R me:me /opt/ac

%end

# Reboot after install
reboot
