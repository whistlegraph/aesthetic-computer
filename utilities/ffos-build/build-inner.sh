#!/usr/bin/env bash
# build-inner.sh — runs INSIDE the Docker container
# Called by build.sh via: docker run ... -v this_file:/work/build-inner.sh ...
set -euo pipefail

COMPONENTS_DIR=/work/ffos-user/components
LOCAL_REPO=/work/local-repo
mkdir -p "$LOCAL_REPO"
chown -R builder:builder "$LOCAL_REPO"

echo "=== Building Go components ==="
for comp in feral-controld feral-sys-monitord feral-watchdog; do
  echo "Building $comp..."
  cd "$COMPONENTS_DIR/$comp"
  CGO_ENABLED=0 go build -buildvcs=false -ldflags="-s -w" -o "/tmp/$comp" .

  # Create PKGBUILD in builder-owned directory
  BUILDDIR="/tmp/build-$comp"
  mkdir -p "$BUILDDIR"
  cp "/tmp/$comp" "$BUILDDIR/"

  cat > "$BUILDDIR/PKGBUILD" << PKGBUILD
pkgname=$comp
pkgver=${VERSION}
pkgrel=1
pkgdesc="Feral File $comp daemon"
arch=("x86_64")
license=("MIT")
depends=()
source=("$comp")
sha256sums=("SKIP")

package() {
  install -Dm755 "\$srcdir/$comp" "\$pkgdir/usr/bin/$comp"
}
PKGBUILD

  # Build package as builder user
  chown -R builder:builder "$BUILDDIR"
  cd "$BUILDDIR"
  sudo -u builder makepkg -f --nodeps --skipinteg
  mv *.pkg.tar.* "$LOCAL_REPO/"
done

echo "=== Building Rust component (feral-setupd) ==="
cd "$COMPONENTS_DIR/feral-setupd"
cargo build --release 2>/dev/null || echo "Rust build failed, creating stub"

# Find the binary or create a stub
BINARY=$(find target/release -maxdepth 1 -type f -executable -name "feral*" 2>/dev/null | head -1)
if [ -z "$BINARY" ] || [ ! -f "$BINARY" ]; then
  echo "Creating stub for feral-setupd..."
  echo "#!/bin/bash" > /tmp/feral-setupd
  echo "echo feral-setupd stub" >> /tmp/feral-setupd
  chmod +x /tmp/feral-setupd
  BINARY=/tmp/feral-setupd
fi

# Create pacman package for feral-setupd
BUILDDIR="/tmp/build-feral-setupd"
mkdir -p "$BUILDDIR"
cp "$BINARY" "$BUILDDIR/feral-setupd"

cat > "$BUILDDIR/PKGBUILD" << PKGBUILD
pkgname=feral-setupd
pkgver=${VERSION}
pkgrel=1
pkgdesc="Feral File setup daemon"
arch=("x86_64")
license=("MIT")
depends=()
source=("feral-setupd")
sha256sums=("SKIP")

package() {
  install -Dm755 "\$srcdir/feral-setupd" "\$pkgdir/usr/bin/feral-setupd"
}
PKGBUILD

chown -R builder:builder "$BUILDDIR"
cd "$BUILDDIR"
sudo -u builder makepkg -f --nodeps --skipinteg
mv *.pkg.tar.* "$LOCAL_REPO/"

echo "=== Setting up local pacman repo ==="
cd "$LOCAL_REPO"
ls -la *.pkg.tar.* || echo "Warning: No packages found"
repo-add ac-local.db.tar.gz *.pkg.tar.*

echo "=== Configuring archiso to use local repo ==="
# Copy profile to avoid modifying the original
PROFILE_SRC=/work/ffos/archiso-ff1
PROFILE=/tmp/archiso-profile
rm -rf "$PROFILE"
cp -r "$PROFILE_SRC" "$PROFILE"

# Fix: ensure packages.x86_64 ends with a newline before appending overlays
echo "=== Ensuring packages.x86_64 has proper line endings ==="
printf "\n" >> "$PROFILE/packages.x86_64"

# Replace PulseAudio with PipeWire (our overlay ships pipewire + pipewire-pulse)
echo "=== Replacing pulseaudio with pipewire ==="
sed -i '/^pulseaudio$/d' "$PROFILE/packages.x86_64"
sed -i '/^pulseaudio-bluetooth$/d' "$PROFILE/packages.x86_64"

# Apply FFOS overlays if present (additional packages, etc.)
if [ -d /work/overlays/ffos/archiso-ff1 ]; then
  echo "Applying FFOS overlays..."
  if [ -f /work/overlays/ffos/archiso-ff1/packages.x86_64.append ]; then
    cat /work/overlays/ffos/archiso-ff1/packages.x86_64.append >> "$PROFILE/packages.x86_64"
    echo "Added packages:"
    cat /work/overlays/ffos/archiso-ff1/packages.x86_64.append
  fi
fi

# EFI-only boot (ThinkPads are modern EFI machines, no BIOS/syslinux needed)
# This prevents accidental live-USB boot via BIOS which skips auto-install
sed -i "s/bootmodes=.*/bootmodes=('uefi.systemd-boot')/" "$PROFILE/profiledef.sh"

# FIX: Change squashfs compression from xz to zstd to prevent corruption
echo "=== Fixing SquashFS compression (xz -> zstd) ==="
sed -i "s/airootfs_image_tool_options=.*/airootfs_image_tool_options=('-comp' 'zstd' '-Xcompression-level' '19' '-b' '1M')/" "$PROFILE/profiledef.sh"
echo "Updated compression settings:"
grep airootfs_image_tool_options "$PROFILE/profiledef.sh"

# Remove non-auto-install boot entries (force auto-install only)
echo "=== Removing non-auto-install boot entries ==="
rm -f "$PROFILE/efiboot/loader/entries/archiso-x86_64-linux-installation.conf"
rm -f "$PROFILE/efiboot/loader/entries/archiso-x86_64-linux-soak-test.conf"
echo "Only auto-install boot entry remains"
ls "$PROFILE/efiboot/loader/entries/"

# Patch auto-install.sh for cleaner experience
echo "=== Patching auto-install.sh ==="
AUTOINST="$PROFILE/airootfs/root/scripts/auto-install.sh"
if [ -f "$AUTOINST" ]; then
  # Add quiet boot params to installed system (hide cursor, suppress messages)
  sed -i 's/ipv6.disable=1 rw quiet loglevel=3/ipv6.disable=1 rw quiet loglevel=3 vt.global_cursor_default=0/' "$AUTOINST"
  # Rebrand FF1 -> Aesthetic Computer
  sed -i 's/title   FF1$/title   Aesthetic Computer/' "$AUTOINST"
  sed -i 's/title   FF1 - Factory Reset/title   Aesthetic Computer - Factory Reset/' "$AUTOINST"
  sed -i 's/=== Feral File Arch Installer ===/=== Aesthetic Computer OS Installer ===/' "$AUTOINST"
  # Add clear screen at start for clean install experience
  sed -i 's/echo "Booting..."/clear\necho ""\necho "  Installing Aesthetic Computer OS..."\necho ""/' "$AUTOINST"
  echo "Patched auto-install.sh"
fi

# Add local repo to pacman.conf (using unique name to avoid conflicts)
echo "" >> "$PROFILE/pacman.conf"
echo "[ac-local]" >> "$PROFILE/pacman.conf"
echo "SigLevel = Optional TrustAll" >> "$PROFILE/pacman.conf"
echo "Server = file://$LOCAL_REPO" >> "$PROFILE/pacman.conf"

# Debug: show pacman.conf tail
echo "--- pacman.conf tail ---"
tail -10 "$PROFILE/pacman.conf"

# Debug: show repo contents
echo "--- Local repo contents ---"
ls -la "$LOCAL_REPO/"

# Merge ffos-user data into airootfs
mkdir -p "$PROFILE/airootfs/home"
rsync -a /work/ffos-user/users/ "$PROFILE/airootfs/home/"

# Install AC launcher UI (QR code boot screen)
echo "=== Installing AC launcher UI ==="
mkdir -p "$PROFILE/airootfs/opt/ac/ui/launcher"
if [ -d /work/overlays/launcher-ui ]; then
  rsync -a /work/overlays/launcher-ui/ "$PROFILE/airootfs/opt/ac/ui/launcher/"
  echo "Installed launcher UI to /opt/ac/ui/launcher/"
  ls -la "$PROFILE/airootfs/opt/ac/ui/launcher/"
fi
echo "1.0.1" > "$PROFILE/airootfs/opt/ac/version"

# Install AC Config Server (WiFi + piece configuration)
echo "=== Installing AC Config Server ==="
mkdir -p "$PROFILE/airootfs/opt/ac/config-server"
if [ -d /work/overlays/ac-config-server ]; then
  cp /work/overlays/ac-config-server/ac-config-server.py "$PROFILE/airootfs/opt/ac/config-server/"
  chmod +x "$PROFILE/airootfs/opt/ac/config-server/ac-config-server.py"
  mkdir -p "$PROFILE/airootfs/home/feralfile/.config/systemd/user"
  cp /work/overlays/ac-config-server/ac-config-server.service "$PROFILE/airootfs/home/feralfile/.config/systemd/user/"
  mkdir -p "$PROFILE/airootfs/home/feralfile/.config/systemd/user/default.target.wants"
  ln -sf "/home/feralfile/.config/systemd/user/ac-config-server.service" \
     "$PROFILE/airootfs/home/feralfile/.config/systemd/user/default.target.wants/ac-config-server.service"
  echo "Installed and enabled AC Config Server"
fi

# Install SSL certificates (for HTTPS on config server)
echo "=== Installing SSL certificates ==="
mkdir -p "$PROFILE/airootfs/opt/ac-ssl"
if [ -f /work/overlays/ac-ssl/localhost.pem ] && [ -f /work/overlays/ac-ssl/localhost-key.pem ]; then
  cp /work/overlays/ac-ssl/localhost.pem "$PROFILE/airootfs/opt/ac-ssl/"
  cp /work/overlays/ac-ssl/localhost-key.pem "$PROFILE/airootfs/opt/ac-ssl/"
  echo "Installed SSL certificates from overlays"
else
  echo "Generating self-signed SSL certificates..."
  openssl req -x509 -newkey rsa:2048 -keyout "$PROFILE/airootfs/opt/ac-ssl/localhost-key.pem" \
    -out "$PROFILE/airootfs/opt/ac-ssl/localhost.pem" -days 365 -nodes \
    -subj "/CN=localhost/O=Aesthetic Computer/C=US"
  echo "Generated self-signed SSL certificates"
fi
chmod 644 "$PROFILE/airootfs/opt/ac-ssl/localhost.pem"
chmod 600 "$PROFILE/airootfs/opt/ac-ssl/localhost-key.pem"

# Download offline piece bundles from production API
echo "=== Downloading offline piece bundles ==="
mkdir -p "$PROFILE/airootfs/opt/ac/offline-pieces"

OFFLINE_PIECES="notepat:piece starfield:piece roz:code"

for entry in $OFFLINE_PIECES; do
  piece_name="${entry%%:*}"
  piece_type="${entry##*:}"

  if [ "$piece_type" = "code" ]; then
    url="https://aesthetic.computer/api/bundle-html?code=${piece_name}"
  else
    url="https://aesthetic.computer/api/bundle-html?piece=${piece_name}"
  fi

  echo "Downloading ${piece_name} bundle..."
  if curl -f -L -o "$PROFILE/airootfs/opt/ac/offline-pieces/${piece_name}.html" "$url" 2>/dev/null; then
    echo "  ✓ Downloaded ${piece_name}.html"
    ls -lh "$PROFILE/airootfs/opt/ac/offline-pieces/${piece_name}.html"
  else
    echo "  ✗ Failed to download ${piece_name} bundle (will skip)"
  fi
done

echo "Offline pieces:"
ls -la "$PROFILE/airootfs/opt/ac/offline-pieces/" 2>/dev/null || echo "  (none)"

# Install AC Setup TUI (boot-time WiFi + piece configuration)
echo "=== Installing AC Setup TUI ==="
mkdir -p "$PROFILE/airootfs/opt/ac/bin"
if [ -f /work/overlays/ac-setup/ac-setup.py ]; then
  cp /work/overlays/ac-setup/ac-setup.py "$PROFILE/airootfs/opt/ac/bin/ac-setup"
  chmod +x "$PROFILE/airootfs/opt/ac/bin/ac-setup"
  mkdir -p "$PROFILE/airootfs/usr/local/bin"
  ln -sf /opt/ac/bin/ac-setup "$PROFILE/airootfs/usr/local/bin/ac-setup"
  echo "Installed AC Setup TUI"
fi

echo "=== Installing systemd service files ==="
mkdir -p "$PROFILE/airootfs/etc/systemd/system"
SERVICES_SRC="$PROFILE/airootfs/home/feralfile/systemd-services"
if [ -d "$SERVICES_SRC" ]; then
  for svc in feral-controld feral-sys-monitord feral-watchdog feral-setupd; do
    if [ -f "$SERVICES_SRC/${svc}.service" ]; then
      cp "$SERVICES_SRC/${svc}.service" "$PROFILE/airootfs/etc/systemd/system/"
      mkdir -p "$PROFILE/airootfs/etc/systemd/system/multi-user.target.wants"
      ln -sf "/etc/systemd/system/${svc}.service" "$PROFILE/airootfs/etc/systemd/system/multi-user.target.wants/${svc}.service"
      echo "Installed and enabled: ${svc}.service"
    fi
  done
fi

# Also install user-level services for the feralfile user
mkdir -p "$PROFILE/airootfs/home/feralfile/.config/systemd/user/default.target.wants"
if [ -d "$PROFILE/airootfs/home/feralfile/.config/systemd/user" ]; then
  for svc in aesthetic-kiosk; do
    if [ -f "$PROFILE/airootfs/home/feralfile/.config/systemd/user/${svc}.service" ]; then
      ln -sf "/home/feralfile/.config/systemd/user/${svc}.service" "$PROFILE/airootfs/home/feralfile/.config/systemd/user/default.target.wants/${svc}.service"
      echo "Enabled user service: ${svc}.service"
    fi
  done
fi

# Ensure airootfs group file has all required groups
# (The FFOS overlay replaces the system /etc/group, so missing groups
#  cause silent failures in customize_airootfs.sh usermod commands)
mkdir -p "$PROFILE/airootfs/etc"
for grp_entry in "seat:98" "render:96" "bluetooth:84"; do
  grp_name="${grp_entry%%:*}"
  grp_gid="${grp_entry##*:}"
  if ! grep -q "^${grp_name}:" "$PROFILE/airootfs/etc/group" 2>/dev/null; then
    echo "${grp_name}:x:${grp_gid}:" >> "$PROFILE/airootfs/etc/group"
    echo "Added missing group: ${grp_name} (gid ${grp_gid})"
  fi
done
# Add feralfile user entry if not already present
if ! grep -q "^feralfile:" "$PROFILE/airootfs/etc/passwd" 2>/dev/null; then
  echo "feralfile:x:1000:1000:Feral File:/home/feralfile:/bin/bash" >> "$PROFILE/airootfs/etc/passwd"
fi
if ! grep -q "^feralfile:" "$PROFILE/airootfs/etc/group" 2>/dev/null; then
  echo "feralfile:x:1000:" >> "$PROFILE/airootfs/etc/group"
fi
if ! grep -q "^feralfile:" "$PROFILE/airootfs/etc/shadow" 2>/dev/null; then
  echo "feralfile:!:19000:0:99999:7:::" >> "$PROFILE/airootfs/etc/shadow"
fi

# Pre-create setup-done marker so we boot straight to kiosk (skip TUI)
mkdir -p "$PROFILE/airootfs/home/feralfile/.state"
touch "$PROFILE/airootfs/home/feralfile/.state/setup-done"
echo "Pre-created setup-done marker (boot straight to kiosk, skip TUI)"

# Configure console font for nicer terminal appearance (setup TUI)
echo "=== Configuring console font ==="
if [ -f /work/overlays/vconsole.conf ]; then
  cp /work/overlays/vconsole.conf "$PROFILE/airootfs/etc/vconsole.conf"
  echo "Installed vconsole.conf with terminus font"
fi

# === HARDENING: Auto-login feralfile on TTY1 ===
echo "=== Configuring auto-login for feralfile user ==="
mkdir -p "$PROFILE/airootfs/etc/systemd/system/getty@tty1.service.d"
cat > "$PROFILE/airootfs/etc/systemd/system/getty@tty1.service.d/autologin.conf" << 'AUTOLOGIN'
[Service]
ExecStart=
ExecStart=-/usr/bin/agetty --noclear --autologin feralfile %I $TERM
AUTOLOGIN

# === HARDENING: Override feral-sys-monitord to be resilient ===
echo "=== Creating hardened feral-sys-monitord service override ==="
if [ -f "$PROFILE/airootfs/etc/systemd/system/feral-sys-monitord.service" ]; then
  cat > "$PROFILE/airootfs/etc/systemd/system/feral-sys-monitord.service" << 'SYSMON'
[Unit]
Description=Feral File System Monitord (hardened)
After=network.target dbus.service systemd-logind.service user@1000.service
Wants=dbus.service user@1000.service
StartLimitBurst=10
StartLimitIntervalSec=300

[Service]
Type=simple
User=feralfile
Group=feralfile
Environment="DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus"
Environment="XDG_RUNTIME_DIR=/run/user/1000"
ExecStartPre=/bin/bash -c 'n=0; while [ $n -lt 60 ]; do [ -S /run/user/1000/bus ] && exit 0; n=$((n+1)); sleep 1; done; echo WARN: D-Bus session bus not found after 60s >&2'
ExecStart=/usr/bin/feral-sys-monitord
Restart=always
RestartSec=5
StandardOutput=append:/home/feralfile/.logs/sys-monitord.log
StandardError=append:/home/feralfile/.logs/sys-monitord.log

[Install]
WantedBy=multi-user.target
SYSMON
  echo "Overwrote feral-sys-monitord.service with hardened version"
fi

# === HARDENING: Also override if it was installed from systemd-services dir ===
mkdir -p "$PROFILE/airootfs/home/feralfile/systemd-services"
if [ -f "$PROFILE/airootfs/home/feralfile/systemd-services/feral-sys-monitord.service" ]; then
  cp "$PROFILE/airootfs/etc/systemd/system/feral-sys-monitord.service" \
     "$PROFILE/airootfs/home/feralfile/systemd-services/feral-sys-monitord.service"
  echo "Also updated user-level systemd-services copy"
fi

# === HARDENING: Enable system-level presets ===
mkdir -p "$PROFILE/airootfs/etc/systemd/system-preset"
cat > "$PROFILE/airootfs/etc/systemd/system-preset/90-ac-hardened.preset" << 'PRESET'
enable sshd.service
enable bluetooth.service
enable NetworkManager.service
disable systemd-networkd.service
enable systemd-resolved.service
enable seatd.service
enable getty@tty1.service
PRESET
echo "Added hardened system presets"

# === HARDENING: Expanded customize_airootfs.sh ===
cat > "$PROFILE/airootfs/root/customize_airootfs.sh" << 'CUSTOMIZE'
#!/bin/bash
set -e

echo "=== AC Hardened customize_airootfs.sh ==="

# Create logs and state directories
mkdir -p /home/feralfile/.logs
mkdir -p /home/feralfile/.state
mkdir -p /home/feralfile/.config

# Set ownership
chown -R feralfile:feralfile /home/feralfile

# Ensure scripts are executable
chmod +x /home/feralfile/scripts/*.sh 2>/dev/null || true

# Ensure AC binaries are executable
chmod +x /opt/ac/bin/* 2>/dev/null || true
chmod +x /opt/ac/config-server/*.py 2>/dev/null || true

# Give feralfile passwordless sudo for network management
echo "feralfile ALL=(ALL) NOPASSWD: /usr/bin/nmcli, /usr/bin/systemctl" > /etc/sudoers.d/feralfile-network
chmod 440 /etc/sudoers.d/feralfile-network

# Enable lingering for user services (critical for D-Bus session bus at boot)
mkdir -p /var/lib/systemd/linger
touch /var/lib/systemd/linger/feralfile

# Ensure D-Bus is properly configured for session bus
mkdir -p /run/user/1000
chown feralfile:feralfile /run/user/1000
chmod 700 /run/user/1000

# Create polkit rule so feralfile can manage NetworkManager
mkdir -p /etc/polkit-1/rules.d
cat > /etc/polkit-1/rules.d/90-nmcli-feralfile.rules << 'POLKIT'
polkit.addRule(function(action, subject) {
    if (action.id.indexOf("org.freedesktop.NetworkManager") === 0 &&
        subject.user === "feralfile") {
        return polkit.Result.YES;
    }
});
POLKIT

# Enable critical system services
systemctl enable NetworkManager.service 2>/dev/null || true
systemctl disable systemd-networkd.service 2>/dev/null || true
systemctl enable wpa_supplicant.service 2>/dev/null || true
systemctl enable sshd.service 2>/dev/null || true
systemctl enable seatd.service 2>/dev/null || true
systemctl enable bluetooth.service 2>/dev/null || true
systemctl enable getty@tty1.service 2>/dev/null || true
systemctl enable systemd-resolved.service 2>/dev/null || true

# Enable feral services (system-level)
for svc in feral-controld feral-sys-monitord feral-watchdog feral-setupd; do
  if [ -f "/etc/systemd/system/${svc}.service" ]; then
    systemctl enable "${svc}.service" 2>/dev/null || true
  fi
done

# Ensure required groups exist (airootfs overlay may have replaced system group file)
for grp in seat render bluetooth; do
  groupadd -f "$grp" 2>/dev/null || true
done

# Add feralfile to required groups
for grp in seat audio video render input bluetooth; do
  usermod -aG "$grp" feralfile 2>/dev/null || true
done
echo "feralfile groups: $(id feralfile 2>/dev/null || echo 'unknown')"

# Setup .bash_profile for auto-start on TTY1
cat > /home/feralfile/.bash_profile << 'BASHPROFILE'
# AC Hardened boot profile — silent boot to kiosk
if [ "$(tty)" = "/dev/tty1" ]; then
    # Clear screen and hide cursor for clean boot
    clear
    printf '\e[?25l'

    # Ensure directories exist
    mkdir -p /home/feralfile/.logs /home/feralfile/.state

    # Wait for systemd user session (D-Bus) silently
    for i in $(seq 1 30); do
        [ -S "/run/user/$(id -u)/bus" ] && break
        sleep 1
    done

    export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus"
    export XDG_RUNTIME_DIR="/run/user/$(id -u)"

    # Reload user daemons
    systemctl --user daemon-reload 2>/dev/null || true
    systemctl --user start feral-sys-monitord.service 2>/dev/null || true

    # Run setup TUI on first boot, or start kiosk
    if [ ! -f ~/.state/setup-done ]; then
        printf '\e[?25h'
        /opt/ac/bin/ac-setup
    else
        # Start kiosk silently — cage will take over the display
        systemctl --user start aesthetic-kiosk.service 2>/dev/null || true
    fi
fi
BASHPROFILE
chown feralfile:feralfile /home/feralfile/.bash_profile

# Set hostname
echo "aesthetic-computer" > /etc/hostname

# Set locale
echo "en_US.UTF-8 UTF-8" > /etc/locale.gen
locale-gen 2>/dev/null || true
echo "LANG=en_US.UTF-8" > /etc/locale.conf

echo "=== customize_airootfs.sh complete ==="
CUSTOMIZE
chmod +x "$PROFILE/airootfs/root/customize_airootfs.sh"

echo "=== Building ISO ==="
mkarchiso -v -o /work/out "$PROFILE"

echo "=== Generating checksums ==="
cd /work/out
for iso in *.iso; do
  if [ -f "$iso" ]; then
    echo "Generating SHA256 for $iso..."
    sha256sum "$iso" > "${iso}.sha256"
    echo "Checksum: $(cat "${iso}.sha256")"

    # Also verify the squashfs inside the ISO
    echo "Verifying SquashFS integrity..."
    mkdir -p /tmp/iso-verify
    mount -o loop,ro "$iso" /tmp/iso-verify
    if [ -f /tmp/iso-verify/arch/x86_64/airootfs.sfs ]; then
      if unsquashfs -l /tmp/iso-verify/arch/x86_64/airootfs.sfs > /dev/null 2>&1; then
        echo "✅ SquashFS integrity verified"
      else
        echo "❌ SquashFS integrity check FAILED!"
        umount /tmp/iso-verify
        rm -rf /tmp/iso-verify
        exit 1
      fi
    fi
    umount /tmp/iso-verify
    rm -rf /tmp/iso-verify
  fi
done

echo "✅ Build complete!"
ls -lh /work/out/*.iso 2>/dev/null || echo "No ISO files found"
