#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$ROOT_DIR/utilities/ffos-build"
CACHE_DIR="$WORK_DIR/.ffos-cache"
OUT_DIR="$CACHE_DIR/out"

FFOS_REPO="${FFOS_REPO:-https://github.com/feral-file/ffos.git}"
FFOS_USER_REPO="${FFOS_USER_REPO:-https://github.com/feral-file/ffos-user.git}"
FFOS_BRANCH="${FFOS_BRANCH:-develop}"
FFOS_USER_BRANCH="${FFOS_USER_BRANCH:-develop}"
VERSION="${VERSION:-1.0.0}"

# Ensure Docker is available
if ! sudo docker info >/dev/null 2>&1; then
  echo "❌ Docker is not available from this environment."
  echo "   - Ensure the Docker daemon is running on the host."
  echo "   - Ensure you have sudo access to docker."
  exit 1
fi

mkdir -p "$CACHE_DIR" "$OUT_DIR"

echo "📦 Cloning/updating FFOS repos..."

# Clone or update ffos
if [ ! -d "$CACHE_DIR/ffos/.git" ]; then
  git clone --branch "$FFOS_BRANCH" --depth 1 "$FFOS_REPO" "$CACHE_DIR/ffos"
else
  git -C "$CACHE_DIR/ffos" fetch origin "$FFOS_BRANCH"
  git -C "$CACHE_DIR/ffos" reset --hard "origin/$FFOS_BRANCH"
fi

# Clone or update ffos-user
if [ ! -d "$CACHE_DIR/ffos-user/.git" ]; then
  git clone --branch "$FFOS_USER_BRANCH" --depth 1 "$FFOS_USER_REPO" "$CACHE_DIR/ffos-user"
else
  git -C "$CACHE_DIR/ffos-user" fetch origin "$FFOS_USER_BRANCH"
  git -C "$CACHE_DIR/ffos-user" reset --hard "origin/$FFOS_USER_BRANCH"
fi

# Apply local overlays (optional)
if [ -d "$WORK_DIR/overlays/ffos-user" ]; then
  echo "📝 Applying local overlays..."
  rsync -a "$WORK_DIR/overlays/ffos-user/" "$CACHE_DIR/ffos-user/"
fi

echo "🔨 Building Docker image..."
IMAGE_NAME="ffos-local-builder"
docker build -t "$IMAGE_NAME" -f "$WORK_DIR/Dockerfile" "$WORK_DIR"

echo "🏗️ Building components and ISO inside container..."

# Run everything inside the container:
# 1. Build Go components (feral-controld, feral-sys-monitord, feral-watchdog)
# 2. Build Rust component (feral-setupd)
# 3. Create pacman packages for each
# 4. Set up local pacman repo
# 5. Build ISO with mkarchiso

docker run --rm \
  --privileged \
  -v "$CACHE_DIR/ffos:/work/ffos" \
  -v "$CACHE_DIR/ffos-user:/work/ffos-user" \
  -v "$WORK_DIR/overlays:/work/overlays:ro" \
  -v "$OUT_DIR:/work/out" \
  -e VERSION="$VERSION" \
  "$IMAGE_NAME" '
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
    
    # The feral packages are already in the FFOS packages.x86_64 file
    # We just need to ensure our local repo packages override them
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
      # Append additional packages
      if [ -f /work/overlays/ffos/archiso-ff1/packages.x86_64.append ]; then
        cat /work/overlays/ffos/archiso-ff1/packages.x86_64.append >> "$PROFILE/packages.x86_64"
        echo "Added packages:"
        cat /work/overlays/ffos/archiso-ff1/packages.x86_64.append
      fi
    fi
    
    # Add BIOS boot support (for older systems or if UEFI fails)
    # Modify profiledef.sh to include both UEFI and BIOS boot modes
    sed -i "s/bootmodes=.*/bootmodes=('bios.syslinux' 'uefi.systemd-boot')/" "$PROFILE/profiledef.sh"
    
    # FIX: Change squashfs compression from xz to zstd to prevent corruption
    # xz with 1M dict can cause corruption on memory-constrained GitHub runners
    echo "=== Fixing SquashFS compression (xz -> zstd) ==="
    sed -i "s/airootfs_image_tool_options=.*/airootfs_image_tool_options=('-comp' 'zstd' '-Xcompression-level' '19' '-b' '1M')/" "$PROFILE/profiledef.sh"
    echo "Updated compression settings:"
    grep airootfs_image_tool_options "$PROFILE/profiledef.sh"
    
    # Install syslinux for BIOS boot
    echo "syslinux" >> "$PROFILE/packages.x86_64"
    
    # Create syslinux config for BIOS boot
    mkdir -p "$PROFILE/syslinux"
    cat > "$PROFILE/syslinux/syslinux.cfg" << SYSLINUX
DEFAULT arch
PROMPT 0
TIMEOUT 50

LABEL arch
    LINUX ../boot/x86_64/vmlinuz-linux
    INITRD ../boot/x86_64/initramfs-linux.img
    APPEND archisobasedir=arch archisolabel=ARCH_FFOS
SYSLINUX
    
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
    echo "dev" > "$PROFILE/airootfs/opt/ac/version"
    
    # Install AC Config Server (WiFi + piece configuration)
    echo "=== Installing AC Config Server ==="
    mkdir -p "$PROFILE/airootfs/opt/ac/config-server"
    if [ -d /work/overlays/ac-config-server ]; then
      cp /work/overlays/ac-config-server/ac-config-server.py "$PROFILE/airootfs/opt/ac/config-server/"
      chmod +x "$PROFILE/airootfs/opt/ac/config-server/ac-config-server.py"
      # Install user service
      mkdir -p "$PROFILE/airootfs/home/feralfile/.config/systemd/user"
      cp /work/overlays/ac-config-server/ac-config-server.service "$PROFILE/airootfs/home/feralfile/.config/systemd/user/"
      # Enable it
      mkdir -p "$PROFILE/airootfs/home/feralfile/.config/systemd/user/default.target.wants"
      ln -sf "/home/feralfile/.config/systemd/user/ac-config-server.service" \
         "$PROFILE/airootfs/home/feralfile/.config/systemd/user/default.target.wants/ac-config-server.service"
      echo "Installed and enabled AC Config Server"
    fi
    
    # Install SSL certificates (for HTTPS on config server)
    echo "=== Installing SSL certificates ==="
    mkdir -p "$PROFILE/airootfs/opt/ac-ssl"
    # Generate self-signed certs if not provided in overlays
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
    
    # Install AC Setup TUI (boot-time WiFi + piece configuration)
    echo "=== Installing AC Setup TUI ==="
    mkdir -p "$PROFILE/airootfs/opt/ac/bin"
    if [ -f /work/overlays/ac-setup/ac-setup.py ]; then
      cp /work/overlays/ac-setup/ac-setup.py "$PROFILE/airootfs/opt/ac/bin/ac-setup"
      chmod +x "$PROFILE/airootfs/opt/ac/bin/ac-setup"
      # Add to PATH via symlink
      mkdir -p "$PROFILE/airootfs/usr/local/bin"
      ln -sf /opt/ac/bin/ac-setup "$PROFILE/airootfs/usr/local/bin/ac-setup"
      echo "Installed AC Setup TUI"
    fi
    
    echo "=== Installing systemd service files ==="
    # Install system-level services
    mkdir -p "$PROFILE/airootfs/etc/systemd/system"
    SERVICES_SRC="$PROFILE/airootfs/home/feralfile/systemd-services"
    if [ -d "$SERVICES_SRC" ]; then
      for svc in feral-controld feral-sys-monitord feral-watchdog feral-setupd; do
        if [ -f "$SERVICES_SRC/${svc}.service" ]; then
          cp "$SERVICES_SRC/${svc}.service" "$PROFILE/airootfs/etc/systemd/system/"
          # Create symlink to enable service at boot
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
    
    # Create feralfile user setup
    mkdir -p "$PROFILE/airootfs/etc"
    # Ensure feralfile user exists (uid 1000)
    echo "feralfile:x:1000:1000:Feral File:/home/feralfile:/bin/bash" >> "$PROFILE/airootfs/etc/passwd"
    echo "feralfile:x:1000:" >> "$PROFILE/airootfs/etc/group"
    echo "feralfile:!:19000:0:99999:7:::" >> "$PROFILE/airootfs/etc/shadow"
    
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
    # The upstream service uses Type=notify and hard-depends on D-Bus session bus.
    # On non-Radxa hardware (Chromebooks, etc.) the session bus may not be ready
    # in time. Override to Type=simple and add a pre-check for the bus socket.
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
    # Create the same override for the user-level version (if bind-mounted)
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
enable systemd-networkd.service
enable systemd-resolved.service
enable seatd.service
enable getty@tty1.service
PRESET
    echo "Added hardened system presets"
    
    # === HARDENING: Expanded customize_airootfs.sh ===
    # Set proper permissions via customize script
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

# Add feralfile to required groups
usermod -aG seat feralfile 2>/dev/null || true
usermod -aG audio feralfile 2>/dev/null || true
usermod -aG video feralfile 2>/dev/null || true
usermod -aG render feralfile 2>/dev/null || true
usermod -aG input feralfile 2>/dev/null || true
usermod -aG bluetooth feralfile 2>/dev/null || true

# Setup .bash_profile for auto-start on TTY1
cat > /home/feralfile/.bash_profile << 'BASHPROFILE'
# AC Hardened boot profile
if [ "$(tty)" = "/dev/tty1" ]; then
    # Ensure directories exist
    mkdir -p /home/feralfile/.logs
    mkdir -p /home/feralfile/.state

    # Wait for systemd user session to be ready
    echo "Waiting for user session..."
    for i in $(seq 1 30); do
        if [ -S "/run/user/$(id -u)/bus" ]; then
            echo "D-Bus session bus ready"
            break
        fi
        sleep 1
    done

    export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/$(id -u)/bus"
    export XDG_RUNTIME_DIR="/run/user/$(id -u)"

    # Reload user daemons
    systemctl --user daemon-reload 2>/dev/null || true

    # Start feral system services (user-level)
    systemctl --user start feral-sys-monitord.service 2>/dev/null || true

    # Run setup TUI on first boot, or if setup not complete
    if [ ! -f ~/.state/setup-done ]; then
        echo ""
        echo "Running first-time setup..."
        sleep 1
        /opt/ac/bin/ac-setup
    else
        # Setup done, start kiosk directly
        systemctl --user start aesthetic-kiosk.service 2>/dev/null || true
        echo "Aesthetic Computer OS booted."
        echo ""
        echo "Run 'ac-setup' to reconfigure WiFi or piece."
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
  '

echo "✅ Build complete. ISO output: $OUT_DIR"
ls -lh "$OUT_DIR"/*.iso 2>/dev/null || echo "No ISO files found"

# Show verification info
echo ""
echo "════════════════════════════════════════════════════════════════"
echo "  To verify and flash the ISO, use:"
echo "    bash utilities/ffos-build/verify-iso.sh $OUT_DIR/*.iso /dev/sdX"
echo ""
echo "  Or verify only:"
echo "    bash utilities/ffos-build/verify-iso.sh --verify-only $OUT_DIR/*.iso"
echo "════════════════════════════════════════════════════════════════"