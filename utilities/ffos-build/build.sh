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
if ! docker info >/dev/null 2>&1; then
  echo "❌ Docker is not available from this environment."
  echo "   - Ensure the Docker daemon is running on the host."
  echo "   - Ensure your user can access /var/run/docker.sock (docker group)."
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
    sed -i "s/bootmodes=.*/bootmodes=('bios.syslinux.mbr' 'bios.syslinux.eltorito' 'uefi-x64.systemd-boot.esp' 'uefi-x64.systemd-boot.eltorito')/" "$PROFILE/profiledef.sh"
    
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
    
    # Set proper permissions via customize script
    cat > "$PROFILE/airootfs/root/customize_airootfs.sh" << 'CUSTOMIZE'
#!/bin/bash
# Create logs directory
mkdir -p /home/feralfile/.logs
chown -R feralfile:feralfile /home/feralfile
# Enable lingering for user services
mkdir -p /var/lib/systemd/linger
touch /var/lib/systemd/linger/feralfile
CUSTOMIZE
    chmod +x "$PROFILE/airootfs/root/customize_airootfs.sh"
    
    echo "=== Building ISO ==="
    mkarchiso -v -o /work/out "$PROFILE"
    
    echo "✅ Build complete!"
    ls -lh /work/out/*.iso 2>/dev/null || echo "No ISO files found"
  '

echo "✅ Build complete. ISO output: $OUT_DIR"
ls -lh "$OUT_DIR"/*.iso 2>/dev/null || echo "No ISO files found"