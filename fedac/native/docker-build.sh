#!/bin/bash
# AC Native OS — Minimal reproducible build
# Builds: C binary → initramfs → kernel → vmlinuz
# Input: /src (repo), Output: /out/vmlinuz
set -euo pipefail

SRC="${AC_SRC:-/repo}"
OUT="${AC_OUT:-/out}"
NATIVE="$SRC/fedac/native"
BUILD="$NATIVE/build"
KVER="${KERNEL_VERSION:-6.19.9}"
KMAJOR="${KVER%%.*}"

log()  { echo -e "\033[0;36m[ac-os]\033[0m $*"; }
err()  { echo -e "\033[0;31m[ac-os]\033[0m $*" >&2; }

# Always build in /tmp inside the container to avoid bind-mount permission issues
BUILD="/tmp/ac-build"
mkdir -p "$BUILD" "$OUT"

# ── Git info ──
GIT_HASH="${AC_GIT_HASH:-$(cd "$SRC" 2>/dev/null && git rev-parse --short HEAD 2>/dev/null || echo unknown)}"
BUILD_TS="${AC_BUILD_TS:-$(date -u '+%Y-%m-%dT%H:%M')}"
BUILD_NAME="${AC_BUILD_NAME:-docker-build}"
HANDLE="${AC_HANDLE:-jeffrey}"

log "Building $BUILD_NAME ($GIT_HASH)"

# ══════════════════════════════════════════════
# Step 1: Compile ac-native binary
# ══════════════════════════════════════════════
log "Step 1/4: Compiling ac-native..."
cd "$NATIVE"

# Use cached QuickJS (from Docker image) or download
if [ ! -f "$BUILD/quickjs/quickjs.h" ]; then
    if [ -d /cache/quickjs ]; then
        log "  Using cached QuickJS..."
        cp -a /cache/quickjs-2024-01-13 "$BUILD/"
        ln -sf quickjs-2024-01-13 "$BUILD/quickjs"
    else
        log "  Downloading QuickJS..."
        cd "$BUILD"
        curl -sL https://bellard.org/quickjs/quickjs-2024-01-13.tar.xz | tar xJ
        ln -sf quickjs-2024-01-13 quickjs
    fi
    cd "$NATIVE"
fi

make -j$(nproc) CC=gcc BUILDDIR="$BUILD" \
    BUILD_TS="$BUILD_TS" GIT_HASH="$GIT_HASH" BUILD_NAME="$BUILD_NAME" \
    2>&1 | tee "$BUILD/.make.log" | tail -5

[ -f "$BUILD/ac-native" ] || { err "Binary compilation failed"; tail -30 "$BUILD/.make.log"; exit 1; }
log "  Binary: $(stat -c%s "$BUILD/ac-native") bytes"

# ══════════════════════════════════════════════
# Step 2: Build initramfs from scratch
# ══════════════════════════════════════════════
log "Step 2/4: Building initramfs..."
IROOT="$BUILD/initramfs-root"
rm -rf "$IROOT"
mkdir -p "$IROOT"/{bin,lib64,lib/firmware/i915,dev,proc,sys,tmp,run,etc,etc/pki/tls/certs}

# ── 2a: Static busybox (no shared lib deps for shell) ──
BUSYBOX=$(command -v busybox)
cp "$BUSYBOX" "$IROOT/bin/busybox"
for cmd in sh sleep mkdir mount umount cat echo ls cp mv rm ln chmod chown \
    date dd find grep head kill ps sed sort tail tee test touch tr wc which \
    mktemp printf seq stat basename dirname env expr true false readlink \
    realpath rmdir uniq yes tar gzip gunzip hostname id ip modprobe \
    mkswap swapon vi df du diff xargs nohup pgrep killall cut whoami; do
    ln -s busybox "$IROOT/bin/$cmd"
done

# ── 2b: Init script ──
cp "$NATIVE/initramfs/init" "$IROOT/init"
chmod +x "$IROOT/init"

# ── 2c: ac-native binary ──
cp "$BUILD/ac-native" "$IROOT/ac-native"

# ── 2d: Pieces ──
cp "$NATIVE/pieces/prompt.mjs" "$IROOT/piece.mjs"
mkdir -p "$IROOT/pieces"
cp "$NATIVE/pieces/"*.mjs "$IROOT/pieces/" 2>/dev/null || true
log "  Pieces: $(ls "$IROOT/pieces/" | wc -l)"

# ── 2e: /dev nodes (needed before devtmpfs mounts) ──
mknod "$IROOT/dev/console" c 5 1 2>/dev/null || true
mknod "$IROOT/dev/null" c 1 3 2>/dev/null || true
mknod "$IROOT/dev/tty" c 5 0 2>/dev/null || true
mknod "$IROOT/dev/zero" c 1 5 2>/dev/null || true

# ── 2f: Dynamic linker ──
cp -L /lib64/ld-linux-x86-64.so.2 "$IROOT/lib64/"

# ── 2g: Shared libraries (FOLLOW symlinks with cp -L) ──
# Direct deps of ac-native
log "  Copying shared libraries..."
for lib in $(ldd "$BUILD/ac-native" 2>/dev/null | grep -oP '/\S+'); do
    BASENAME=$(basename "$lib")
    REAL=$(readlink -f "$lib")
    [ -f "$REAL" ] && cp -L "$REAL" "$IROOT/lib64/$BASENAME"
done

# Mesa/GPU libs (ac-native dlopen's these)
for lib in libgbm.so.1 libEGL.so.1 libEGL_mesa.so.0 libGLESv2.so.2 \
    libGL.so.1 libGLX_mesa.so.0 libGLdispatch.so.0 libglapi.so.0 \
    libdrm_intel.so.1 libdrm_amdgpu.so.1; do
    REAL=$(readlink -f "/lib64/$lib" 2>/dev/null)
    [ -f "$REAL" ] && cp -L "$REAL" "$IROOT/lib64/$lib"
done

# Mesa DRI drivers
if [ -d /lib64/dri ]; then
    mkdir -p "$IROOT/lib64/dri"
    for drv in /lib64/dri/i915_dri.so /lib64/dri/iris_dri.so /lib64/dri/kms_swrast_dri.so /lib64/dri/swrast_dri.so; do
        [ -f "$drv" ] && cp -L "$drv" "$IROOT/lib64/dri/"
    done
fi
# Gallium megadriver
GALLIUM=$(readlink -f /lib64/libgallium-*.so 2>/dev/null || true)
[ -f "$GALLIUM" ] && cp -L "$GALLIUM" "$IROOT/lib64/"

# Transitive deps — resolve everything in lib64
log "  Resolving transitive dependencies..."
ADDED=1
while [ "$ADDED" -gt 0 ]; do
    ADDED=0
    for elf in "$IROOT/lib64/"*.so* "$IROOT/ac-native"; do
        [ -f "$elf" ] || continue
        for needed in $(readelf -d "$elf" 2>/dev/null | grep NEEDED | sed 's/.*\[\(.*\)\]/\1/'); do
            if [ ! -f "$IROOT/lib64/$needed" ]; then
                REAL=$(readlink -f "/lib64/$needed" 2>/dev/null)
                if [ -f "$REAL" ]; then
                    cp -L "$REAL" "$IROOT/lib64/$needed"
                    ADDED=$((ADDED + 1))
                fi
            fi
        done
    done
done

# ── 2h: Verify NO broken symlinks ──
BROKEN=$(find "$IROOT/lib64/" -type l ! -exec test -e {} \; -print 2>/dev/null | wc -l)
if [ "$BROKEN" -gt 0 ]; then
    err "  WARNING: $BROKEN broken symlinks found, fixing..."
    for broken in $(find "$IROOT/lib64/" -type l ! -exec test -e {} \; -print 2>/dev/null); do
        name=$(basename "$broken")
        real=$(readlink -f "/lib64/$name" 2>/dev/null || readlink -f "/usr/lib64/$name" 2>/dev/null)
        if [ -f "$real" ]; then
            rm -f "$broken"
            cp "$real" "$broken"
        else
            rm -f "$broken"
            log "  Removed unfixable: $name"
        fi
    done
fi

LIB_COUNT=$(ls "$IROOT/lib64/"*.so* 2>/dev/null | wc -l)
log "  Libraries: $LIB_COUNT"

# ── 2i: WiFi tools ──
for tool in wpa_supplicant wpa_cli iw dhclient rfkill; do
    SRC_BIN=$(command -v "$tool" 2>/dev/null)
    [ -n "$SRC_BIN" ] && cp -L "$SRC_BIN" "$IROOT/bin/"
    # Copy their deps too
    if [ -n "$SRC_BIN" ]; then
        for dep in $(ldd "$SRC_BIN" 2>/dev/null | grep -oP '/\S+'); do
            BASENAME=$(basename "$dep")
            [ ! -f "$IROOT/lib64/$BASENAME" ] && cp -L "$(readlink -f "$dep")" "$IROOT/lib64/$BASENAME" 2>/dev/null || true
        done
    fi
done

# ── 2j: Firmware ──
log "  Copying firmware..."
# Find firmware dir (Fedora: /usr/lib/firmware or /lib/firmware)
FWDIR=""
for d in /usr/lib/firmware /lib/firmware; do
    [ -d "$d/i915" ] && FWDIR="$d" && break
done
if [ -n "$FWDIR" ]; then
    # WiFi
    for fw in "$FWDIR"/iwlwifi-*.ucode "$FWDIR"/iwlwifi-*.ucode.zst "$FWDIR"/iwlwifi-*.ucode.xz; do
        [ -f "$fw" ] && cp -L "$fw" "$IROOT/lib/firmware/"
    done
    # Regulatory
    cp -L "$FWDIR/regulatory.db" "$IROOT/lib/firmware/" 2>/dev/null || true
    cp -L "$FWDIR/regulatory.db.p7s" "$IROOT/lib/firmware/" 2>/dev/null || true
    # GPU (i915)
    for fw in "$FWDIR"/i915/*; do
        [ -f "$fw" ] && cp -L "$fw" "$IROOT/lib/firmware/i915/"
    done
else
    log "  WARNING: No firmware directory found!"
fi
# Decompress any .zst or .xz files
for zst in "$IROOT/lib/firmware/"*.zst "$IROOT/lib/firmware/i915/"*.zst; do
    [ -f "$zst" ] && zstd -d --rm "$zst" 2>/dev/null || true
done
for xzf in "$IROOT/lib/firmware/"*.xz "$IROOT/lib/firmware/i915/"*.xz; do
    [ -f "$xzf" ] && xz -d "$xzf" 2>/dev/null || true
done
FW_COUNT=$(find "$IROOT/lib/firmware" -type f | wc -l)
log "  Firmware: $FW_COUNT files"

# ── 2k: SSL certs ──
cp /etc/pki/tls/certs/ca-bundle.crt "$IROOT/etc/pki/tls/certs/" 2>/dev/null || true

# ── 2l: ALSA config ──
if [ -d /usr/share/alsa ]; then
    mkdir -p "$IROOT/usr/share"
    cp -a /usr/share/alsa "$IROOT/usr/share/" 2>/dev/null || true
fi

# ── 2m: /etc/group and /etc/passwd ──
echo "root:x:0:" > "$IROOT/etc/group"
echo "root:x:0:root" > "$IROOT/etc/passwd"

# ── 2n: KidLisp bundle ──
if command -v npx &>/dev/null && [ -f "$SRC/system/public/aesthetic.computer/lib/kidlisp.mjs" ]; then
    log "  Bundling KidLisp..."
    cd "$SRC"
    npx esbuild system/public/aesthetic.computer/lib/kidlisp.mjs \
        --bundle --format=esm --platform=neutral \
        --outfile=/tmp/kidlisp-bundle.js 2>/dev/null || true
    if [ -f /tmp/kidlisp-bundle.js ]; then
        mkdir -p "$IROOT/jslib"
        cp /tmp/kidlisp-bundle.js "$IROOT/jslib/"
    fi
    cd "$NATIVE"
fi

# ── Final verification ──
BROKEN_FINAL=$(find "$IROOT" -type l ! -exec test -e {} \; -print 2>/dev/null | wc -l)
TOTAL_FILES=$(find "$IROOT" -type f | wc -l)
log "  Initramfs: $TOTAL_FILES files, $BROKEN_FINAL broken symlinks"
[ "$BROKEN_FINAL" -gt 0 ] && err "  WARNING: broken symlinks remain!"

# ══════════════════════════════════════════════
# Step 3: Pack initramfs (cpio + lz4)
# ══════════════════════════════════════════════
log "Step 3/4: Packing initramfs..."
cd "$IROOT"
find . -print0 | cpio --null -ov --format=newc 2>/dev/null | lz4 -l -9 -f - "$BUILD/initramfs.cpio.lz4"
INITRAMFS_SIZE=$(stat -c%s "$BUILD/initramfs.cpio.lz4")
log "  Initramfs: $((INITRAMFS_SIZE / 1048576))MB compressed"

# ══════════════════════════════════════════════
# Step 4: Build kernel with embedded initramfs
# ══════════════════════════════════════════════
log "Step 4/4: Building kernel..."
LINUX_DIR="$BUILD/linux-$KVER"

# Use cached kernel source or download
if [ ! -f "$LINUX_DIR/Makefile" ]; then
    if [ -d "/cache/linux-$KVER" ]; then
        log "  Using cached Linux $KVER..."
        cp -a "/cache/linux-$KVER" "$BUILD/"
    else
        log "  Downloading Linux $KVER..."
        cd "$BUILD"
        curl -sL "https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-${KVER}.tar.xz" | tar xJ
    fi
fi

# Copy config
cp "$NATIVE/kernel/config-minimal" "$LINUX_DIR/.config"

# Copy initramfs into kernel tree
cp "$BUILD/initramfs.cpio.lz4" "$LINUX_DIR/initramfs.cpio.lz4"

# Configure — force-disable bloated GPU drivers that olddefconfig enables
cd "$LINUX_DIR"
make olddefconfig 2>&1 | tail -3

# Strip GPU drivers that Fedora defaults enable (they cause KALLSYMS overflow)
scripts/config --disable DRM_AMDGPU
scripts/config --disable DRM_NOUVEAU
scripts/config --disable DRM_RADEON
scripts/config --disable DRM_QXL
scripts/config --disable DRM_BOCHS
scripts/config --disable DRM_CIRRUS_QEMU
scripts/config --disable DRM_VIRTIO_GPU
scripts/config --enable DRM_SIMPLEDRM
make olddefconfig 2>&1 | tail -1

# Clean initramfs object to force re-embed
rm -f usr/initramfs_data.o usr/.initramfs_data.o.cmd

# Build
log "  Compiling ($(nproc) cores)..."
make -j$(nproc) KALLSYMS_EXTRA_PASS=1 bzImage 2>&1 | tail -3

# Copy output
cp arch/x86/boot/bzImage "$BUILD/vmlinuz"
cp arch/x86/boot/bzImage "$OUT/vmlinuz" 2>/dev/null || true

VMLINUZ_SIZE=$(stat -c%s "$BUILD/vmlinuz")
SHA=$(sha256sum "$BUILD/vmlinuz" | awk '{print $1}')

log ""
log "═══════════════════════════════════════════"
log "  Build complete: $BUILD_NAME"
log "  Kernel: $((VMLINUZ_SIZE / 1048576))MB"
log "  SHA256: $SHA"
log "═══════════════════════════════════════════"

# Write metadata
cat > "$OUT/build.json" << EOF
{
  "name": "$BUILD_NAME",
  "git_hash": "$GIT_HASH",
  "build_ts": "$BUILD_TS",
  "size": $VMLINUZ_SIZE,
  "sha256": "$SHA",
  "handle": "$HANDLE"
}
EOF
