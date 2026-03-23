#!/bin/bash
# AC Native OS — Minimal reproducible build
# Builds: C binary → initramfs → kernel → vmlinuz
# Input: /src (repo), Output: /out/vmlinuz
set -eu

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
KERNEL_JOBS="${AC_KERNEL_JOBS:-$(nproc)}"

show_kernel_error_context() {
    local log_file="$1"
    local line
    local start
    local end

    line=$(grep -n -m 1 -E '(^|[^[:alpha:]])error:|Error [0-9]+|No rule to make target|undefined reference' "$log_file" | cut -d: -f1 || true)
    if [ -n "$line" ]; then
        start=$((line > 80 ? line - 80 : 1))
        end=$((line + 160))
        err "  Kernel error context (lines ${start}-${end}):"
        sed -n "${start},${end}p" "$log_file" >&2
    fi
    err "  Kernel build tail (last 220 lines):"
    tail -220 "$log_file" >&2 || true
}

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
    > "$BUILD/.make.log" 2>&1 || true

[ -f "$BUILD/ac-native" ] || { err "Binary compilation failed"; tail -30 "$BUILD/.make.log"; exit 1; }
log "  Binary: $(stat -c%s "$BUILD/ac-native") bytes"

# CL build happens after initramfs (step 2) — see below

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
    mkswap swapon vi df du diff xargs nohup pgrep killall cut whoami awk \
    sync poweroff reboot halt mknod; do
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

# ── 2n: Claude Code ──
CLAUDE_BIN=""
for p in /claude-bin /usr/local/bin/claude /home/me/.local/share/claude/versions/*; do
    [ -f "$p" ] && CLAUDE_BIN="$p" && break
done
if [ -n "$CLAUDE_BIN" ]; then
    cp "$CLAUDE_BIN" "$IROOT/bin/claude"
    chmod +x "$IROOT/bin/claude"
    # Copy its shared libs
    for lib in $(ldd "$CLAUDE_BIN" 2>/dev/null | grep -oP '/\S+'); do
        [ -f "$lib" ] && cp -nL "$lib" "$IROOT/lib64/" 2>/dev/null || true
    done
    log "  Claude Code: $(du -sh "$IROOT/bin/claude" | cut -f1)"
else
    log "  Claude Code: not available (mount with -v /path/to/claude:/claude-bin)"
fi

# ── 2o: KidLisp bundle ──
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

# ── Optional: Swap in Common Lisp binary ──
if [ "${AC_BUILD_LISP:-0}" = "1" ]; then
    log "Step 2b: Building ac-native (Common Lisp)..."
    CL_DIR="$NATIVE/cl"
    sbcl --non-interactive \
        --eval '(load "/opt/quicklisp/setup.lisp")' \
        --eval '(require :asdf)' \
        --eval "(push #P\"$CL_DIR/\" asdf:*central-registry*)" \
        --eval '(asdf:load-system :ac-native)' \
        --eval "(ac-native.build:build \"$BUILD/ac-native-cl\")" \
        2>&1 || { err "CL build failed"; }
    if [ -f "$BUILD/ac-native-cl" ]; then
        log "  CL Binary: $(stat -c%s "$BUILD/ac-native-cl") bytes"
        # Swap into initramfs (keep C version in build dir)
        cp "$IROOT/ac-native" "$BUILD/ac-native-c"
        cp "$BUILD/ac-native-cl" "$IROOT/ac-native"
        # Add libzstd (CL runtime needs it, C binary doesn't)
        for lib in /lib64/libzstd.so*; do
            [ -f "$lib" ] && cp -L "$lib" "$IROOT/lib64/" 2>/dev/null
        done
        log "  Swapped CL binary into initramfs"
    else
        err "CL binary not produced — using C binary"
    fi
fi

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
KCFG_LOG="$BUILD/kernel-olddefconfig.log"
make olddefconfig >"$KCFG_LOG" 2>&1 || { err "Kernel olddefconfig failed"; tail -80 "$KCFG_LOG" >&2; exit 1; }
tail -3 "$KCFG_LOG" || true

# Strip GPU drivers that Fedora defaults enable (they cause KALLSYMS overflow)
scripts/config --disable DRM_AMDGPU
scripts/config --disable DRM_NOUVEAU
scripts/config --disable DRM_RADEON
scripts/config --disable DRM_QXL
scripts/config --disable DRM_BOCHS
scripts/config --disable DRM_CIRRUS_QEMU
scripts/config --disable DRM_VIRTIO_GPU
scripts/config --enable DRM_SIMPLEDRM
make olddefconfig >>"$KCFG_LOG" 2>&1 || { err "Kernel olddefconfig (post-config) failed"; tail -120 "$KCFG_LOG" >&2; exit 1; }
tail -1 "$KCFG_LOG" || true

# Clean stale objects to avoid config mismatch errors
make clean 2>/dev/null || true

# Build
log "  Compiling (${KERNEL_JOBS} cores)..."
KERNEL_LOG="$BUILD/kernel-build.log"
if ! make -j"${KERNEL_JOBS}" KALLSYMS_EXTRA_PASS=1 bzImage >"$KERNEL_LOG" 2>&1; then
    err "Kernel compile failed while building bzImage."
    show_kernel_error_context "$KERNEL_LOG"
    exit 1
fi
tail -3 "$KERNEL_LOG" || true

# Copy output
if [ ! -f arch/x86/boot/bzImage ]; then
    err "Kernel build completed but arch/x86/boot/bzImage is missing."
    show_kernel_error_context "$KERNEL_LOG"
    exit 1
fi
cp arch/x86/boot/bzImage "$BUILD/vmlinuz"
cp arch/x86/boot/bzImage "$OUT/vmlinuz" 2>/dev/null || true

VMLINUZ_SIZE=$(stat -c%s "$BUILD/vmlinuz")
SHA=$(sha256sum "$BUILD/vmlinuz" | awk '{print $1}')

# ══════════════════════════════════════════════
# Step 5: Generate UEFI-bootable ISO
# ══════════════════════════════════════════════
log "Step 5: Building ISO..."
ISO_DIR="$BUILD/iso-root"
EFI_IMG="$BUILD/efi.img"
ISO_OUT="$BUILD/ac-os.iso"

rm -rf "$ISO_DIR" "$EFI_IMG"
mkdir -p "$ISO_DIR/EFI/BOOT"

# Copy kernel as UEFI boot binary
cp "$BUILD/vmlinuz" "$ISO_DIR/EFI/BOOT/BOOTX64.EFI"

# Create config.json with patchable marker (browser replaces this)
CONFIG_MARKER='{"handle":"","piece":"notepat","sub":"","email":""}'
CONFIG_PAD=4096
printf "%-${CONFIG_PAD}s" "$CONFIG_MARKER" > "$ISO_DIR/config.json"

# Create FAT32 EFI boot image
EFI_SIZE_MB=$(( (VMLINUZ_SIZE + CONFIG_PAD + 1048576) / 1048576 + 4 ))
dd if=/dev/zero of="$EFI_IMG" bs=1M count=$EFI_SIZE_MB 2>/dev/null
mkfs.fat -F 32 "$EFI_IMG" >/dev/null 2>&1
mmd -i "$EFI_IMG" ::EFI ::EFI/BOOT 2>/dev/null
mcopy -i "$EFI_IMG" "$BUILD/vmlinuz" ::EFI/BOOT/BOOTX64.EFI
mcopy -i "$EFI_IMG" "$ISO_DIR/config.json" ::config.json

# Build hybrid ISO with xorriso (EFI boot via El Torito + GPT for dd/USB)
# Uses -append_partition to attach the EFI image, matching ac-os generate_template_iso().
if command -v xorriso &>/dev/null; then
    xorriso -as mkisofs \
        -o "$ISO_OUT" \
        -V "AC_NATIVE" \
        -J -joliet-long \
        -append_partition 2 0xef "$EFI_IMG" \
        -e --interval:appended_partition_2:all:: \
        -no-emul-boot \
        -isohybrid-gpt-basdat \
        "$ISO_DIR" 2>&1 | grep -v "^xorriso\|^Drive\|^Media" || true
else
    # Fallback: raw EFI disk image (dd-able)
    log "  No xorriso — creating raw disk image"
    cp "$EFI_IMG" "$ISO_OUT"
fi

if [ -f "$ISO_OUT" ]; then
    ISO_SIZE=$(stat -c%s "$ISO_OUT")
    ISO_SHA=$(sha256sum "$ISO_OUT" | awk '{print $1}')
    cp "$ISO_OUT" "$OUT/ac-os.iso" 2>/dev/null || true
    log "  ISO: $((ISO_SIZE / 1048576))MB (sha256: ${ISO_SHA:0:16}...)"
else
    log "  ISO generation failed — vmlinuz still available"
fi

log ""
log "═══════════════════════════════════════════"
log "  Build complete: $BUILD_NAME"
log "  Kernel: $((VMLINUZ_SIZE / 1048576))MB"
log "  SHA256: $SHA"
if [ -f "$ISO_OUT" ]; then
    log "  ISO: $((ISO_SIZE / 1048576))MB"
fi
log "═══════════════════════════════════════════"

# Write metadata
cat > "$OUT/build.json" << EOF
{
  "name": "$BUILD_NAME",
  "git_hash": "$GIT_HASH",
  "build_ts": "$BUILD_TS",
  "size": $VMLINUZ_SIZE,
  "sha256": "$SHA",
  "iso_size": ${ISO_SIZE:-0},
  "iso_sha256": "${ISO_SHA:-}",
  "handle": "$HANDLE"
}
EOF
