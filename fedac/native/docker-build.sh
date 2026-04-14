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
MEDIA_LAYOUT_LIB="$NATIVE/scripts/media-layout.sh"

log()  { echo -e "\033[0;36m[ac-os]\033[0m $*"; }
err()  { echo -e "\033[0;31m[ac-os]\033[0m $*" >&2; }

# shellcheck source=/workspaces/aesthetic-computer/fedac/native/scripts/media-layout.sh
source "$MEDIA_LAYOUT_LIB"

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

run_make_with_heartbeat() {
    local log_file="$1"
    shift
    local heartbeat_secs="${AC_KERNEL_HEARTBEAT_SECS:-20}"
    local last_count="-1"
    local last_line=""
    local line_count
    local current_line

    : > "$log_file"
    "$@" >"$log_file" 2>&1 &
    local make_pid=$!

    while kill -0 "$make_pid" 2>/dev/null; do
        sleep "$heartbeat_secs"
        [ -f "$log_file" ] || continue
        line_count=$(wc -l <"$log_file" 2>/dev/null || echo 0)
        current_line=$(tail -1 "$log_file" 2>/dev/null || true)
        current_line="${current_line:0:180}"
        if [ "$line_count" != "$last_count" ] || [ "$current_line" != "$last_line" ]; then
            log "  [kernel] running... lines=$line_count last=$current_line"
            last_count="$line_count"
            last_line="$current_line"
        else
            log "  [kernel] running... lines=$line_count (no new lines yet)"
        fi
    done

    if wait "$make_pid"; then
        return 0
    fi
    return $?
}

# ── ccache setup (persisted via Docker volume at /ccache) ──
export CCACHE_DIR="${CCACHE_DIR:-/ccache}"
mkdir -p "$CCACHE_DIR"
export CCACHE_MAXSIZE="${CCACHE_MAXSIZE:-2G}"
export CCACHE_COMPRESS=1
if command -v ccache &>/dev/null; then
    CC_USE="ccache gcc"
    log "ccache: enabled (dir=$CCACHE_DIR, max=$CCACHE_MAXSIZE)"
    ccache -s 2>/dev/null | grep -E "cache size|hit rate" || true
else
    CC_USE="gcc"
fi

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

make -j$(nproc) CC="${CC_USE}" BUILDDIR="$BUILD" \
    BUILD_TS="$BUILD_TS" GIT_HASH="$GIT_HASH" BUILD_NAME="$BUILD_NAME" \
    > "$BUILD/.make.log" 2>&1 || true

[ -f "$BUILD/ac-native" ] || { tail -60 "$BUILD/.make.log" >&2; err "Binary compilation failed"; exit 1; }
log "  Binary: $(stat -c%s "$BUILD/ac-native") bytes"
log "  NEEDED libs:"
readelf -d "$BUILD/ac-native" 2>/dev/null | grep NEEDED | sed 's/.*\[/    /' | sed 's/\]//'
ldd "$BUILD/ac-native" 2>&1 | grep -i "not found" && err "  MISSING LIBS DETECTED" || log "  All libs resolved"

# CL build happens after initramfs (step 2) — see below

# ══════════════════════════════════════════════
# Step 2: Build initramfs from scratch
# ══════════════════════════════════════════════
log "Step 2/4: Building initramfs..."
IROOT="$BUILD/initramfs-root"
rm -rf "$IROOT"
mkdir -p "$IROOT"/{bin,lib64,lib/firmware/i915,dev,proc,sys,tmp,run,scripts,etc,etc/pki/tls/certs}

# ── 2a: Static busybox (no shared lib deps for shell) ──
BUSYBOX=$(command -v busybox)
cp "$BUSYBOX" "$IROOT/bin/busybox"
for cmd in sh sleep mkdir mount umount cat echo ls cp mv rm ln chmod chown \
    date dd find grep head kill ps sed sort tail tee test touch tr wc which \
    mktemp printf seq stat basename dirname env expr true false readlink \
    realpath rmdir uniq yes tar gzip gunzip hostname id ip modprobe \
    mkswap swapon vi df du diff xargs nohup pgrep killall cut whoami awk \
    sync poweroff reboot halt mknod udhcpc; do
    ln -s busybox "$IROOT/bin/$cmd"
done
# udhcpc also expected at /sbin/ by wifi.c
ln -sf ../bin/busybox "$IROOT/sbin/udhcpc" 2>/dev/null || true

# ── 2a2: curl + wget for HTTP fetches (captive portals, OTA, API calls) ──
CURL_BIN=$(command -v curl)
if [ -n "$CURL_BIN" ]; then
    cp "$CURL_BIN" "$IROOT/bin/curl"
    for lib in $(ldd "$CURL_BIN" 2>/dev/null | grep -oP '/\S+'); do
        [ -f "$lib" ] && cp -nL "$lib" "$IROOT/lib64/" 2>/dev/null || true
    done
fi
# busybox wget as fallback
ln -s busybox "$IROOT/bin/wget" 2>/dev/null || true
# Minimal udhcpc script to configure interface after getting lease
mkdir -p "$IROOT/usr/share/udhcpc"
cat > "$IROOT/usr/share/udhcpc/default.script" << 'UDHCPC_SCRIPT'
#!/bin/sh
case "$1" in
    bound|renew)
        ip addr flush dev "$interface"
        ip addr add "$ip/${mask:-24}" dev "$interface"
        [ -n "$router" ] && ip route add default via "$router" dev "$interface"
        # DNS: use DHCP-provided + fallback to Google/Cloudflare
        {
            for d in $dns; do echo "nameserver $d"; done
            echo "nameserver 8.8.8.8"
            echo "nameserver 1.1.1.1"
        } > /etc/resolv.conf
        ;;
    deconfig)
        ip addr flush dev "$interface"
        ;;
esac
UDHCPC_SCRIPT
chmod +x "$IROOT/usr/share/udhcpc/default.script"

# ── 2b: Init script ──
cp "$NATIVE/initramfs/init" "$IROOT/init"
chmod +x "$IROOT/init"
cp "$NATIVE/initramfs-scripts/"*.sh "$IROOT/scripts/" 2>/dev/null || true
chmod +x "$IROOT/scripts/"*.sh 2>/dev/null || true

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

# SDL3 + Mesa/GPU libs — included for GPU acceleration via dlopen.
# ac-native runs as a child of init (not PID 1), so if Mesa DRI crashes,
# init catches the signal and restarts without SDL (AC_NO_SDL=1).
for lib in libSDL3.so.0 \
    libgbm.so.1 libEGL.so.1 libEGL_mesa.so.0 libGLESv2.so.2 \
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

# GBM backend loader plugin. libgbm.so.1 has a hardcoded
# /usr/lib64/gbm/dri_gbm.so path baked in at build time — without this
# file, SDL3's KMSDRM probe segfaults during Mesa init and ac-native
# falls back to DRM-only every boot. Ship it at both /lib64/gbm AND
# /usr/lib64/gbm so the loader resolves regardless of root.
for src in /usr/lib64/gbm/dri_gbm.so /lib64/gbm/dri_gbm.so; do
    if [ -f "$src" ]; then
        mkdir -p "$IROOT/lib64/gbm" "$IROOT/usr/lib64/gbm"
        cp -L "$src" "$IROOT/lib64/gbm/dri_gbm.so"
        cp -L "$src" "$IROOT/usr/lib64/gbm/dri_gbm.so"
        break
    fi
done

# Gallium megadriver
GALLIUM=$(readlink -f /lib64/libgallium-*.so 2>/dev/null || true)
[ -f "$GALLIUM" ] && cp -L "$GALLIUM" "$IROOT/lib64/"

# Transitive deps — resolve everything in lib64
log "  Resolving transitive dependencies..."
ADDED=1
while [ "$ADDED" -gt 0 ]; do
    ADDED=0
    for elf in "$IROOT/lib64/"*.so* "$IROOT/lib64/dri/"*.so* "$IROOT/ac-native"; do
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

# ── 2g2: Verify DRI driver deps ──
if [ -f "$IROOT/lib64/dri/iris_dri.so" ]; then
    log "  Checking iris_dri.so dependencies..."
    MISSING_DRI=""
    for needed in $(LD_LIBRARY_PATH="$IROOT/lib64" ldd "$IROOT/lib64/dri/iris_dri.so" 2>/dev/null | grep "not found" | awk '{print $1}'); do
        MISSING_DRI="$MISSING_DRI $needed"
        # Try to find and copy the missing lib
        REAL=$(readlink -f "/lib64/$needed" 2>/dev/null || readlink -f "/usr/lib64/$needed" 2>/dev/null)
        if [ -f "$REAL" ]; then
            cp -L "$REAL" "$IROOT/lib64/$needed"
            log "    Fixed: $needed"
        else
            log "    MISSING: $needed (not found on build system)"
        fi
    done
    if [ -n "$MISSING_DRI" ]; then
        log "  iris_dri.so had missing deps:$MISSING_DRI"
        # Re-run transitive dep resolution after fixing
        ADDED=1
        while [ "$ADDED" -gt 0 ]; do
            ADDED=0
            for elf in "$IROOT/lib64/"*.so* "$IROOT/lib64/dri/"*.so*; do
                [ -f "$elf" ] || continue
                for needed in $(readelf -d "$elf" 2>/dev/null | grep NEEDED | sed 's/.*\[\(.*\)\]/\1/'); do
                    if [ ! -f "$IROOT/lib64/$needed" ]; then
                        REAL=$(readlink -f "/lib64/$needed" 2>/dev/null || readlink -f "/usr/lib64/$needed" 2>/dev/null)
                        if [ -f "$REAL" ]; then
                            cp -L "$REAL" "$IROOT/lib64/$needed"
                            ADDED=$((ADDED + 1))
                        fi
                    fi
                done
            done
        done
    else
        log "  iris_dri.so: all deps OK"
    fi
    # Final ldd check
    FINAL_MISSING=$(LD_LIBRARY_PATH="$IROOT/lib64" ldd "$IROOT/lib64/dri/iris_dri.so" 2>&1 | grep -c "not found" || true)
    log "  iris_dri.so final check: ${FINAL_MISSING:-0} missing deps"
fi

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
    SRC_BIN=$(command -v "$tool" 2>/dev/null || true)
    if [ -n "$SRC_BIN" ]; then cp -L "$SRC_BIN" "$IROOT/bin/"; fi
    # Copy their deps too
    if [ -n "$SRC_BIN" ]; then
        for dep in $(ldd "$SRC_BIN" 2>/dev/null | grep -oP '/\S+'); do
            BASENAME=$(basename "$dep")
            [ ! -f "$IROOT/lib64/$BASENAME" ] && cp -L "$(readlink -f "$dep")" "$IROOT/lib64/$BASENAME" 2>/dev/null || true
        done
    fi
done

# ── 2i2: Disk/EFI tools (for HD install + OTA flash) ──
for tool in sfdisk mkfs.vfat efibootmgr partprobe; do
    SRC_BIN=$(command -v "$tool" 2>/dev/null || true)
    if [ -n "$SRC_BIN" ]; then
        cp -L "$SRC_BIN" "$IROOT/bin/"
        for dep in $(ldd "$SRC_BIN" 2>/dev/null | grep -oP '/\S+'); do
            BASENAME=$(basename "$dep")
            [ ! -f "$IROOT/lib64/$BASENAME" ] && cp -L "$(readlink -f "$dep")" "$IROOT/lib64/$BASENAME" 2>/dev/null || true
        done
    fi
done

# ── 2j: Firmware (trimmed to common Intel WiFi + GPU chips) ──
log "  Copying firmware..."
FWDIR=""
for d in /usr/lib/firmware /lib/firmware; do
    [ -d "$d/i915" ] && FWDIR="$d" && break
done
if [ -n "$FWDIR" ]; then
    # WiFi — only common Intel chip families (covers ThinkPad, NUC, Surface)
    # Each family: 3160, 7260, 7265, 8000, 8265, 9000, 9260, ax200, ax201, ax210, ax211, be200
    for chip in 3160 3168 7260 7265 8000C 8265 9000 9260 \
                ax200 ax201 ax210 ax211 be200 gl; do
        for fw in "$FWDIR"/iwlwifi-${chip}*.ucode "$FWDIR"/iwlwifi-${chip}*.ucode.zst "$FWDIR"/iwlwifi-${chip}*.ucode.xz; do
            [ -f "$fw" ] && cp -L "$fw" "$IROOT/lib/firmware/"
        done
    done
    # Regulatory
    cp -L "$FWDIR/regulatory.db" "$IROOT/lib/firmware/" 2>/dev/null || true
    cp -L "$FWDIR/regulatory.db.p7s" "$IROOT/lib/firmware/" 2>/dev/null || true
    # GPU — only KBL/SKL/CFL/ICL/TGL/ADL/RPL (covers ~95% of target hardware)
    for pattern in kbl skl cfl icl tgl adl dg1 rkl rpl mtl; do
        for fw in "$FWDIR"/i915/${pattern}_*; do
            [ -f "$fw" ] && cp -L "$fw" "$IROOT/lib/firmware/i915/"
        done
    done
    # DMC firmware (display power management)
    for fw in "$FWDIR"/i915/*_dmc_*.bin "$FWDIR"/i915/*_dmc_*.bin.zst; do
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
FW_SIZE=$(du -sh "$IROOT/lib/firmware" | cut -f1)
log "  Firmware: $FW_COUNT files ($FW_SIZE)"

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
for p in /claude-bin /usr/local/bin/claude-native /usr/local/bin/claude /root/.local/share/claude/versions/* /home/me/.local/share/claude/versions/*; do
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
        --bundle --format=iife --global-name=KidLispModule --platform=neutral \
        --external:https --external:http --external:net --external:fs --external:path \
        --outfile=/tmp/kidlisp-bundle.js 2>&1 || true
    if [ -f /tmp/kidlisp-bundle.js ]; then
        mkdir -p "$IROOT/jslib"
        cp /tmp/kidlisp-bundle.js "$IROOT/jslib/"
    fi
    cd "$NATIVE"
fi

# ── 2o2: FPS system bundle (CamDoll + Camera + Dolly for `system="fps"` pieces) ──
# Tree-shakes graph.mjs down to just Camera + Dolly and pulls in CamDoll.
# Pieces like arena.mjs that export `system = "fps"` get this injected
# by the piece loader (see js_load_piece in js-bindings.c) so web and
# native share the exact same FPS camera/input source.
if command -v npx &>/dev/null && [ -f "$NATIVE/scripts/fps-bundle-entry.mjs" ]; then
    log "  Bundling FPS system (Camera + Dolly + CamDoll)..."
    cd "$SRC"
    # IIFE format so the bundle self-executes at load and exposes the
    # classes as `globalThis.__FpsSystem.{Camera,Dolly,CamDoll}` — lets
    # the piece loader wire them in synchronously without ES module
    # resolution. Mirrors the kidlisp-bundle.js pattern.
    npx esbuild "$NATIVE/scripts/fps-bundle-entry.mjs" \
        --bundle --format=iife --global-name=__FpsSystem --platform=neutral \
        --external:https --external:http --external:net --external:fs --external:path \
        --outfile=/tmp/fps-system-bundle.js 2>&1 || true
    if [ -f /tmp/fps-system-bundle.js ]; then
        mkdir -p "$IROOT/jslib"
        cp /tmp/fps-system-bundle.js "$IROOT/jslib/fps-system-bundle.js"
        log "    fps-system-bundle.js: $(stat -c%s /tmp/fps-system-bundle.js) bytes"
    fi
    cd "$NATIVE"
fi

# ── 2p: ES module lib files for pieces (clock.mjs needs these) ──
# These are pure JS with no DOM/browser deps — work in QuickJS as-is.
# The module loader resolves "../lib/X.mjs" → "/lib/X.mjs" in the initramfs.
mkdir -p "$IROOT/lib"
for libmjs in melody-parser.mjs notepat-convert.mjs note-colors.mjs num.mjs; do
    SRC_LIB="$SRC/system/public/aesthetic.computer/lib/$libmjs"
    if [ -f "$SRC_LIB" ]; then
        cp "$SRC_LIB" "$IROOT/lib/$libmjs"
        log "  Bundled lib: $libmjs ($(stat -c%s "$SRC_LIB") bytes)"
    fi
done

# ── 2q: Web pieces that work natively (bundled verbatim, no mods) ──
# Every piece here must run unchanged under the QuickJS-based native
# runtime. If you add a piece that uses browser-only APIs, the runtime
# will throw — don't "port" the piece, expose the missing API in
# js-bindings.c instead (so web and native stay in parity).
for webpiece in clock.mjs arena.mjs; do
    SRC_PIECE="$SRC/system/public/aesthetic.computer/disks/$webpiece"
    if [ -f "$SRC_PIECE" ]; then
        cp "$SRC_PIECE" "$IROOT/pieces/$webpiece"
        log "  Bundled web piece: $webpiece ($(stat -c%s "$SRC_PIECE") bytes)"
    fi
done

# ── Final verification ──
BROKEN_FINAL=$(find "$IROOT" -type l ! -exec test -e {} \; -print 2>/dev/null | wc -l)
TOTAL_FILES=$(find "$IROOT" -type f | wc -l)
log "  Initramfs: $TOTAL_FILES files, $BROKEN_FINAL broken symlinks"
[ "$BROKEN_FINAL" -gt 0 ] && err "  WARNING: broken symlinks remain!"

# Write build metadata (C variant by default, CL overrides below)
mkdir -p "$IROOT/etc"
printf '%s\n%s\n%s\nc\n' "$BUILD_NAME" "$GIT_HASH" "$BUILD_TS" > "$IROOT/etc/ac-build"

# ── Embed SBCL + Swank for live CL development ──
log "Step 2b: Embedding SBCL + Swank..."

# Build libquickjs.so for CL CFFI bindings
QJSDIR="/cache/quickjs-2024-01-13"
log "  Building libquickjs.so..."
gcc -shared -fPIC -O2 -Wl,-soname,libquickjs.so \
    -o /lib64/libquickjs.so \
    "$QJSDIR/quickjs.c" "$QJSDIR/libunicode.c" \
    "$QJSDIR/libregexp.c" "$QJSDIR/cutils.c" "$QJSDIR/libbf.c" \
    '-DCONFIG_VERSION="0.8.0"' -lm 2>&1 || { err "libquickjs.so build failed"; }

CL_DIR="$NATIVE/cl"
log "  Building libquickjs-shim.so..."
gcc -shared -fPIC -O2 -Wl,-soname,libquickjs-shim.so \
    -o /lib64/libquickjs-shim.so \
    "$CL_DIR/quickjs-shim.c" \
    -I"$QJSDIR" -L/lib64 -lquickjs -lm 2>&1 || { err "shim build failed"; }

ldconfig 2>/dev/null || true

# Copy shared libs into initramfs
cp /lib64/libquickjs.so "$IROOT/lib64/"
cp /lib64/libquickjs-shim.so "$IROOT/lib64/"
for lib in /lib64/libzstd.so*; do
    [ -f "$lib" ] && cp -L "$lib" "$IROOT/lib64/" 2>/dev/null
done

# Build SBCL Swank server image (standalone binary with Swank preloaded)
export LD_LIBRARY_PATH="/lib64:${LD_LIBRARY_PATH:-}"
log "  Building SBCL Swank image..."
sbcl --non-interactive \
    --eval '(load "/opt/quicklisp/setup.lisp")' \
    --eval '(require :asdf)' \
    --eval "(push #P\"$CL_DIR/\" asdf:*central-registry*)" \
    --eval '(asdf:load-system :ac-native)' \
    --eval "(sb-ext:save-lisp-and-die \"$BUILD/ac-swank\"
              :toplevel #'ac-native:main
              :executable t
              :compression t)" \
    2>&1 || { err "SBCL Swank build failed (non-fatal)"; }

if [ -f "$BUILD/ac-swank" ]; then
    cp "$BUILD/ac-swank" "$IROOT/ac-swank"
    chmod +x "$IROOT/ac-swank"
    log "  SBCL Swank: $(stat -c%s "$BUILD/ac-swank") bytes"
else
    log "  SBCL Swank: skipped (build failed, C-only build)"
fi

# Copy CL pieces (only runnable .lisp pieces from pieces/ dir, not cl/ library)
if ls "$NATIVE/pieces/"*.lisp 1>/dev/null 2>&1; then
    cp "$NATIVE/pieces/"*.lisp "$IROOT/pieces/"
fi
CL_PIECES=$(ls "$IROOT/pieces/"*.lisp 2>/dev/null | wc -l)
log "  CL pieces: $CL_PIECES"

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

# Stage built-in firmware blobs referenced by CONFIG_EXTRA_FIRMWARE
# into a container-local directory and rewrite CONFIG_EXTRA_FIRMWARE_DIR.
FIRMWARE_ABS="$BUILD/firmware"
mkdir -p "$FIRMWARE_ABS"

HOST_FWDIR=""
for d in /usr/lib/firmware /lib/firmware; do
    if [ -d "$d" ]; then
        HOST_FWDIR="$d"
        break
    fi
done

copy_builtin_fw_blob() {
    local rel="$1"
    local src_base="$2"
    local dst="$FIRMWARE_ABS/$rel"
    mkdir -p "$(dirname "$dst")"
    if [ -f "$src_base/$rel" ]; then
        cp -L "$src_base/$rel" "$dst"
        return 0
    fi
    if [ -f "$src_base/$rel.zst" ]; then
        zstd -d "$src_base/$rel.zst" -o "$dst" 2>/dev/null && return 0
    fi
    if [ -f "$src_base/$rel.xz" ]; then
        xz -dc "$src_base/$rel.xz" >"$dst" 2>/dev/null && return 0
    fi
    return 1
}

FW_LIST=$(sed -n 's/^CONFIG_EXTRA_FIRMWARE="\([^"]*\)"/\1/p' "$LINUX_DIR/.config" | head -1)
if [ -n "$FW_LIST" ]; then
    if [ -z "$HOST_FWDIR" ]; then
        err "Kernel config requests built-in firmware but no /usr/lib/firmware or /lib/firmware directory was found."
        exit 1
    fi

    missing_fw=""
    for fw in $FW_LIST; do
        if ! copy_builtin_fw_blob "$fw" "$HOST_FWDIR"; then
            missing_fw="$missing_fw $fw"
        fi
    done

    if [ -n "$missing_fw" ]; then
        err "Missing built-in firmware blobs:$missing_fw"
        err "Looked under: $HOST_FWDIR (including .zst/.xz variants)"
        exit 1
    fi

    sed -i "s|^CONFIG_EXTRA_FIRMWARE_DIR=.*|CONFIG_EXTRA_FIRMWARE_DIR=\"$FIRMWARE_ABS\"|" "$LINUX_DIR/.config"
    log "  Built-in firmware dir: $FIRMWARE_ABS"
    log "  Built-in firmware files: $FW_LIST"
fi

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

# With ccache, skip make clean — stale objects get cache misses anyway,
# and clean destroys the build tree that ccache relies on for hits.
# Without ccache, clean to avoid config mismatch errors.
if ! command -v ccache &>/dev/null; then
    make clean 2>/dev/null || true
fi

# Build
log "  Compiling (${KERNEL_JOBS} cores)..."
KERNEL_LOG="$BUILD/kernel-build.log"
if ! run_make_with_heartbeat "$KERNEL_LOG" make -j"${KERNEL_JOBS}" CC="${CC_USE}" KALLSYMS_EXTRA_PASS=1 bzImage; then
    err "Kernel compile failed while building bzImage (parallel pass)."
    show_kernel_error_context "$KERNEL_LOG"
    if [ "${KERNEL_JOBS}" -gt 1 ]; then
        err "Retrying kernel build in serial mode (-j1, V=1) for deterministic diagnostics..."
        make clean 2>/dev/null || true
        KERNEL_LOG_RETRY="$BUILD/kernel-build-retry.log"
        if ! run_make_with_heartbeat "$KERNEL_LOG_RETRY" make -j1 V=1 CC="${CC_USE}" KALLSYMS_EXTRA_PASS=1 bzImage; then
            err "Kernel compile failed again in serial retry."
            show_kernel_error_context "$KERNEL_LOG_RETRY"
            exit 1
        fi
        KERNEL_LOG="$KERNEL_LOG_RETRY"
        log "  Serial retry succeeded."
    else
        exit 1
    fi
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
# Step 4b: Build slim kernel (no embedded initramfs) for Mac EFI boot
# ══════════════════════════════════════════════
log "Step 4b: Building slim kernel for Mac..."
sed -i 's|^CONFIG_INITRAMFS_SOURCE=.*|CONFIG_INITRAMFS_SOURCE=""|' .config
make olddefconfig >>"$KCFG_LOG" 2>&1 || { err "Kernel olddefconfig failed before slim build"; tail -120 "$KCFG_LOG" >&2; exit 1; }
if ! command -v ccache &>/dev/null; then make clean 2>/dev/null || true; fi
rm -f usr/initramfs_data.o usr/.initramfs_data.o.cmd
if run_make_with_heartbeat "$BUILD/kernel-slim.log" make -j"${KERNEL_JOBS}" CC="${CC_USE}" bzImage; then
    cp arch/x86/boot/bzImage "$BUILD/vmlinuz-slim"
    cp arch/x86/boot/bzImage "$OUT/vmlinuz-slim" 2>/dev/null || true
    SLIM_SIZE=$(stat -c%s "$BUILD/vmlinuz-slim")
    log "  Slim kernel: $((SLIM_SIZE / 1048576))MB"
else
    err "Slim kernel build failed (non-fatal)"
fi
# Restore config for any subsequent builds
sed -i 's|^CONFIG_INITRAMFS_SOURCE=.*|CONFIG_INITRAMFS_SOURCE="initramfs.cpio.lz4"|' .config
make olddefconfig >>"$KCFG_LOG" 2>&1 || { err "Kernel olddefconfig failed while restoring initramfs config"; tail -120 "$KCFG_LOG" >&2; exit 1; }

# Export initramfs as gzip (for systemd-boot on Mac)
log "  Packing initramfs.cpio.gz..."
lz4 -d "$BUILD/initramfs.cpio.lz4" -c 2>/dev/null | gzip -c > "$BUILD/initramfs.cpio.gz"
cp "$BUILD/initramfs.cpio.gz" "$OUT/initramfs.cpio.gz" 2>/dev/null || true
log "  initramfs.cpio.gz: $(($(stat -c%s "$BUILD/initramfs.cpio.gz") / 1048576))MB"

# ══════════════════════════════════════════════
# Step 5: Generate UEFI-bootable ISO
# ══════════════════════════════════════════════
log "Step 5: Building ISO..."
ISO_DIR="$BUILD/iso-root"
EFI_IMG="$BUILD/efi.img"
ISO_OUT="$BUILD/ac-os.iso"
CONFIG_TMP="$BUILD/identity-config.json"

rm -rf "$ISO_DIR" "$EFI_IMG" "$CONFIG_TMP"

ac_media_write_identity_config "$CONFIG_TMP"
ac_media_stage_boot_tree "$ISO_DIR" "$BUILD/vmlinuz" "$CONFIG_TMP"
ac_media_create_fat_image "$ISO_DIR" "$EFI_IMG" "AC_NATIVE"

# Build hybrid ISO with xorriso (EFI boot via El Torito + GPT for dd/USB)
# Uses the same chainloader-first staged tree as ac-os generate_template_iso().
if command -v xorriso &>/dev/null; then
    ac_media_build_hybrid_iso "$ISO_DIR" "$EFI_IMG" "$ISO_OUT" "AC_NATIVE"
else
    # Fallback: raw EFI disk image (dd-able)
    log "  No xorriso — creating raw disk image"
    cp "$EFI_IMG" "$ISO_OUT"
fi

rm -f "$CONFIG_TMP"

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
if command -v ccache &>/dev/null; then
    CCACHE_HIT=$(ccache -s 2>/dev/null | grep "cache hit rate" | head -1 || true)
    CCACHE_SIZE=$(ccache -s 2>/dev/null | grep "cache size" | head -1 || true)
    log "  ccache: ${CCACHE_HIT:-n/a} | ${CCACHE_SIZE:-n/a}"
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
