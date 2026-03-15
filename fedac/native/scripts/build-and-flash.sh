#!/bin/bash
# build-and-flash.sh — Build ac-native from scratch and optionally flash to USB
#
# Works in Docker/Codespaces (no losetup/mount needed — uses mtools).
#
# Usage:
#   ./build-and-flash.sh                      # Build kernel only
#   ./build-and-flash.sh --flash /dev/sdb     # Build + flash to USB
#   ./build-and-flash.sh --skip-kernel --flash /dev/sdb  # Rebuild binary/initramfs, reuse kernel source
#   ./build-and-flash.sh --skip-binary --flash /dev/sdb  # Reuse binary, rebuild initramfs+kernel
#
# Requirements: cpio lz4 jq musl-gcc (or gcc) libdrm-devel alsa-lib-devel
#               mtools (for --flash)

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log()  { echo -e "${GREEN}[build]${NC} $*"; }
warn() { echo -e "${YELLOW}[build]${NC} $*"; }
err()  { echo -e "${RED}[build]${NC} $*" >&2; }

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
NATIVE_DIR="$(dirname "${SCRIPT_DIR}")"
BUILD_DIR="${NATIVE_DIR}/build"
KERNEL_DIR="${NATIVE_DIR}/kernel"

FLASH_DEV=""
SKIP_KERNEL=0
SKIP_BINARY=0
USE_SDL=0
PIECE_PATH="${NATIVE_DIR}/pieces/prompt.mjs"
KERNEL_VERSION="${KERNEL_VERSION:-6.14.2}"
HANDLE="${AC_HANDLE:-jeffrey}"
HANDLE_COLORS_API="${HANDLE_COLORS_API:-https://aesthetic.computer/.netlify/functions/handle-colors}"

while [ $# -gt 0 ]; do
    case "$1" in
        --flash)       FLASH_DEV="$2"; shift 2 ;;
        --skip-kernel) SKIP_KERNEL=1; shift ;;
        --skip-binary) SKIP_BINARY=1; shift ;;
        --sdl)         USE_SDL=1; shift ;;
        --piece)       PIECE_PATH="$2"; shift 2 ;;
        --handle)      HANDLE="$2"; shift 2 ;;
        --help|-h)
            echo "Usage: $0 [--flash /dev/sdX] [--skip-kernel] [--skip-binary] [--sdl] [--piece path.mjs] [--handle your-handle]"
            exit 0 ;;
        *) err "Unknown option: $1"; exit 1 ;;
    esac
done

mkdir -p "${BUILD_DIR}"

# ============================================================
# Step 1: Check dependencies
# ============================================================
log "Checking dependencies..."

# Auto-install required packages if missing (Fedora/dnf)
if command -v dnf &>/dev/null; then
    PKGS_NEEDED=""
    # Build tools
    command -v cpio &>/dev/null     || PKGS_NEEDED="${PKGS_NEEDED} cpio"
    command -v lz4 &>/dev/null      || PKGS_NEEDED="${PKGS_NEEDED} lz4"
    command -v bc &>/dev/null       || PKGS_NEEDED="${PKGS_NEEDED} bc"
    command -v perl &>/dev/null     || PKGS_NEEDED="${PKGS_NEEDED} perl"
    command -v make &>/dev/null     || PKGS_NEEDED="${PKGS_NEEDED} make"
    command -v curl &>/dev/null     || PKGS_NEEDED="${PKGS_NEEDED} curl"
    command -v jq &>/dev/null       || PKGS_NEEDED="${PKGS_NEEDED} jq"
    # Kernel build headers
    [ -f /usr/include/libelf.h ]    || PKGS_NEEDED="${PKGS_NEEDED} elfutils-libelf-devel"
    [ -f /etc/pki/tls/certs/ca-bundle.crt ] || PKGS_NEEDED="${PKGS_NEEDED} ca-certificates"
    # Native binary deps
    pkg-config --exists alsa 2>/dev/null        || PKGS_NEEDED="${PKGS_NEEDED} alsa-lib-devel"
    pkg-config --exists libdrm 2>/dev/null      || PKGS_NEEDED="${PKGS_NEEDED} libdrm-devel"
    [ -f /usr/include/flite/flite.h ]           || PKGS_NEEDED="${PKGS_NEEDED} flite-devel"
    # WiFi tools + firmware
    command -v wpa_supplicant &>/dev/null || PKGS_NEEDED="${PKGS_NEEDED} wpa_supplicant"
    command -v dhclient &>/dev/null       || PKGS_NEEDED="${PKGS_NEEDED} dhcp-client"
    command -v iw &>/dev/null             || PKGS_NEEDED="${PKGS_NEEDED} iw"
    [ -d /lib/firmware ] && ls /lib/firmware/iwlwifi-cc-a0-* &>/dev/null || PKGS_NEEDED="${PKGS_NEEDED} iwlwifi-mvm-firmware"
    [ -f /lib/firmware/regulatory.db ]    || PKGS_NEEDED="${PKGS_NEEDED} wireless-regdb"
    # Flash tools
    if [ -n "${FLASH_DEV}" ]; then
        command -v mmd &>/dev/null        || PKGS_NEEDED="${PKGS_NEEDED} mtools"
        command -v mkfs.vfat &>/dev/null  || PKGS_NEEDED="${PKGS_NEEDED} dosfstools"
    fi

    if [ -n "${PKGS_NEEDED}" ]; then
        log "Installing missing packages:${PKGS_NEEDED}"
        sudo dnf install -y ${PKGS_NEEDED} || err "Failed to install packages"
    fi
fi

MISSING=""
for cmd in cpio lz4 make curl jq bc perl; do
    command -v "$cmd" &>/dev/null || MISSING="${MISSING} ${cmd}"
done
if [ -n "${MISSING}" ]; then
    err "Missing required tools:${MISSING}"
    exit 1
fi
if [ -n "${FLASH_DEV}" ]; then
    for cmd in mmd mcopy mkfs.vfat sfdisk; do
        command -v "$cmd" &>/dev/null || MISSING="${MISSING} ${cmd}"
    done
    if [ -n "${MISSING}" ]; then
        err "Missing flash tools:${MISSING}"
        exit 1
    fi
fi

# ============================================================
# Step 2: Build ac-native binary (static, musl)
# ============================================================
if [ "${SKIP_BINARY}" -eq 0 ]; then
    log "Building ac-native binary..."
    cd "${NATIVE_DIR}"
    CC_USE="${CC:-}"
    if [ -z "${CC_USE}" ]; then
        # Use musl-gcc for static linking only if it has linux/input.h
        if command -v musl-gcc &>/dev/null \
           && echo '#include <linux/input.h>' | musl-gcc -E -x c - &>/dev/null; then
            CC_USE=musl-gcc
        else
            CC_USE=gcc
        fi
    fi
    MAKE_ARGS="CC=${CC_USE}"
    if [ "${USE_SDL}" -eq 1 ]; then
        MAKE_ARGS="${MAKE_ARGS} USE_SDL=1"
    fi
    # Only request static linking when using musl-gcc (gcc needs .so files)
    if [ "${CC_USE}" = "musl-gcc" ]; then
        MAKE_ARGS="${MAKE_ARGS} STATIC=1"
    fi
    make ${MAKE_ARGS}
    log "Binary: $(wc -c < "${BUILD_DIR}/ac-native") bytes"
else
    log "Skipping binary build"
fi

if [ ! -f "${BUILD_DIR}/ac-native" ]; then
    err "ac-native binary not found. Run without --skip-binary."
    exit 1
fi

# ============================================================
# Step 3: Create initramfs
# ============================================================
log "Creating initramfs..."
INITRAMFS_DIR="${BUILD_DIR}/initramfs-root"

# Preserve piece.mjs if it exists and no --piece override
if [ -f "${INITRAMFS_DIR}/piece.mjs" ] && [ "${PIECE_PATH}" = "${INITRAMFS_DIR}/piece.mjs" ]; then
    PIECE_CONTENT="$(cat "${INITRAMFS_DIR}/piece.mjs")"
fi

rm -rf "${INITRAMFS_DIR}"
mkdir -p "${INITRAMFS_DIR}"/{dev,proc,sys,tmp,mnt}

# Copy binary
cp "${BUILD_DIR}/ac-native" "${INITRAMFS_DIR}/ac-native"
chmod +x "${INITRAMFS_DIR}/ac-native"

# Copy piece
if [ -n "${PIECE_CONTENT:-}" ]; then
    echo "${PIECE_CONTENT}" > "${INITRAMFS_DIR}/piece.mjs"
elif [ -f "${PIECE_PATH}" ]; then
    cp "${PIECE_PATH}" "${INITRAMFS_DIR}/piece.mjs"
else
    err "No piece.mjs found at ${PIECE_PATH}"
    exit 1
fi

# Copy all pieces to /pieces/ for system.jump() navigation
PIECES_SRC="${NATIVE_DIR}/pieces"
if [ -d "${PIECES_SRC}" ]; then
    mkdir -p "${INITRAMFS_DIR}/pieces"
    for p in "${PIECES_SRC}"/*.mjs; do
        [ -f "$p" ] && cp "$p" "${INITRAMFS_DIR}/pieces/"
    done
    log "Bundled pieces: $(ls "${INITRAMFS_DIR}/pieces/" | tr '\n' ' ')"
fi

# Copy web pieces that run unmodified on native (Wave 1 + clock)
AC_DISKS_DIR="${NATIVE_DIR}/../../system/public/aesthetic.computer/disks"
for web_piece in clock.mjs 3x3.mjs 404.mjs beat.mjs brick-breaker.mjs \
    dync.mjs error.mjs gostop.mjs hop.mjs shh.mjs chart.mjs \
    f3ral3xp.mjs hw.mjs ptt.mjs; do
    [ -f "${AC_DISKS_DIR}/${web_piece}" ] && cp "${AC_DISKS_DIR}/${web_piece}" "${INITRAMFS_DIR}/pieces/"
done
log "Bundled web pieces: $(ls "${INITRAMFS_DIR}/pieces/" | grep -c '.mjs') total"

# Copy shared JS libraries needed by pieces (pure JS, no browser deps)
AC_LIB_DIR="${NATIVE_DIR}/../../system/public/aesthetic.computer/lib"
mkdir -p "${INITRAMFS_DIR}/lib"
for lib_file in melody-parser.mjs notepat-convert.mjs note-colors.mjs; do
    if [ -f "${AC_LIB_DIR}/${lib_file}" ]; then
        cp "${AC_LIB_DIR}/${lib_file}" "${INITRAMFS_DIR}/lib/"
    fi
done
log "Bundled JS libs: $(ls "${INITRAMFS_DIR}/lib/"*.mjs 2>/dev/null | xargs -n1 basename | tr '\n' ' ')"

# Bundle initramfs scripts (upload-log.sh etc.)
SCRIPTS_SRC="${NATIVE_DIR}/initramfs-scripts"
if [ -d "${SCRIPTS_SRC}" ]; then
    mkdir -p "${INITRAMFS_DIR}/scripts"
    cp "${SCRIPTS_SRC}"/*.sh "${INITRAMFS_DIR}/scripts/" 2>/dev/null || true
    chmod +x "${INITRAMFS_DIR}/scripts/"*.sh 2>/dev/null || true
fi

# Bake default config.json into initramfs (handle + colors)
# This ensures "hi @handle" shows even without USB flash path or /mnt/config.json
HANDLE_CLEAN="${HANDLE#@}"
[ -z "${HANDLE_CLEAN}" ] && HANDLE_CLEAN="jeffrey"
INITRAMFS_CONFIG="${INITRAMFS_DIR}/default-config.json"
COLORS_JSON=""
if [ -n "${HANDLE_CLEAN}" ]; then
    HANDLE_URI="$(printf '%s' "${HANDLE_CLEAN}" | jq -sRr @uri 2>/dev/null || echo "${HANDLE_CLEAN}")"
    COLORS_RESP="$(curl -fsSL --connect-timeout 5 --max-time 12 \
        "${HANDLE_COLORS_API}?handle=${HANDLE_URI}" 2>/dev/null || true)"
    if [ -n "${COLORS_RESP}" ]; then
        COLORS_JSON="$(printf '%s' "${COLORS_RESP}" | jq -c '.colors // empty' 2>/dev/null || true)"
        [ "${COLORS_JSON}" = "null" ] && COLORS_JSON=""
    fi
fi
if [ -n "${COLORS_JSON}" ] && printf '%s' "${COLORS_JSON}" | jq -e 'type == "array"' >/dev/null 2>&1; then
    jq -cn --arg handle "${HANDLE_CLEAN}" --argjson colors "${COLORS_JSON}" \
        '{handle:$handle, colors:$colors}' > "${INITRAMFS_CONFIG}"
    log "Baked config.json (handle: ${HANDLE_CLEAN}, colors: $(printf '%s' "${COLORS_JSON}" | jq 'length'))"
else
    jq -cn --arg handle "${HANDLE_CLEAN}" '{handle:$handle}' > "${INITRAMFS_CONFIG}"
    log "Baked config.json (handle: ${HANDLE_CLEAN}, no colors)"
fi

# KidLisp bundling is handled by ac-os (runs as user, has npx in PATH).
# build-and-flash.sh runs under sudo where npx is unavailable.
mkdir -p "${INITRAMFS_DIR}/jslib"

# Copy shared libs (if dynamic build)
if file "${BUILD_DIR}/ac-native" | grep -q "dynamically linked"; then
    log "Copying shared libraries for dynamic binary..."
    mkdir -p "${INITRAMFS_DIR}/lib64"
    for lib in $(ldd "${BUILD_DIR}/ac-native" | grep -oP '/\S+'); do
        [ -f "$lib" ] && cp "$lib" "${INITRAMFS_DIR}/lib64/"
    done
    # Symlink /lib -> /lib64 (some lookups use /lib)
    ln -sf lib64 "${INITRAMFS_DIR}/lib"
fi

# Copy SDL2 + Mesa GPU libraries (if --sdl)
if [ "${USE_SDL}" -eq 1 ]; then
    log "Bundling SDL2 + Mesa GPU stack..."
    mkdir -p "${INITRAMFS_DIR}/lib64/dri"

    # SDL2-compat + SDL3 (Fedora uses sdl2-compat over SDL3)
    for lib in libSDL2-2.0.so.0 libSDL3.so.0; do
        src=$(readlink -f "/usr/lib64/${lib}" 2>/dev/null)
        [ -f "$src" ] && cp "$src" "${INITRAMFS_DIR}/lib64/" && ln -sf "$(basename "$src")" "${INITRAMFS_DIR}/lib64/${lib}"
    done

    # EGL + GLES + GBM (dlopen'd by SDL3 KMSDRM backend)
    for lib in libEGL.so.1 libEGL_mesa.so.0 libGLESv2.so.2 libgbm.so.1 libGL.so.1 libGLX_mesa.so.0 libGLdispatch.so.0 libglapi.so.0; do
        src=$(readlink -f "/usr/lib64/${lib}" 2>/dev/null)
        [ -f "$src" ] && cp -n "$src" "${INITRAMFS_DIR}/lib64/" 2>/dev/null && ln -sf "$(basename "$src")" "${INITRAMFS_DIR}/lib64/${lib}" 2>/dev/null
    done

    # Mesa gallium (monolithic driver — contains iris/i915/swrast)
    GALLIUM=$(ls /usr/lib64/libgallium-*.so 2>/dev/null | head -1)
    [ -f "$GALLIUM" ] && cp "$GALLIUM" "${INITRAMFS_DIR}/lib64/"

    # DRI driver stubs (Mesa loads these, which then load libgallium)
    for drv in iris_dri.so i915_dri.so kms_swrast_dri.so swrast_dri.so libdril_dri.so; do
        src="/usr/lib64/dri/${drv}"
        if [ -L "$src" ]; then
            # Copy as symlink
            tgt=$(readlink "$src")
            ln -sf "$tgt" "${INITRAMFS_DIR}/lib64/dri/${drv}"
        elif [ -f "$src" ]; then
            cp "$src" "${INITRAMFS_DIR}/lib64/dri/"
        fi
    done

    # libexpat (needed by Mesa DRI loader)
    for lib in libexpat.so.1; do
        src=$(readlink -f "/usr/lib64/${lib}" 2>/dev/null)
        [ -f "$src" ] && cp -n "$src" "${INITRAMFS_DIR}/lib64/" 2>/dev/null && ln -sf "$(basename "$src")" "${INITRAMFS_DIR}/lib64/${lib}" 2>/dev/null
    done

    GPU_SIZE=$(du -sh "${INITRAMFS_DIR}/lib64/libgallium"* "${INITRAMFS_DIR}/lib64/libSDL"* "${INITRAMFS_DIR}/lib64/libEGL"* "${INITRAMFS_DIR}/lib64/dri/" 2>/dev/null | tail -1 | cut -f1)
    log "SDL2 + Mesa GPU stack bundled (gallium: $(du -sh "${GALLIUM}" 2>/dev/null | cut -f1))"
fi

# Copy ALSA config files (required for snd_pcm_open to resolve device names)
if [ -d "/usr/share/alsa" ]; then
    log "Copying ALSA config files..."
    mkdir -p "${INITRAMFS_DIR}/usr/share/alsa"
    cp -r /usr/share/alsa/* "${INITRAMFS_DIR}/usr/share/alsa/"
    log "ALSA config: $(du -sh "${INITRAMFS_DIR}/usr/share/alsa" | cut -f1)"
else
    warn "No /usr/share/alsa found — audio may not work"
fi

# Copy WiFi components (wpa_supplicant, dhclient, iw, ip, firmware)
# Resolve actual binary paths (location varies by distro: /usr/sbin vs /usr/bin)
resolve_bin() { command -v "$1" 2>/dev/null || true; }
WIFI_BINS=(
    "$(resolve_bin wpa_supplicant)"
    "$(resolve_bin wpa_cli)"
    "$(resolve_bin dhclient)"
    "$(resolve_bin iw)"
    "$(resolve_bin ip)"
)
WIFI_COPIED=0
for bin in "${WIFI_BINS[@]}"; do
    if [ -f "$bin" ]; then
        mkdir -p "${INITRAMFS_DIR}/$(dirname "$bin")"
        cp "$bin" "${INITRAMFS_DIR}${bin}"
        chmod +x "${INITRAMFS_DIR}${bin}"
        WIFI_COPIED=$((WIFI_COPIED + 1))
        # Copy their shared libraries too
        for lib in $(ldd "$bin" 2>/dev/null | grep -oP '/\S+'); do
            [ -f "$lib" ] && cp -n "$lib" "${INITRAMFS_DIR}/lib64/" 2>/dev/null || true
        done
    fi
done
if [ "${WIFI_COPIED}" -gt 0 ]; then
    log "WiFi binaries: ${WIFI_COPIED} copied"

    # Copy Intel WiFi firmware (only latest version per card to save space)
    # 9260 = 9260-th-b0-jf-b0, AX200 = cc-a0, AX201 = QuZ-a0-hr-b0 / QuZ-a0-jf-b0
    mkdir -p "${INITRAMFS_DIR}/lib/firmware"
    # Regulatory database (required for WiFi scanning)
    for rdb in regulatory.db regulatory.db.p7s; do
        [ -f "/lib/firmware/$rdb" ] && cp "/lib/firmware/$rdb" "${INITRAMFS_DIR}/lib/firmware/"
    done
    for pattern in "iwlwifi-9260-th-b0-jf-b0-*" "iwlwifi-cc-a0-*" "iwlwifi-QuZ-a0-hr-b0-*" "iwlwifi-QuZ-a0-jf-b0-*"; do
        # Get the latest (highest version number) firmware file
        fw=$(ls -v /lib/firmware/${pattern}.ucode* 2>/dev/null | tail -1 || true)
        if [ -n "$fw" ] && [ -f "$fw" ]; then
            dest="${INITRAMFS_DIR}/lib/firmware/$(basename "${fw%.xz}")"
            xz -dk "$fw" -c > "$dest" 2>/dev/null || cp "$fw" "$dest"
            log "  firmware: $(basename "$dest") ($(du -sh "$dest" | cut -f1))"
        fi
    done
    FW_SIZE=$(du -sh "${INITRAMFS_DIR}/lib/firmware" 2>/dev/null | cut -f1)
    log "WiFi firmware: ${FW_SIZE:-0}"

    # Copy flite TTS libraries (core + slt female + kal male voices)
    for flib in libflite.so.2.2 libflite_cmulex.so.2.2 libflite_usenglish.so.2.2 libflite_cmu_us_slt.so.2.2 libflite_cmu_us_kal.so.2.2; do
        src="/usr/lib64/${flib}"
        if [ -f "$src" ]; then
            cp "$src" "${INITRAMFS_DIR}/lib64/"
            # Create soname symlinks
            ln -sf "$flib" "${INITRAMFS_DIR}/lib64/$(echo $flib | sed 's/\.so\..*/\.so/')" 2>/dev/null || true
        fi
    done
    FLITE_SIZE=$(du -sh "${INITRAMFS_DIR}/lib64/libflite"* 2>/dev/null | tail -1 | cut -f1 || true)
    log "Flite TTS: ${FLITE_SIZE:-not found}"

    # Create /var/run for wpa_supplicant
    mkdir -p "${INITRAMFS_DIR}/var/run/wpa_supplicant"

    # Minimal dhclient-script: configure interface after DHCP lease obtained
    # dhclient requires this at /sbin/dhclient-script (or via -sf flag)
    mkdir -p "${INITRAMFS_DIR}/sbin"
    cat > "${INITRAMFS_DIR}/sbin/dhclient-script" << 'DHCLIENT_SCRIPT'
#!/bin/sh
# Minimal dhclient hook for ac-native bare metal
case "$reason" in
    BOUND|RENEW|REBIND|REBOOT)
        ip addr flush dev "$interface" 2>/dev/null
        ip addr add "$new_ip_address/$new_subnet_mask" dev "$interface" 2>/dev/null
        [ -n "$new_routers" ] && ip route add default via "$new_routers" dev "$interface" 2>/dev/null
        mkdir -p /etc
        {
            # Use DHCP-provided DNS if available, always add public fallbacks
            [ -n "$new_domain_name_servers" ] && printf 'nameserver %s\n' $new_domain_name_servers
            printf 'nameserver 8.8.8.8\nnameserver 1.1.1.1\n'
        } > /etc/resolv.conf
        echo "[dhclient] BOUND $interface ip=$new_ip_address dns=$new_domain_name_servers" >> /tmp/dhclient.log
        ;;
    EXPIRE|FAIL|RELEASE|STOP)
        ip addr flush dev "$interface" 2>/dev/null
        echo "[dhclient] $reason $interface" >> /tmp/dhclient.log
        ;;
esac
exit 0
DHCLIENT_SCRIPT
    chmod +x "${INITRAMFS_DIR}/sbin/dhclient-script"
    log "dhclient-script: installed"

    # Need /bin/sh for system() calls — use busybox or link to bash if available
    mkdir -p "${INITRAMFS_DIR}/bin"
    if command -v busybox &>/dev/null; then
        cp "$(command -v busybox)" "${INITRAMFS_DIR}/bin/busybox"
        ln -sf busybox "${INITRAMFS_DIR}/bin/sh"
        for lib in $(ldd "$(command -v busybox)" 2>/dev/null | grep -oP '/\S+'); do
            [ -f "$lib" ] && cp -n "$lib" "${INITRAMFS_DIR}/lib64/" 2>/dev/null || true
        done
    elif [ -f /bin/bash ]; then
        cp /bin/bash "${INITRAMFS_DIR}/bin/sh"
        for lib in $(ldd /bin/bash 2>/dev/null | grep -oP '/\S+'); do
            [ -f "$lib" ] && cp -n "$lib" "${INITRAMFS_DIR}/lib64/" 2>/dev/null || true
        done
    fi

    # Claude Code needs /bin/bash — symlink to sh (busybox ash is close enough)
    [ -f "${INITRAMFS_DIR}/bin/sh" ] && ln -sf sh "${INITRAMFS_DIR}/bin/bash"

    # Symlink WiFi binaries into /bin/ so system() calls find them via PATH
    for bin in "${WIFI_BINS[@]}"; do
        bname="$(basename "$bin")"
        [ -f "${INITRAMFS_DIR}${bin}" ] && ln -sf "$bin" "${INITRAMFS_DIR}/bin/${bname}" 2>/dev/null || true
    done

    # Need basic utilities for shell commands (grep, awk, pgrep, killall, ls, rfkill, curl, etc.)
    for util in grep awk sed pgrep killall cat ls head cut rfkill which curl sleep mkdir chmod; do
        UTIL_PATH="$(command -v "$util" 2>/dev/null || true)"
        if [ -n "$UTIL_PATH" ] && [ -f "$UTIL_PATH" ]; then
            cp "$UTIL_PATH" "${INITRAMFS_DIR}/bin/"
            for lib in $(ldd "$UTIL_PATH" 2>/dev/null | grep -oP '/\S+'); do
                [ -f "$lib" ] && cp -n "$lib" "${INITRAMFS_DIR}/lib64/" 2>/dev/null || true
            done
        fi
    done
else
    warn "No WiFi binaries found — WiFi will not work"
fi

# ── SSH daemon (dropbear) for remote access ──
DROPBEAR_BIN="$(command -v dropbear 2>/dev/null || true)"
DROPBEARKEY_BIN="$(command -v dropbearkey 2>/dev/null || true)"
if [ -n "$DROPBEAR_BIN" ] && [ -n "$DROPBEARKEY_BIN" ]; then
    log "Bundling dropbear SSH daemon..."
    mkdir -p "${INITRAMFS_DIR}/usr/sbin" "${INITRAMFS_DIR}/etc/dropbear"
    cp "$DROPBEAR_BIN" "${INITRAMFS_DIR}/usr/sbin/dropbear"
    cp "$DROPBEARKEY_BIN" "${INITRAMFS_DIR}/usr/sbin/dropbearkey"
    chmod +x "${INITRAMFS_DIR}/usr/sbin/dropbear" "${INITRAMFS_DIR}/usr/sbin/dropbearkey"
    ln -sf /usr/sbin/dropbear "${INITRAMFS_DIR}/bin/dropbear" 2>/dev/null || true
    ln -sf /usr/sbin/dropbearkey "${INITRAMFS_DIR}/bin/dropbearkey" 2>/dev/null || true
    for lib in $(ldd "$DROPBEAR_BIN" 2>/dev/null | grep -oP '/\S+'); do
        [ -f "$lib" ] && cp -n "$lib" "${INITRAMFS_DIR}/lib64/" 2>/dev/null || true
    done
    # Create passwd/group so dropbear can resolve uid 0
    echo "root:x:0:0:root:/:/bin/sh" > "${INITRAMFS_DIR}/etc/passwd"
    echo "root:x:0:" > "${INITRAMFS_DIR}/etc/group"
    log "  dropbear: $(du -sh "${INITRAMFS_DIR}/usr/sbin/dropbear" | cut -f1)"
else
    warn "dropbear not found — SSH remote access not available"
fi

# ── Node.js + Claude Code CLI ──
NODE_BIN="$(command -v node 2>/dev/null || true)"
if [ -n "$NODE_BIN" ] && [ -f "$NODE_BIN" ]; then
    log "Bundling Node.js for Claude Code CLI..."
    cp "$NODE_BIN" "${INITRAMFS_DIR}/bin/node"
    chmod +x "${INITRAMFS_DIR}/bin/node"
    for lib in $(ldd "$NODE_BIN" 2>/dev/null | grep -oP '/\S+'); do
        [ -f "$lib" ] && cp -n "$lib" "${INITRAMFS_DIR}/lib64/" 2>/dev/null || true
    done
    NODE_SIZE=$(du -sh "${INITRAMFS_DIR}/bin/node" | cut -f1)
    log "  node: ${NODE_SIZE} ($(${NODE_BIN} --version))"

    # Bundle Claude Code (x64-linux only, strip other platforms)
    CLAUDE_PKG="$(npm root -g 2>/dev/null)/@anthropic-ai/claude-code"
    if [ -d "$CLAUDE_PKG" ]; then
        log "Bundling Claude Code CLI..."
        CLAUDE_DIR="${INITRAMFS_DIR}/opt/claude-code"
        mkdir -p "${CLAUDE_DIR}/vendor/ripgrep/x64-linux" \
                 "${CLAUDE_DIR}/vendor/tree-sitter-bash/x64-linux"
        # Core files
        cp "${CLAUDE_PKG}/cli.js" "${CLAUDE_DIR}/"
        cp "${CLAUDE_PKG}/package.json" "${CLAUDE_DIR}/"
        [ -f "${CLAUDE_PKG}/resvg.wasm" ] && cp "${CLAUDE_PKG}/resvg.wasm" "${CLAUDE_DIR}/"
        # Vendor binaries (x64-linux only)
        cp -r "${CLAUDE_PKG}/vendor/ripgrep/x64-linux/"* "${CLAUDE_DIR}/vendor/ripgrep/x64-linux/" 2>/dev/null || true
        cp -r "${CLAUDE_PKG}/vendor/tree-sitter-bash/x64-linux/"* "${CLAUDE_DIR}/vendor/tree-sitter-bash/x64-linux/" 2>/dev/null || true
        # Create claude wrapper
        cat > "${INITRAMFS_DIR}/bin/claude" << 'CLAUDE_WRAPPER'
#!/bin/sh
export NODE_PATH=/opt/claude-code
export HOME="${HOME:-/tmp}"
export TERM="${TERM:-dumb}"
exec /bin/node /opt/claude-code/cli.js "$@"
CLAUDE_WRAPPER
        chmod +x "${INITRAMFS_DIR}/bin/claude"
        CLAUDE_SIZE=$(du -sh "${CLAUDE_DIR}" | cut -f1)
        log "  claude-code: ${CLAUDE_SIZE} (x64-linux only)"
    else
        warn "Claude Code not found globally — run: npm i -g @anthropic-ai/claude-code"
    fi
else
    warn "Node.js not found — Claude Code CLI not available"
fi

# Copy CA trust bundle for HTTPS curl calls (OS update/clock/version checks).
CA_BUNDLE_SRC=""
for p in /etc/pki/tls/certs/ca-bundle.crt /etc/ssl/certs/ca-certificates.crt; do
    if [ -f "$p" ]; then
        CA_BUNDLE_SRC="$p"
        break
    fi
done
if [ -n "${CA_BUNDLE_SRC}" ]; then
    mkdir -p "${INITRAMFS_DIR}/etc/pki/tls/certs" \
             "${INITRAMFS_DIR}/etc/pki/tls" \
             "${INITRAMFS_DIR}/etc/ssl/certs"
    cp "${CA_BUNDLE_SRC}" "${INITRAMFS_DIR}/etc/pki/tls/certs/ca-bundle.crt"
    cp "${CA_BUNDLE_SRC}" "${INITRAMFS_DIR}/etc/pki/tls/cert.pem"
    cp "${CA_BUNDLE_SRC}" "${INITRAMFS_DIR}/etc/ssl/certs/ca-certificates.crt"
    log "CA bundle installed: /etc/pki/tls/certs/ca-bundle.crt"
else
    warn "No CA bundle found — HTTPS fetches may fail"
fi

# Bundle ac-native's own shared library dependencies (needed when built dynamically)
# Copies each .so to its canonical path inside initramfs, preserving directory structure
if [ -f "${BUILD_DIR}/ac-native" ]; then
    for lib in $(ldd "${BUILD_DIR}/ac-native" 2>/dev/null | grep -oP '/[^ ()]+\.so[^ ()]*'); do
        [ -f "$lib" ] || continue
        dest_dir="${INITRAMFS_DIR}$(dirname "$lib")"
        mkdir -p "$dest_dir"
        cp -n "$lib" "$dest_dir/" 2>/dev/null || true
        # Also create unversioned .so symlink so dlopen() finds it
        base="$(basename "$lib")"
        novers="$(echo "$base" | sed 's/\.so\..*/\.so/')"
        [ "$novers" != "$base" ] && ln -sf "$base" "$dest_dir/$novers" 2>/dev/null || true
    done
    log "Bundled $(ldd "${BUILD_DIR}/ac-native" 2>/dev/null | grep -c '\.so') shared libs"
fi

# Init: symlink to ac-native (no /bin/sh in initramfs)
ln -sf /ac-native "${INITRAMFS_DIR}/init"

# Create cpio + lz4
INITRAMFS_CPIO="${BUILD_DIR}/initramfs.cpio"
cd "${INITRAMFS_DIR}"
find . -print0 | cpio --null -ov --format=newc > "${INITRAMFS_CPIO}" 2>/dev/null
lz4 -l -9 -f "${INITRAMFS_CPIO}" "${INITRAMFS_CPIO}.lz4"
CPIO_SIZE=$(wc -c < "${INITRAMFS_CPIO}.lz4")
log "Initramfs: ${CPIO_SIZE} bytes (LZ4)"

# ============================================================
# Step 4: Build kernel with embedded initramfs
# ============================================================
KERNEL_SRC="${BUILD_DIR}/linux-${KERNEL_VERSION}"
VMLINUZ="${BUILD_DIR}/vmlinuz"

if [ "${SKIP_KERNEL}" -eq 0 ] || [ ! -f "${VMLINUZ}" ]; then
    log "Building kernel ${KERNEL_VERSION} with embedded initramfs..."

    # Download kernel source if needed
    KERNEL_TAR="${BUILD_DIR}/linux-${KERNEL_VERSION}.tar.xz"
    if [ ! -f "${KERNEL_TAR}" ]; then
        log "Downloading kernel..."
        curl -L "https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-${KERNEL_VERSION}.tar.xz" -o "${KERNEL_TAR}"
    fi
    if [ ! -d "${KERNEL_SRC}" ]; then
        log "Extracting kernel..."
        tar xf "${KERNEL_TAR}" -C "${BUILD_DIR}"
    fi

    # Apply config
    cp "${KERNEL_DIR}/config-minimal" "${KERNEL_SRC}/.config"

    # Set initramfs source path
    INITRAMFS_ABS="$(cd "${BUILD_DIR}" && pwd)/initramfs.cpio.lz4"
    echo "CONFIG_INITRAMFS_SOURCE=\"${INITRAMFS_ABS}\"" >> "${KERNEL_SRC}/.config"

    # Finalize config
    cd "${KERNEL_SRC}"
    make olddefconfig

    # Build
    JOBS="${JOBS:-$(nproc)}"
    log "Compiling kernel (${JOBS} jobs)..."
    make -j"${JOBS}" bzImage

    # Copy output
    BZIMAGE="${KERNEL_SRC}/arch/x86/boot/bzImage"
    if [ -f "${BZIMAGE}" ]; then
        cp "${BZIMAGE}" "${VMLINUZ}"
    else
        err "bzImage not found!"
        exit 1
    fi
else
    log "Skipping kernel build (using existing vmlinuz)"
fi

KERNEL_SIZE=$(wc -c < "${VMLINUZ}")
log "Kernel: $(( KERNEL_SIZE / 1024 / 1024 ))MB (${KERNEL_SIZE} bytes)"

# ============================================================
# Step 5: Flash to USB (if --flash specified)
# ============================================================
if [ -n "${FLASH_DEV}" ]; then
    if [ ! -b "${FLASH_DEV}" ]; then
        err "${FLASH_DEV} is not a block device"
        exit 1
    fi

    log "Flashing to ${FLASH_DEV}..."
    warn "This will ERASE ALL DATA on ${FLASH_DEV}"

    # Create GPT + EFI System Partition
    sfdisk "${FLASH_DEV}" << 'PART_EOF'
label: gpt
type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, size=512M
PART_EOF
    sync

    # Wait for partition
    PART="${FLASH_DEV}1"
    for i in $(seq 1 20); do
        [ -b "${PART}" ] && break
        sleep 0.3
    done

    if [ ! -b "${PART}" ]; then
        err "Partition ${PART} not found"
        exit 1
    fi

    # Format FAT32
    mkfs.vfat -F 32 -n "AC-NATIVE" "${PART}"

    # Copy kernel + splash chainloader using mtools (no mount needed)
    mmd -i "${PART}" ::EFI
    mmd -i "${PART}" ::EFI/BOOT
    SPLASH_EFI="${NATIVE_DIR}/bootloader/splash.efi"
    if [ -f "${SPLASH_EFI}" ]; then
        log "Using splash chainloader (${SPLASH_EFI})"
        mcopy -i "${PART}" "${SPLASH_EFI}" ::EFI/BOOT/BOOTX64.EFI
        mcopy -i "${PART}" "${VMLINUZ}" ::EFI/BOOT/KERNEL.EFI
    else
        log "No splash chainloader found, using kernel directly"
        mcopy -i "${PART}" "${VMLINUZ}" ::EFI/BOOT/BOOTX64.EFI
    fi

    # Write config.json with handle + optional per-char colors from handle-colors API
    HANDLE_CLEAN="${HANDLE#@}"
    [ -z "${HANDLE_CLEAN}" ] && HANDLE_CLEAN="jeffrey"
    CONFIG_TMP=$(mktemp)
    COLORS_JSON=""
    if [ -n "${HANDLE_CLEAN}" ]; then
        HANDLE_URI="$(printf '%s' "${HANDLE_CLEAN}" | jq -sRr @uri)"
        COLORS_RESP="$(curl -fsSL --connect-timeout 5 --max-time 12 \
            "${HANDLE_COLORS_API}?handle=${HANDLE_URI}" 2>/dev/null || true)"
        if [ -n "${COLORS_RESP}" ]; then
            COLORS_JSON="$(printf '%s' "${COLORS_RESP}" | jq -c '.colors // empty' 2>/dev/null || true)"
            [ "${COLORS_JSON}" = "null" ] && COLORS_JSON=""
        fi
    fi

    if [ -n "${COLORS_JSON}" ] && printf '%s' "${COLORS_JSON}" | jq -e 'type == "array"' >/dev/null 2>&1; then
        jq -cn --arg handle "${HANDLE_CLEAN}" --argjson colors "${COLORS_JSON}" \
            '{handle:$handle, colors:$colors}' > "${CONFIG_TMP}"
        COLOR_COUNT="$(printf '%s' "${COLORS_JSON}" | jq 'length')"
        log "Fetched ${COLOR_COUNT} boot colors for @${HANDLE_CLEAN}"
    else
        jq -cn --arg handle "${HANDLE_CLEAN}" '{handle:$handle}' > "${CONFIG_TMP}"
        warn "No handle colors for @${HANDLE_CLEAN} (using fallback rainbow title)"
    fi

    mcopy -i "${PART}" "${CONFIG_TMP}" ::config.json
    rm -f "${CONFIG_TMP}"
    log "Wrote config.json (handle: ${HANDLE_CLEAN})"

    sync
    log "Flashed! Kernel at EFI/BOOT/BOOTX64.EFI on ${FLASH_DEV}"
    log "Remove USB and boot from it (UEFI, Secure Boot OFF)."
else
    log ""
    log "=== Build complete ==="
    log "Kernel: ${VMLINUZ}"
    log ""
    log "To flash to USB:"
    log "  $0 --flash /dev/sdX"
    log ""
    log "Or manually with mtools:"
    log "  sfdisk /dev/sdX < <(echo 'label: gpt'; echo 'type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, size=512M')"
    log "  mkfs.vfat -F 32 -n AC-NATIVE /dev/sdX1"
    log "  mmd -i /dev/sdX1 ::EFI ::EFI/BOOT"
    log "  mcopy -i /dev/sdX1 ${VMLINUZ} ::EFI/BOOT/BOOTX64.EFI"
fi
