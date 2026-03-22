## AC Native OS — Reproducible Build Container
##
## Build the image (one-time, ~5 min):
##   docker build -t ac-os-builder -f Dockerfile.builder .
##
## Build a kernel (input: source, output: vmlinuz):
##   docker run --rm -v $(pwd):/src -v $(pwd)/out:/out ac-os-builder
##
## Build + flash USB (needs --privileged for /dev access):
##   docker run --rm --privileged -v /dev:/dev -v $(pwd):/src ac-os-builder --flash /dev/sda
##
## With custom options:
##   docker run --rm -v $(pwd):/src -v $(pwd)/out:/out ac-os-builder \
##     --handle jeffrey --piece pieces/notepat.mjs
##
## Environment variables:
##   AC_BUILD_NAME   Override build name (otherwise auto-generated)
##   AC_GIT_HASH     Git hash to embed in binary
##   AC_BUILD_TS     Build timestamp
##   AC_HANDLE       User handle (default: jeffrey)

FROM fedora:43

# ── All packages needed for ac-native + kernel build ──
# Pinned to Fedora 43 for reproducibility.
# Comments match build-and-flash.sh dependency checks.
RUN dnf install -y --setopt=install_weak_deps=False \
    # Build toolchain
    gcc make cpio lz4 bc perl flex bison \
    elfutils-libelf-devel openssl-devel \
    curl jq git tar xz findutils pkgconf-pkg-config \
    ca-certificates \
    # ac-native C library dependencies
    alsa-lib-devel libdrm-devel flite-devel \
    ffmpeg-free-devel \
    # WiFi tools (copied into initramfs)
    wpa_supplicant dhcp-client iw \
    # WiFi + GPU firmware
    iwlwifi-mvm-firmware wireless-regdb \
    linux-firmware \
    # Initramfs essentials
    busybox mtools dosfstools \
    # Mesa GPU userspace (DRM needs these at runtime)
    mesa-libgbm mesa-libgbm-devel \
    mesa-libEGL mesa-libEGL-devel \
    mesa-libGLES mesa-libGLES-devel \
    mesa-dri-drivers \
    # Audio
    alsa-lib alsa-firmware alsa-ucm \
    # SSL/TLS runtime
    openssl \
    # Node.js for KidLisp bundling
    nodejs npm \
    # SSH server (dropbear in initramfs)
    dropbear \
    # EFI boot manager
    efibootmgr \
    && dnf clean all \
    && rm -rf /var/cache/dnf

# ── Verify critical tools exist ──
RUN gcc --version | head -1 \
    && busybox --help >/dev/null 2>&1 \
    && pkg-config --exists alsa libdrm \
    && node --version \
    && echo "=== Build environment OK ==="

# ── Entrypoint ──
COPY docker-build.sh /docker-build.sh
RUN chmod +x /docker-build.sh

ENTRYPOINT ["/docker-build.sh"]
