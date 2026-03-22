## AC Native OS — Reproducible Build Container
##
## Usage (from repo root):
##   docker build -t ac-os-builder -f fedac/native/Dockerfile.builder .
##   docker run --rm -v $(pwd)/out:/out ac-os-builder
##
## The source is COPY'd into the image — no bind mounts needed.
## Output: /out/vmlinuz + /out/build.json

FROM fedora:43

# ── All packages needed for ac-native + kernel build ──
RUN dnf install -y --setopt=install_weak_deps=False \
    gcc make cpio lz4 bc perl flex bison diffutils \
    elfutils-libelf-devel openssl-devel \
    curl jq git tar xz findutils pkgconf-pkg-config zstd \
    ca-certificates \
    alsa-lib-devel libdrm-devel flite-devel \
    ffmpeg-free-devel \
    wpa_supplicant dhcp-client iw \
    iwlwifi-mvm-firmware wireless-regdb \
    linux-firmware linux-firmware-whence \
    intel-gpu-firmware \
    busybox mtools dosfstools \
    mesa-libgbm mesa-libgbm-devel \
    mesa-libEGL mesa-libEGL-devel \
    mesa-libGLES mesa-libGLES-devel \
    mesa-dri-drivers \
    alsa-lib alsa-firmware alsa-ucm \
    openssl nodejs npm dropbear efibootmgr \
    && dnf clean all && rm -rf /var/cache/dnf

# ── Pre-download QuickJS + Linux kernel source ──
RUN mkdir -p /cache && cd /cache \
    && curl -sL https://bellard.org/quickjs/quickjs-2024-01-13.tar.xz | tar xJ \
    && ln -sf quickjs-2024-01-13 quickjs \
    && curl -sL https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-6.19.9.tar.xz | tar xJ \
    && echo "=== Cached: QuickJS + Linux 6.19.9 ==="

# ── Install esbuild for KidLisp bundling ──
RUN npm install -g esbuild

# ── Verify tools ──
RUN gcc --version | head -1 && busybox --help >/dev/null 2>&1 && esbuild --version && echo "OK"

# ── Copy source into image ──
COPY . /repo

# ── Copy build script ──
COPY fedac/native/docker-build.sh /docker-build.sh
RUN chmod +x /docker-build.sh

ENV AC_SRC=/repo
ENTRYPOINT ["/docker-build.sh"]
