## AC Native OS — Reproducible Build Container (make-clean)
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
    gcc ccache make cpio lz4 bc perl flex bison diffutils \
    elfutils-libelf-devel openssl-devel \
    gnu-efi-devel \
    curl jq git tar xz findutils pkgconf-pkg-config zstd xorriso \
    ca-certificates \
    alsa-lib-devel libdrm-devel flite-devel SDL3-devel \
    ffmpeg-free-devel \
    wpa_supplicant dhcp-client iw \
    iwlwifi-mvm-firmware wireless-regdb \
    linux-firmware linux-firmware-whence \
    intel-gpu-firmware \
    busybox mtools dosfstools hfsplus-tools hfsutils gdisk \
    mesa-libgbm mesa-libgbm-devel \
    mesa-libEGL mesa-libEGL-devel \
    mesa-libGLES mesa-libGLES-devel \
    mesa-dri-drivers \
    alsa-lib alsa-firmware alsa-ucm \
    openssl nodejs npm dropbear efibootmgr \
    sbcl \
    && dnf clean all && rm -rf /var/cache/dnf

# ── Install Quicklisp + CL dependencies for Lisp build variant ──
RUN mkdir -p /cache \
    && curl -sfo /tmp/ql.lisp https://beta.quicklisp.org/quicklisp.lisp \
    && sbcl --non-interactive \
       --load /tmp/ql.lisp \
       --eval '(quicklisp-quickstart:install :path "/opt/quicklisp/")' \
       --eval '(ql:quickload (list :cffi :bordeaux-threads :alexandria :swank))' \
    && rm /tmp/ql.lisp

# ── Pre-download QuickJS + Linux kernel source ──
RUN mkdir -p /cache && cd /cache \
    && curl -sL https://bellard.org/quickjs/quickjs-2024-01-13.tar.xz | tar xJ \
    && ln -sf quickjs-2024-01-13 quickjs \
    && curl -sL https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-6.19.9.tar.xz | tar xJ \
    && echo "=== Cached: QuickJS + Linux 6.19.9 ==="

# ── Install esbuild for KidLisp bundling ──
RUN npm install -g esbuild

# ── Install Claude Code CLI (native binary) ──
RUN curl -fsSL https://claude.ai/install.sh | bash 2>/dev/null \
    && CLAUDE_BIN=$(find /root/.local/share/claude/versions -type f 2>/dev/null | sort -V | tail -1) \
    && if [ -n "$CLAUDE_BIN" ]; then cp "$CLAUDE_BIN" /usr/local/bin/claude-native && chmod +x /usr/local/bin/claude-native; fi \
    || echo "Claude Code install skipped (non-fatal)"

# ── Verify tools ──
RUN gcc --version | head -1 && busybox --help >/dev/null 2>&1 && esbuild --version && echo "OK"

# ── Copy source into image ──
COPY . /repo

# ── Copy build script ──
COPY fedac/native/docker-build.sh /docker-build.sh
RUN chmod +x /docker-build.sh

ENV AC_SRC=/repo
ENTRYPOINT ["/docker-build.sh"]
# cache-bust 1774407968
