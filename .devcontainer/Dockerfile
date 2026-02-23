# Rebuilt to run SmartPy on Python 3.11 and avoid 3.13 BrokenPipe
FROM --platform=linux/amd64 fedora:latest

# Preconfigure DNF to minimize footprint and avoid unnecessary optional deps
RUN set -eux; \
    echo 'fastestmirror=True' >> /etc/dnf/dnf.conf; \
    echo 'max_parallel_downloads=5' >> /etc/dnf/dnf.conf; \
    echo 'keepcache=0' >> /etc/dnf/dnf.conf; \
    echo 'install_weak_deps=False' >> /etc/dnf/dnf.conf; \
    echo 'tsflags=nodocs' >> /etc/dnf/dnf.conf

ENV TZ=America/Los_Angeles
ENV NODE_OPTIONS=--max_old_space_size=8192
ENV DENO_DIR=/home/me/.cache/deno
ENV NETLIFY_DENO_VERSION=2.4.5
# Prevent paging for LLM/agentic use
ENV PAGER=cat
ENV GIT_PAGER=cat
ENV SYSTEMD_PAGER=cat
ENV MANPAGER=cat

# --- Base toolchain & deps ----------------------------------------------------
RUN dnf clean all && rm -rf /var/cache/dnf && \
    dnf update -y --refresh --setopt=zchunk=false && \
    dnf install -y \
      awk which tree procps-ng htop coreutils unzip jq bat ripgrep \
      git gh gcc gcc-c++ clang make cmake \
      python3.11 python3.11-devel python3-pip python3-devel \
      emacs-nox fish util-linux-user util-linux-script sshpass \
      qemu-kvm qemu-img qemu-system-x86 qemu-user-static \
      openssl openssl-devel awscli redis \
      dnf-plugins-core lolcat toilet iproute xclip nmap-ncat \
      inotify-tools wget curl tar gzip bzip2 \
      libsodium libsodium-devel libffi libffi-devel \
      gmp gmp-devel \
      pkg-config autoconf automake libtool \
      ocaml opam rsync m4 patch bubblewrap \
      libX11 libxkbfile libsecret libxshmfence libXtst \
      nss atk pango cups-libs libXcomposite libXcursor \
      libXi libXdamage libXrandr alsa-lib gtk3 \
      xorg-x11-server-Xvfb psmisc lsof chromium \
      sbcl zeromq-devel redhat-rpm-config \
      poppler-utils \
            caddy \
        && dnf clean all && rm -rf /var/cache/dnf

# --- FFmpeg from RPM Fusion ---------------------------------------------------
RUN dnf install -y --setopt=zchunk=false \
      https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm \
      https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm \
    && dnf install -y --allowerasing --skip-unavailable --setopt=zchunk=false ffmpeg ffmpeg-devel \
        && dnf clean all && rm -rf /var/cache/dnf

# --- Python 3.11 venv as default "python" -------------------------------------
RUN /usr/bin/python3.11 -m venv /opt/py311 && \
    /opt/py311/bin/pip install -U pip setuptools wheel && \
    ln -s /opt/py311/bin/python /usr/local/bin/python && \
    ln -s /opt/py311/bin/pip /usr/local/bin/pip
ENV PATH="/opt/py311/bin:${PATH}"

# --- SmartPy (pinned) + smoke test -------------------------------------------
# Note: SmartPy doesn't support ARM64 architecture (Apple Silicon)
RUN set -eux; \
    if [ "$(uname -m)" = "x86_64" ]; then \
        pip install "tezos-smartpy==0.22.0" "pytezos>=3.9.0,<4" requests && \
        python -c "import smartpy as sp; print('SmartPy successfully imported')"; \
    else \
        echo "Skipping SmartPy on $(uname -m) architecture (not supported)"; \
        pip install "pytezos>=3.9.0,<4" requests; \
    fi

# --- Octez (best-effort binary) ----------------------------------------------
# Note: v20.3 doesn't support current Ghostnet protocol (PtSeouLouXkxhg39...)
# Use Docker method instead: deploy-via-docker.py
RUN set -eux; \
    if curl -fsSL -o /usr/local/bin/octez-client \
         https://github.com/serokell/tezos-packaging/releases/download/v20.3-1/octez-client && \
       curl -fsSL -o /usr/local/bin/octez-node \
         https://github.com/serokell/tezos-packaging/releases/download/v20.3-1/octez-node; then \
      chmod +x /usr/local/bin/octez-*; \
    else \
      echo "Octez binaries unavailable, skipping"; \
    fi

# --- Docker CLI (for running latest Octez via Docker) ------------------------
# This allows us to use tezos/tezos:master which supports current Ghostnet protocol
RUN dnf install -y --setopt=zchunk=false docker && dnf clean all && rm -rf /var/cache/dnf

# Pre-pull Tezos & SmartPy Docker images during build (saves time later)
# Note: This requires BuildKit and may fail in some build environments
# If it fails, the images will be pulled on first use instead
RUN --mount=type=cache,target=/var/cache/buildkit \
    docker pull tezos/tezos:master 2>/dev/null || echo "Octez image will be pulled on first use" && \
    docker pull bakingbad/smartpy-cli:latest 2>/dev/null || echo "SmartPy image will be pulled on first use"

# --- Stripe CLI (architecture-aware) -----------------------------------------
RUN set -eux; \
    case "$(uname -m)" in \
        x86_64) ARCH=x86_64 ;; \
        aarch64) ARCH=arm64 ;; \
        *) echo "Unsupported architecture: $(uname -m)" && exit 1 ;; \
    esac && \
    curl -fsSL "https://github.com/stripe/stripe-cli/releases/download/v1.30.0/stripe_1.30.0_linux_${ARCH}.tar.gz" \
    | tar xz -C /usr/local/bin

# --- redli (architecture-aware) ----------------------------------------------
RUN set -eux; \
    case "$(uname -m)" in \
        x86_64) ARCH=amd64 ;; \
        aarch64) ARCH=arm64 ;; \
        *) echo "Unsupported architecture: $(uname -m)" && exit 1 ;; \
    esac && \
    curl -fsSL "https://github.com/IBM-Cloud/redli/releases/download/v0.15.0/redli_0.15.0_linux_${ARCH}.tar.gz" \
    | tar xz -C /usr/local/bin && mv "/usr/local/bin/redli_linux_${ARCH}" /usr/local/bin/redli

# --- mkcert (pre-built binary from GitHub) -----------------------------------
RUN set -eux; \
    case "$(uname -m)" in \
        x86_64) ARCH=amd64 ;; \
        aarch64) ARCH=arm64 ;; \
        *) echo "Unsupported architecture: $(uname -m)" && exit 1 ;; \
    esac && \
    curl -fsSL "https://github.com/FiloSottile/mkcert/releases/download/v1.4.4/mkcert-v1.4.4-linux-${ARCH}" -o /usr/local/bin/mkcert && \
    chmod +x /usr/local/bin/mkcert

# --- ngrok (architecture-aware) ----------------------------------------------
RUN set -eux; \
    case "$(uname -m)" in \
        x86_64) ARCH=amd64 ;; \
        aarch64) ARCH=arm64 ;; \
        *) echo "Unsupported architecture: $(uname -m)" && exit 1 ;; \
    esac && \
    curl -fsSL "https://bin.equinox.io/c/bNyj1mQVY4c/ngrok-v3-stable-linux-${ARCH}.tgz" -o /tmp/ngrok.tgz && \
    tar -xzf /tmp/ngrok.tgz -C /usr/local/bin && rm /tmp/ngrok.tgz

# --- Ollama (architecture-aware) ---------------------------------------------
RUN set -eux; \
    curl -fsSL https://ollama.com/install.sh | sh

# --- doctl (DigitalOcean CLI, architecture-aware) ----------------------------
RUN set -eux; \
    case "$(uname -m)" in \
        x86_64) ARCH=amd64 ;; \
        aarch64) ARCH=arm64 ;; \
        *) echo "Unsupported architecture: $(uname -m)" && exit 1 ;; \
    esac && \
    DOCTL_VERSION=1.109.0 && \
    curl -fsSL "https://github.com/digitalocean/doctl/releases/download/v${DOCTL_VERSION}/doctl-${DOCTL_VERSION}-linux-${ARCH}.tar.gz" -o /tmp/doctl.tar.gz && \
    tar -xzf /tmp/doctl.tar.gz -C /usr/local/bin && rm /tmp/doctl.tar.gz && \
    chmod +x /usr/local/bin/doctl

# --- PowerShell (for running Windows scripts on Linux) -----------------------
# Install from GitHub releases since Microsoft's repo doesn't have Fedora 43 packages yet
RUN ARCH=$(uname -m) && \
    if [ "$ARCH" = "x86_64" ]; then PS_ARCH="x64"; else PS_ARCH="arm64"; fi && \
    PS_VERSION=$(curl -s https://api.github.com/repos/PowerShell/PowerShell/releases/latest | grep '"tag_name":' | sed -E 's/.*"v([^"]+)".*/\1/') && \
    curl -sSL -o /tmp/powershell.tar.gz "https://github.com/PowerShell/PowerShell/releases/download/v${PS_VERSION}/powershell-${PS_VERSION}-linux-${PS_ARCH}.tar.gz" && \
    mkdir -p /opt/microsoft/powershell/7 && \
    tar -xzf /tmp/powershell.tar.gz -C /opt/microsoft/powershell/7 && \
    chmod +x /opt/microsoft/powershell/7/pwsh && \
    ln -s /opt/microsoft/powershell/7/pwsh /usr/local/bin/pwsh && \
    ln -s /opt/microsoft/powershell/7/pwsh /usr/local/bin/powershell && \
    rm /tmp/powershell.tar.gz

# --- dbxcli (Dropbox CLI) -----------------------------------------------------
RUN set -eux; \
    curl -fsSL "https://github.com/dropbox/dbxcli/releases/download/v3.0.0/dbxcli-linux-amd64" \
      -o /usr/local/bin/dbxcli && \
    chmod +x /usr/local/bin/dbxcli

# --- s3cmd (for DigitalOcean Spaces management) -------------------------------
RUN dnf install -y --setopt=zchunk=false s3cmd && dnf clean all

# --- Jupyter bits -------------------------------------------------------------
RUN dnf install -y --setopt=zchunk=false python3-notebook python3-nbconvert && dnf clean all

# --- gcloud SDK ---------------------------------------------------------------
RUN curl -sSL https://sdk.cloud.google.com > /tmp/install_gcloud.sh && \
    bash /tmp/install_gcloud.sh --disable-prompts --install-dir=/opt && \
    ln -s /opt/google-cloud-sdk/bin/gcloud /usr/local/bin/gcloud && \
    ln -s /opt/google-cloud-sdk/bin/gsutil /usr/local/bin/gsutil && \
    ln -s /opt/google-cloud-sdk/bin/bq /usr/local/bin/bq && \
    rm /tmp/install_gcloud.sh

# --- User ---------------------------------------------------------------------
RUN useradd -m me && echo "me ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers && chsh -s /usr/bin/fish me

# Fix fish path issue: Fedora installs to /usr/sbin but shebangs expect /usr/bin
RUN if test ! -f /usr/bin/fish && test -f /usr/sbin/fish; then \
        ln -s /usr/sbin/fish /usr/bin/fish; \
    fi

USER me
WORKDIR /home/me

# --- Rust + websocat (built from source) --------------------------------------
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y && \
    . "$HOME/.cargo/env" && \
    git clone https://github.com/vi/websocat.git && \
    cd websocat && cargo build --release && sudo cp target/release/websocat /usr/local/bin/

# --- fnm / Node toolchain (PART 1: Create directories with correct ownership FIRST) ---
RUN mkdir -p /home/me/.config/fish/conf.d /home/me/.config/fish/functions && \
    mkdir -p /home/me/.local/share/fish && \
    chown -R me:me /home/me/.config && \
    chown -R me:me /home/me/.local && \
    chmod -R 755 /home/me/.config && \
    chmod -R 755 /home/me/.local

# --- fnm / Node toolchain (PART 2: Now install fnm and node) ------------------
RUN curl -fsSL https://fnm.vercel.app/install | bash -s -- --install-dir "$HOME/.fnm" --skip-shell
ENV PATH="/home/me/.fnm:${PATH}"
RUN echo 'fnm env --use-on-cd --shell fish | source' > /home/me/.config/fish/conf.d/fnm.fish && \
    export PATH="/home/me/.fnm:${PATH}" && \
    eval "$(/home/me/.fnm/fnm env --use-on-cd)" && \
    /home/me/.fnm/fnm install lts-jod && \
    /home/me/.fnm/fnm install 20.5.0 && \
    /home/me/.fnm/fnm default lts-jod && \
    /home/me/.fnm/fnm use lts-jod && \
    npm i -g prettier typescript typescript-language-server npm-check-updates @anthropic-ai/claude-code @openai/codex netlify-cli wrangler && \
    echo 'fnm use lts-jod' >> /home/me/.config/fish/config.fish && \
    chown -R me:me /home/me/.config/fish

# --- ATProto dependencies for /at directory ------------------------------------
# Note: These will be available globally for ATProto experiments
# See /at directory for ATProto/Bluesky exploration tools
RUN export PATH="/home/me/.fnm:${PATH}" && \
    eval "$(/home/me/.fnm/fnm env --use-on-cd)" && \
    npm i -g @atproto/api@^0.17.0 @atproto/xrpc@^0.6.5 @atproto/lexicon@^0.4.2 @atproto/identity@^0.4.3 @atproto/syntax@^0.3.1 dotenv@^16.4.5

# --- SBCL / Quicklisp ---------------------------------------------------------
RUN curl -fsSLO https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --non-interactive \
         --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
         --eval '(quit)'

# --- OPS (nanos) --------------------------------------------------------------
# Allow failure - ops.city can have intermittent TLS issues
RUN curl https://ops.city/get.sh -sSfL | bash || echo "OPS installation failed, skipping..."

# --- uv (fast Python package manager for MCP servers) -------------------------
# --- emacs-mcp-server (for VS Code Copilot <-> Emacs integration) -------------
# Combined into single RUN to ensure uv is available immediately after install
RUN curl -LsSf https://astral.sh/uv/install.sh | sh && \
    export PATH="$HOME/.local/bin:$PATH" && \
    uv --version && \
    git clone https://github.com/vivekhaldar/emacs-mcp-server.git /home/me/emacs-mcp-server && \
    cd /home/me/emacs-mcp-server && \
    uv sync
ENV PATH="/home/me/.local/bin:${PATH}"

# --- Deno ---------------------------------------------------------------------
RUN curl -fsSL https://deno.land/install.sh | DENO_INSTALL=/home/me/.deno sh -s v2.4.5 && \
    /home/me/.deno/bin/deno --version && \
    /home/me/.deno/bin/deno cache https://v2-13-0--edge.netlify.com/bootstrap/index-combined.ts 2>/dev/null || true && \
    /home/me/.deno/bin/deno cache https://v2-13-0--edge.netlify.com/bootstrap/handler.ts 2>/dev/null || true && \
    /home/me/.deno/bin/deno cache https://v2-13-0--edge.netlify.com/bootstrap/server.ts 2>/dev/null || true

# --- Fish config / entry ------------------------------------------------------
# Directories were already created above during fnm setup, now just copy files
COPY --chown=me:me config.fish /home/me/.config/fish/config.fish
COPY --chown=me:me entry.fish /entry.fish

# Ensure jupyter directory exists with correct permissions
RUN mkdir -p /home/me/.jupyter && \
    chmod -R u+rw /home/me/.jupyter

# --- IPython preload ----------------------------------------------------------
RUN mkdir -p /home/me/.ipython/profile_default/startup && \
    echo "import sys; sys.path.append('/workspaces/aesthetic-computer/notebook'); import aesthetic" > /home/me/.ipython/profile_default/startup/00-aesthetic.py && \
    sudo chown -R me:me /home/me/.ipython

# --- GYP shim (legacy consumers) ---------------------------------------------
RUN mkdir -p /home/me/.local/lib/python3.10/site-packages && \
    git clone https://chromium.googlesource.com/external/gyp.git /home/me/.local/lib/python3.10/site-packages/gyp || true && \
    chown -R me:me /home/me/.local

# Optional: default shell session
# ENTRYPOINT ["/usr/bin/fish", "/entry.fish"]