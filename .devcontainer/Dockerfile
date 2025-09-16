# Rebuilt to run SmartPy on Python 3.11 and avoid 3.13 BrokenPipe
FROM fedora:latest

ENV NODE_OPTIONS=--max_old_space_size=8192
ENV DENO_DIR=/home/me/.cache/deno
ENV NETLIFY_DENO_VERSION=1.45.0
ENV NETLIFY_CLI_TELEMETRY_DISABLED=1
ENV NODE_DISABLE_COMPILE_CACHE=1
ENV PIP_DISABLE_PIP_VERSION_CHECK=1
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

# --- Base toolchain & deps ----------------------------------------------------
RUN dnf update -y && \
    dnf install -y \
      awk which tree procps-ng coreutils unzip jq bat \
      git gh gcc gcc-c++ clang make cmake \
      python3.11 python3.11-devel python3-pip python3-devel \
      emacs-nox fish util-linux-user \
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
      caddy \
    && dnf clean all

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
RUN set -eux; \
    if curl -fsSL -o /usr/local/bin/octez-client \
         https://github.com/serokell/tezos-packaging/releases/download/v20.3-1/octez-client && \
       curl -fsSL -o /usr/local/bin/octez-node \
         https://github.com/serokell/tezos-packaging/releases/download/v20.3-1/octez-node; then \
      chmod +x /usr/local/bin/octez-*; \
    else \
      echo "Octez binaries unavailable, skipping"; \
    fi

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

# --- mkcert (architecture-aware) ---------------------------------------------
RUN set -eux; \
    case "$(uname -m)" in \
        x86_64) ARCH=amd64 ;; \
        aarch64) ARCH=arm64 ;; \
        *) echo "Unsupported architecture: $(uname -m)" && exit 1 ;; \
    esac && \
    curl -fsSL "https://github.com/FiloSottile/mkcert/releases/download/v1.4.4/mkcert-v1.4.4-linux-${ARCH}" \
    -o /usr/local/bin/mkcert && chmod +x /usr/local/bin/mkcert

# --- ngrok (architecture-aware) ----------------------------------------------
RUN set -eux; \
    case "$(uname -m)" in \
        x86_64) ARCH=amd64 ;; \
        aarch64) ARCH=arm64 ;; \
        *) echo "Unsupported architecture: $(uname -m)" && exit 1 ;; \
    esac && \
    curl -fsSL "https://bin.equinox.io/c/bNyj1mQVY4c/ngrok-v3-stable-linux-${ARCH}.tgz" -o /tmp/ngrok.tgz && \
    tar -xzf /tmp/ngrok.tgz -C /usr/local/bin && rm /tmp/ngrok.tgz

# --- Jupyter bits -------------------------------------------------------------
RUN dnf install -y python3-notebook python3-nbconvert && dnf clean all

# --- gcloud SDK ---------------------------------------------------------------
RUN curl -sSL https://sdk.cloud.google.com > /tmp/install_gcloud.sh && \
    bash /tmp/install_gcloud.sh --disable-prompts --install-dir=/opt && \
    ln -s /opt/google-cloud-sdk/bin/gcloud /usr/local/bin/gcloud && \
    ln -s /opt/google-cloud-sdk/bin/gsutil /usr/local/bin/gsutil && \
    ln -s /opt/google-cloud-sdk/bin/bq /usr/local/bin/bq && \
    rm /tmp/install_gcloud.sh

# --- User ---------------------------------------------------------------------
RUN useradd -m me && echo "me ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers && chsh -s /usr/bin/fish me
USER me
WORKDIR /home/me

# --- Rust + websocat (built from source) --------------------------------------
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y && \
    . "$HOME/.cargo/env" && \
    git clone https://github.com/vi/websocat.git && \
    cd websocat && cargo build --release && sudo cp target/release/websocat /usr/local/bin/

# --- fnm / Node toolchain -----------------------------------------------------
RUN curl -fsSL https://fnm.vercel.app/install | bash -s -- --install-dir "$HOME/.fnm" --skip-shell
ENV PATH="/home/me/.fnm:${PATH}"
RUN mkdir -p /home/me/.config/fish/conf.d && \
    echo 'fnm env --use-on-cd --shell fish | source' > /home/me/.config/fish/conf.d/fnm.fish && \
    export PATH="/home/me/.fnm:${PATH}" && \
    eval "$(/home/me/.fnm/fnm env --use-on-cd)" && \
    /home/me/.fnm/fnm install lts-jod && \
    /home/me/.fnm/fnm install 20.5.0 && \
    /home/me/.fnm/fnm default lts-jod && \
    /home/me/.fnm/fnm use lts-jod && \
    npm i -g prettier typescript typescript-language-server npm-check-updates @anthropic-ai/claude-code @openai/codex netlify-cli && \
    echo 'fnm use lts-jod' >> /home/me/.config/fish/config.fish

# --- SBCL / Quicklisp ---------------------------------------------------------
RUN curl -fsSLO https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --non-interactive \
         --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
         --eval '(quit)'

# --- OPS (nanos) --------------------------------------------------------------
RUN curl https://ops.city/get.sh -sSfL | bash

# --- Deno (v1.45.0 for Netlify CLI compatibility) ----------------------------
RUN curl -fsSL https://deno.land/install.sh | DENO_INSTALL=/home/me/.deno sh -s v1.45.0 && \
    /home/me/.deno/bin/deno --version && \
    /home/me/.deno/bin/deno cache https://v2-14-0--edge.netlify.com/bootstrap/index-combined.ts 2>/dev/null || true && \
    /home/me/.deno/bin/deno cache https://v2-14-0--edge.netlify.com/bootstrap/handler.ts 2>/dev/null || true && \
    /home/me/.deno/bin/deno cache https://v2-14-0--edge.netlify.com/bootstrap/server.ts 2>/dev/null || true

# --- Fish config / entry ------------------------------------------------------
COPY config.fish /home/me/.config/fish/config.fish
COPY entry.fish /entry.fish
RUN sudo chown -R me:me /home/me/.config/fish && \
    sudo mkdir -p $HOME/.jupyter && sudo chmod -R u+rw $HOME/.jupyter

# --- IPython preload ----------------------------------------------------------
RUN mkdir -p /home/me/.ipython/profile_default/startup && \
    echo "import sys; sys.path.append('/workspaces/aesthetic-computer/notebook'); import aesthetic" > /home/me/.ipython/profile_default/startup/00-aesthetic.py && \
    sudo chown -R me:me /home/me/.ipython

# --- GYP shim (legacy consumers) ---------------------------------------------
RUN sudo mkdir -p /home/me/.local/lib/python3.10/site-packages && \
    git clone https://chromium.googlesource.com/external/gyp.git /home/me/.local/lib/python3.10/site-packages/gyp || true

# Optional: default shell session
# ENTRYPOINT ["/usr/bin/fish", "/entry.fish"]