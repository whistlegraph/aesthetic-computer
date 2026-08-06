#!/usr/bin/env bash
# Bootstrap a native (non-devcontainer) Aesthetic Computer dev environment on Fedora.
# Installs core system deps, Node 22 via fnm, and common CLI tools used by the repo.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
EXTRAS=false
CLONE_VAULT=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    --extras)
      EXTRAS=true
      shift
      ;;
    --clone-vault)
      CLONE_VAULT=true
      shift
      ;;
    --help|-h)
      echo "Usage: $0 [--extras] [--clone-vault]"
      echo "  --extras       Install optional tools (mkcert, stripe-cli, ngrok, doctl)."
      echo "  --clone-vault  Clone aesthetic-computer-vault alongside this repo and copy devcontainer env."
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

have() { command -v "$1" >/dev/null 2>&1; }

need_cmd() {
  if ! have "$1"; then
    echo "Missing required command: $1"
    exit 1
  fi
}

ensure_rpmfusion() {
  # Needed for ffmpeg on Fedora.
  local release
  release="$(rpm -E %fedora)"
  if ! rpm -qa | grep -q rpmfusion-free-release; then
    sudo dnf install -y       "https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-${release}.noarch.rpm"       "https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-${release}.noarch.rpm"
  fi
}

install_packages() {
  local packages=(
    git curl wget unzip tar xz sudo
    fish jq ripgrep tree procps-ng coreutils
    python3 python3-pip python3-devel
    make automake gcc gcc-c++ cmake pkg-config autoconf libtool redhat-rpm-config
    openssl-devel libffi-devel nss-tools
    ffmpeg ffmpeg-devel
    redis
    chromium
    caddy
    # GUI/Chrome deps used by puppeteer/playwright
    alsa-lib libX11 libXcomposite libXcursor libXdamage libXext libXi libXrandr libXScrnSaver libXtst libXrender libxkbfile gtk3 pango atk cups-libs xorg-x11-server-Xvfb
  )

  echo "Installing base packages via dnf..."
  sudo dnf install -y --allowerasing --skip-broken "${packages[@]}"
}

install_extras() {
  echo "Installing optional extras..."

  if ! have mkcert; then
    sudo dnf install -y mkcert || true
    mkcert -install || true
  fi

  # Stripe CLI (architecture-aware)
  if ! have stripe; then
    local arch
    case "$(uname -m)" in
      x86_64) arch="linux_x86_64" ;;
      aarch64) arch="linux_arm64" ;;
      *) arch="" ;;
    esac
    if [[ -n "$arch" ]]; then
      curl -fsSL "https://github.com/stripe/stripe-cli/releases/download/v1.30.0/stripe_1.30.0_${arch}.tar.gz"         | tar xz -C /tmp
      sudo mv "/tmp/stripe" /usr/local/bin/stripe
    else
      echo "Unknown arch for stripe-cli; skipping."
    fi
  fi

  # ngrok
  if ! have ngrok; then
    local arch
    case "$(uname -m)" in
      x86_64) arch="amd64" ;;
      aarch64) arch="arm64" ;;
      *) arch="" ;;
    esac
    if [[ -n "$arch" ]]; then
      curl -fsSL "https://bin.equinox.io/c/bNyj1mQVY4c/ngrok-v3-stable-linux-${arch}.tgz" -o /tmp/ngrok.tgz
      sudo tar -xzf /tmp/ngrok.tgz -C /usr/local/bin
      rm /tmp/ngrok.tgz
    else
      echo "Unknown arch for ngrok; skipping."
    fi
  fi

  # doctl (DigitalOcean CLI)
  if ! have doctl; then
    local arch
    case "$(uname -m)" in
      x86_64) arch="amd64" ;;
      aarch64) arch="arm64" ;;
      *) arch="" ;;
    esac
    if [[ -n "$arch" ]]; then
      local version="1.109.0"
      curl -fsSL "https://github.com/digitalocean/doctl/releases/download/v${version}/doctl-${version}-linux-${arch}.tar.gz" -o /tmp/doctl.tar.gz
      sudo tar -xzf /tmp/doctl.tar.gz -C /usr/local/bin
      rm /tmp/doctl.tar.gz
      sudo chmod +x /usr/local/bin/doctl
    else
      echo "Unknown arch for doctl; skipping."
    fi
  fi
}

install_fnm_node() {
  if ! have fnm; then
    echo "Installing fnm..."
    mkdir -p "$HOME/.fnm"
    curl -fsSL https://fnm.vercel.app/install | bash -s -- --install-dir "$HOME/.fnm" --skip-shell
  fi

  export PATH="$HOME/.fnm:$PATH"
  if [[ -x "$HOME/.fnm/fnm" ]]; then
    eval "$("$HOME/.fnm/fnm" env --use-on-cd --shell bash --log-level quiet)" || true

    echo "Ensuring Node 22 (lts/jod)..."
    "$HOME/.fnm/fnm" install lts/jod >/dev/null 2>&1 || "$HOME/.fnm/fnm" install 22 >/dev/null 2>&1 || true
    "$HOME/.fnm/fnm" default lts/jod || true
    "$HOME/.fnm/fnm" use lts/jod || true
  else
    echo "fnm install failed (missing $HOME/.fnm/fnm)" >&2
  fi

  node -v || true
  npm -v || true
}

install_global_npm() {
  if ! have npm; then
    echo "npm not found; ensure fnm/node succeeded."
    return
  fi
  echo "Installing global npm helpers (user-scoped)..."
  mkdir -p "$HOME/.local"
  npm install -g --prefix "$HOME/.local" netlify-cli npm-check-updates concurrently kill-port @devcontainers/cli typescript typescript-language-server
}

add_hosts_entries() {
  for host in "aesthetic.local" "sotce.local"; do
    if ! grep -q "$host" /etc/hosts; then
      echo "Adding $host to /etc/hosts"
      echo "127.0.0.1 ${host}" | sudo tee -a /etc/hosts >/dev/null
    fi
  done
}

enable_services() {
  if systemctl list-unit-files | grep -q redis.service; then
    sudo systemctl enable redis.service
    sudo systemctl start redis.service
  else
    echo "Redis service not found; start manually with 'redis-server --daemonize yes'."
  fi
}

main() {
  need_cmd dnf
  ensure_rpmfusion
  install_packages
  $EXTRAS && install_extras
  install_fnm_node
  install_global_npm
  add_hosts_entries
  enable_services

  if $CLONE_VAULT; then
    local vault_dir="$REPO_ROOT/aesthetic-computer-vault"
    local vault_repo="${VAULT_URL:-git@github.com:whistlegraph/aesthetic-computer-vault.git}"
    if [[ -d "$vault_dir/.git" ]]; then
      echo "Vault already present at $vault_dir (skipping clone)."
    else
      echo "Cloning aesthetic-computer-vault into $vault_dir ..."
      git clone "$vault_repo" "$vault_dir" || echo "⚠️  Vault clone failed (check access to $vault_repo)."
    fi

    local vault_env="$vault_dir/.devcontainer/envs/devcontainer.env"
    local target_env="$REPO_ROOT/.devcontainer/envs/devcontainer.env"
    if [[ -f "$vault_env" && ! -f "$target_env" ]]; then
      echo "Copying devcontainer env from vault..."
      mkdir -p "$(dirname "$target_env")"
      cp "$vault_env" "$target_env"
      echo "Copied: $target_env"
    else
      echo "Vault env copy skipped (source or target missing/existing)."
    fi
  fi

  cat <<EOF

✅ Fedora host packages and Node toolchain installed.
Next steps:
1) If you haven't yet, copy secrets to ${REPO_ROOT}/.devcontainer/envs/devcontainer.env (or rerun with --clone-vault)
2) Install repo deps: npm install (root), npm install (system), npm install (session-server)
3) Start dev services: cd ${REPO_ROOT} && npm run aesthetic
EOF
}

main "$@"
