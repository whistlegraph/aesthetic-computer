#!/usr/bin/env bash
# Quick checker/installer for a native (non-devcontainer) AC environment on macOS.
# It verifies host tools and repo installs needed to run ac-site (Netlify dev) and Tezos keeps scripts.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OS_NAME="$(uname -s)"
INSTALL=false
INSTALL_NPM=false

for arg in "$@"; do
  case "$arg" in
    --install) INSTALL=true ;;
    --npm) INSTALL_NPM=true ;;
    --help|-h)
      echo "Usage: $0 [--install] [--npm]"
      echo "  --install   Install missing brew deps (fnm, fish, jq, caddy, stripe-cli) on macOS."
      echo "  --npm       Install missing node_modules in root/system/tezos."
      exit 0
      ;;
  esac
done

if [[ "$OS_NAME" != "Darwin" ]]; then
  echo "Note: expected macOS; detected $OS_NAME. The script will continue with best-effort checks."
fi

have() { command -v "$1" >/dev/null 2>&1; }

require_brew() {
  if ! have brew; then
    echo "Homebrew not found. Install from https://brew.sh first."
    return 1
  fi
}

install_pkg() {
  local pkg="$1"
  if $INSTALL; then
    echo "Installing $pkg via brew..."
    brew install "$pkg"
  else
    echo "Missing $pkg (run: brew install $pkg)"
  fi
}

install_npm() {
  local dir="$1"
  if [[ ! -f "$dir/package.json" ]]; then
    echo "Skipping $dir (no package.json)"
    return
  fi
  echo "Installing npm deps in $dir..."
  (cd "$dir" && npm install)
}

echo "Checking host tools..."
require_brew || true

for pkg in fnm; do
  if have "$pkg"; then
    echo "✓ $pkg"
  else
    install_pkg "$pkg"
  fi
done

# Optional helpers that improve parity with the devcontainer.
for pkg in fish jq caddy stripe/stripe-cli; do
  name="${pkg##*/}"
  if have "$name"; then
    echo "✓ $name"
  else
    if $INSTALL; then
      install_pkg "$pkg"
    else
      echo "(optional) Missing $name (brew install $pkg)"
    fi
  fi
done

echo "Ensuring Node (LTS Jod / v22) via fnm..."
if have fnm; then
  if $INSTALL; then
    fnm install lts/jod >/dev/null 2>&1 || fnm install 22 >/dev/null 2>&1 || true
  fi
  # shellcheck disable=SC1090
  eval "$(fnm env --use-on-cd --shell bash --log-level quiet)" >/dev/null 2>&1 || true
fi

if have node; then
  NODE_VER="$(node -v 2>/dev/null || true)"
  echo "Node detected: $NODE_VER"
  if [[ "$NODE_VER" < "v22." ]]; then
    echo "Warning: Node is older than v22. Use: fnm use lts/jod --install-if-missing"
  fi
else
  echo "Node not found. Install via fnm: fnm install lts/jod && fnm use lts/jod"
fi

echo "Checking repo installs..."
if [[ ! -d "$REPO_ROOT/node_modules" ]]; then
  if $INSTALL_NPM; then
    install_npm "$REPO_ROOT"
  else
    echo "Root node_modules missing. Run: cd \"$REPO_ROOT\" && npm install (or rerun with --npm)"
  fi
else
  echo "✓ root node_modules present"
fi

if [[ ! -d "$REPO_ROOT/system/node_modules" ]]; then
  if $INSTALL_NPM; then
    install_npm "$REPO_ROOT/system"
  else
    echo "System node_modules missing. Run: cd \"$REPO_ROOT/system\" && npm install (or rerun with --npm)"
  fi
else
  echo "✓ system node_modules present"
fi

NETLIFY_BIN="$REPO_ROOT/system/node_modules/.bin/netlify"
if [[ -x "$NETLIFY_BIN" ]]; then
  echo "✓ netlify CLI (local) found at $NETLIFY_BIN"
else
  echo "netlify CLI not found locally. After installing system deps, use: cd \"$REPO_ROOT/system\" && npm install"
fi

echo "Tezos keeps helpers..."
if have python3; then
  echo "✓ python3"
else
  echo "Missing python3 (install via: brew install python)"
fi

if [[ ! -d "$REPO_ROOT/tezos/node_modules" ]]; then
  if $INSTALL_NPM; then
    install_npm "$REPO_ROOT/tezos"
  else
    echo "Tezos node_modules missing. Run: cd \"$REPO_ROOT/tezos\" && npm install (or rerun with --npm)"
  fi
else
  echo "✓ tezos node_modules present"
fi

echo
echo "Next steps:"
echo "- To install missing tools: rerun with --install (Homebrew is required)."
echo "- To install missing node_modules: rerun with --npm."
echo "- Start the site: cd \"$REPO_ROOT/system\" && npm run local-dev"
echo "- Full stack: cd \"$REPO_ROOT\" && npm run aesthetic"
echo "- Tezos CLI smoke test: cd \"$REPO_ROOT/tezos\" && node keeps.mjs status"
