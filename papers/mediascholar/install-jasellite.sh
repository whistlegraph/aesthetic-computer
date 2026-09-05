#!/usr/bin/env bash
# Install Mediascholar's headless Prox and guarded systemd user units on
# Jasellite. Default invocation is read-only. Dependencies are installed only
# after the runner's resource admission gate accepts the host.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOURCE_ROOT="${MEDIASCHOLAR_SOURCE_ROOT:-$(cd "$SCRIPT_DIR/../.." && pwd)}"
REPO="${MEDIASCHOLAR_REPO:-$HOME/aesthetic-computer}"
SOURCE="$SOURCE_ROOT/papers/mediascholar/systemd"
RUNTIME="${MEDIASCHOLAR_RUNTIME:-$HOME/.local/lib/mediascholar}"
UNIT_DIR="$HOME/.config/systemd/user"
CONFIG_DIR="$HOME/.config/mediascholar"
ENV_FILE="$CONFIG_DIR/env"
SOURCE_RUNNER="$SOURCE_ROOT/papers/bin/mediascholar.mjs"
RUNTIME_RUNNER="$RUNTIME/papers/bin/mediascholar.mjs"
RUNNER="$SOURCE_RUNNER"
[[ -f "$RUNTIME_RUNNER" ]] && RUNNER="$RUNTIME_RUNNER"

stage_runtime() {
  [[ -d "$REPO/node_modules" ]] || { echo "$REPO/node_modules is required" >&2; exit 1; }
  install -d -m 0755 \
    "$RUNTIME/papers/bin" "$RUNTIME/papers/mediascholar/systemd" \
    "$RUNTIME/slab/bin" "$RUNTIME/toolchain/mcp"
  install -m 0755 "$SOURCE_ROOT/papers/bin/mediascholar.mjs" "$RUNTIME/papers/bin/mediascholar.mjs"
  install -m 0755 "$SOURCE_ROOT/papers/mediascholar/run-sandboxed.sh" "$RUNTIME/papers/mediascholar/run-sandboxed.sh"
  install -m 0755 "$SOURCE_ROOT/slab/bin/mediascholar-credential-proxy.mjs" "$RUNTIME/slab/bin/mediascholar-credential-proxy.mjs"
  install -m 0755 "$SOURCE_ROOT/slab/bin/prox-worker.mjs" "$RUNTIME/slab/bin/prox-worker.mjs"
  install -m 0755 "$SOURCE_ROOT/slab/bin/paper-mcp.mjs" "$RUNTIME/slab/bin/paper-mcp.mjs"
  install -m 0644 "$SOURCE_ROOT/toolchain/mcp/http-front.mjs" "$RUNTIME/toolchain/mcp/http-front.mjs"
  install -m 0644 "$SOURCE_ROOT/papers/source-bundle.mjs" "$RUNTIME/papers/source-bundle.mjs"
  install -m 0644 \
    "$SOURCE_ROOT/papers/mediascholar/SCORE.md" \
    "$SOURCE_ROOT/papers/mediascholar/README.md" \
    "$SOURCE_ROOT/papers/mediascholar/topic.schema.json" \
    "$SOURCE_ROOT/papers/mediascholar/paper-result.schema.json" \
    "$SOURCE_ROOT/papers/mediascholar/topic-prompt.md" \
    "$SOURCE_ROOT/papers/mediascholar/paper-prompt.md" \
    "$RUNTIME/papers/mediascholar/"
  install -m 0755 "$SOURCE_ROOT/papers/mediascholar/install-jasellite.sh" \
    "$RUNTIME/papers/mediascholar/install-jasellite.sh"
  install -m 0644 "$SOURCE"/*.service "$SOURCE"/*.timer "$RUNTIME/papers/mediascholar/systemd/"
  if [[ -e "$RUNTIME/node_modules" && ! -L "$RUNTIME/node_modules" ]]; then
    echo "$RUNTIME/node_modules exists and is not a symlink" >&2
    exit 1
  fi
  ln -sfn "$REPO/node_modules" "$RUNTIME/node_modules"
  RUNNER="$RUNTIME_RUNNER"
}

install_units() {
  stage_runtime
  mkdir -p "$UNIT_DIR" "$CONFIG_DIR/credentials" \
    "$HOME/.local/share/mediascholar" "$HOME/.config/slab/ledger/advertise"
  chmod 700 "$CONFIG_DIR" "$CONFIG_DIR/credentials" "$HOME/.local/share/mediascholar"
  install -m 0644 "$SOURCE/mediascholar-proxy.service" "$UNIT_DIR/mediascholar-proxy.service"
  install -m 0644 "$SOURCE/prox-worker.service" "$UNIT_DIR/prox-worker.service"
  install -m 0644 "$SOURCE/mediascholar.service" "$UNIT_DIR/mediascholar.service"
  install -m 0644 "$SOURCE/mediascholar.timer" "$UNIT_DIR/mediascholar.timer"
  install -m 0644 "$SOURCE/mediascholar-bootstrap.service" "$UNIT_DIR/mediascholar-bootstrap.service"
  install -m 0644 "$SOURCE/mediascholar-bootstrap.timer" "$UNIT_DIR/mediascholar-bootstrap.timer"
  if [[ ! -f "$ENV_FILE" ]]; then
    printf '%s\n' \
      'MEDIASCHOLAR_ENABLED=0' \
      'MEDIASCHOLAR_PROVIDER=auto' \
      'MEDIASCHOLAR_PROVIDER_PREFERENCE=claude,openai' \
      'MEDIASCHOLAR_PROXY_URL=http://127.0.0.1:7431' \
      'MEDIASCHOLAR_MAX_LOAD_PER_CPU=0.55' \
      'MEDIASCHOLAR_MIN_AVAILABLE_MEMORY_MIB=4096' \
      'MEDIASCHOLAR_MIN_FREE_DISK_GIB=32' \
      'MEDIASCHOLAR_MIN_DAYS_BETWEEN_PAPERS=7' \
      'MEDIASCHOLAR_MAX_RETAINED_CANDIDATES=4' \
      'MEDIASCHOLAR_TOPIC_BUDGET_USD=2' \
      'MEDIASCHOLAR_PAPER_BUDGET_USD=12' \
      'MEDIASCHOLAR_STAGE_TIMEOUT_MINUTES=150' \
      'MEDIASCHOLAR_PROXY_MAX_DAILY_REQUESTS=160' \
      'MEDIASCHOLAR_PROXY_MAX_CONCURRENT=2' \
      > "$ENV_FILE"
    chmod 600 "$ENV_FILE"
  fi
  systemctl --user daemon-reload
  systemctl --user enable --now mediascholar-proxy.service prox-worker.service
}

install_dependencies() {
  [[ -f "$RUNTIME_RUNNER" ]] && RUNNER="$RUNTIME_RUNNER"
  node "$RUNNER" admit
  command -v ionice >/dev/null 2>&1 || { echo "ionice is required" >&2; exit 1; }
  sudo -n ionice -c 3 nice -n 19 apt-get update
  sudo -n ionice -c 3 nice -n 19 apt-get install -y --no-install-recommends \
    texlive-xetex texlive-latex-extra texlive-fonts-recommended texlive-bibtex-extra \
    poppler-utils imagemagick zip bubblewrap
}

install_codex() {
  [[ -f "$RUNTIME_RUNNER" ]] && RUNNER="$RUNTIME_RUNNER"
  node "$RUNNER" admit
  sudo -n ionice -c 3 nice -n 19 npm install -g @openai/codex
}

enable_timer() {
  [[ -f "$ENV_FILE" ]] || install_units
  if grep -q '^MEDIASCHOLAR_ENABLED=' "$ENV_FILE"; then
    sed -i.bak 's/^MEDIASCHOLAR_ENABLED=.*/MEDIASCHOLAR_ENABLED=1/' "$ENV_FILE"
    rm -f "$ENV_FILE.bak"
  else
    printf '%s\n' 'MEDIASCHOLAR_ENABLED=1' >> "$ENV_FILE"
  fi
  systemctl --user restart mediascholar-proxy.service prox-worker.service
  systemctl --user enable --now mediascholar.timer
}

queue_bootstrap() {
  install_units
  systemctl --user enable --now mediascholar-bootstrap.timer
}

bootstrap_once() {
  if ! node "$RUNNER" admit; then
    echo "Mediascholar bootstrap deferred by resource admission"
    return 0
  fi
  install_dependencies
  MEDIASCHOLAR_ENABLED=0 "$RUNTIME/papers/mediascholar/run-sandboxed.sh"
  enable_timer
  systemctl --user disable --now mediascholar-bootstrap.timer 2>/dev/null || true
}

disable_timer() {
  systemctl --user disable --now mediascholar.timer 2>/dev/null || true
  systemctl --user disable --now mediascholar-bootstrap.timer 2>/dev/null || true
  if [[ -f "$ENV_FILE" ]]; then
    sed -i.bak 's/^MEDIASCHOLAR_ENABLED=.*/MEDIASCHOLAR_ENABLED=0/' "$ENV_FILE"
    rm -f "$ENV_FILE.bak"
  fi
}

status() {
  node "$RUNNER" doctor
  printf '\nunits:\n'
  systemctl --user is-enabled mediascholar-proxy.service prox-worker.service mediascholar.timer mediascholar-bootstrap.timer 2>/dev/null || true
  systemctl --user is-active mediascholar-proxy.service prox-worker.service mediascholar.service mediascholar-bootstrap.service 2>/dev/null || true
  systemctl --user list-timers mediascholar.timer mediascholar-bootstrap.timer --all --no-pager 2>/dev/null || true
}

case "${1:---check}" in
  --stage) stage_runtime ;;
  --install) install_units ;;
  --queue) queue_bootstrap ;;
  --bootstrap-once) bootstrap_once ;;
  --dependencies) install_dependencies ;;
  --install-codex) install_codex ;;
  --enable) enable_timer ;;
  --disable) disable_timer ;;
  --check) status ;;
  *) echo "usage: install-jasellite.sh [--check|--stage|--install|--queue|--dependencies|--install-codex|--enable|--disable]" >&2; exit 2 ;;
esac
