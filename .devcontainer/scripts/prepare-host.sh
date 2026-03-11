#!/bin/sh
set -eu

# Remove any existing container named "aesthetic" to prevent rebuild conflicts
docker rm -f aesthetic 2>/dev/null || true

mkdir -p \
  .devcontainer/tezos-node \
  .devcontainer/tezos-data \
  .devcontainer/envs \
  .devcontainer/.emacs.d \
  "$HOME/.claude" \
  "$HOME/.codex"

chmod 700 "$HOME/.claude" "$HOME/.codex" 2>/dev/null || true

# Claude also keeps account metadata in ~/.claude.json.
# Ensure the file exists before bind-mounting it into the container.
if [ ! -f "$HOME/.claude.json" ]; then
  : > "$HOME/.claude.json"
fi
chmod 600 "$HOME/.claude.json" 2>/dev/null || true

# Normalize script readability/executability for container startup hooks.
# Some host filesystems/checkouts can leave restrictive bits that cause
# postStartCommand to fail with "Permission denied" inside the container.
chmod 755 .devcontainer .devcontainer/scripts 2>/dev/null || true
chmod 755 .devcontainer/scripts/poststart-wrapper.sh 2>/dev/null || true

if [ ! -f .devcontainer/fish_history ]; then
  : > .devcontainer/fish_history
fi
