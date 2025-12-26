#!/bin/sh
set -eu

# Remove any existing container named "aesthetic" to prevent rebuild conflicts
docker rm -f aesthetic 2>/dev/null || true

mkdir -p \
  .devcontainer/tezos-node \
  .devcontainer/tezos-data \
  .devcontainer/envs \
  .devcontainer/.emacs.d

if [ ! -f .devcontainer/fish_history ]; then
  : > .devcontainer/fish_history
fi
