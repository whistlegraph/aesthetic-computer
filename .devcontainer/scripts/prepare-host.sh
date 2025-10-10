#!/bin/sh
set -eu

mkdir -p \
  .devcontainer/tezos-node \
  .devcontainer/tezos-data \
  .devcontainer/envs \
  .devcontainer/.emacs.d

if [ ! -f .devcontainer/fish_history ]; then
  : > .devcontainer/fish_history
fi
