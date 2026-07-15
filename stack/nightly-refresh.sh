#!/bin/sh
# Wrapper for the nightly stack refresh (invoked by the launchd agent
# computer.aesthetic.stack-refresh). launchd runs with a minimal PATH and no
# fnm, so set the node bin explicitly. If fnm's node version changes, update the
# path here — it's the one place it lives.
export PATH="/Users/jas/.local/share/fnm/node-versions/v22.22.3/installation/bin:/opt/homebrew/bin:/usr/bin:/bin"
cd /Users/jas/aesthetic-computer || exit 1
# Requires the vault env to be present/unlocked; if it's locked the pull fails
# (logged, non-fatal — nothing is committed or pushed, this is read-only).
exec node --env-file=aesthetic-computer-vault/.devcontainer/envs/devcontainer.env stack/refresh.mjs
