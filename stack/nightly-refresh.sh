#!/bin/sh
# Wrapper for the nightly stack refresh (invoked by the launchd agent
# computer.aesthetic.stack-refresh). launchd runs with a minimal PATH and no
# fnm, so use fnm's stable default alias instead of a patch-specific path.
export PATH="/Users/jas/.local/share/fnm/aliases/default/bin:/opt/homebrew/bin:/usr/bin:/bin"
cd /Users/jas/aesthetic-computer || exit 1
# Requires the vault env to be present/unlocked; if it's locked the pull fails
# (logged, non-fatal — nothing is committed or pushed, this is read-only).
exec node --env-file=aesthetic-computer-vault/.devcontainer/envs/devcontainer.env stack/refresh.mjs
