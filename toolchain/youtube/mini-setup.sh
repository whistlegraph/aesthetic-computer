#!/usr/bin/env bash
# mini-setup.sh — put the YouTube CLI on a fleet Mac and sign in there, so a
# client channel's token never lives on the author's laptop.
#
#   toolchain/youtube/mini-setup.sh <ssh-host> <channel-name>
#   e.g. toolchain/youtube/mini-setup.sh chicken fuser
#
# What it does: copies yt.mjs + the OAuth client (from the vault) to the host
# under ~/.local/share/yt and ~/.config/yt (mode 600), then runs `yt.mjs auth`
# THERE. The consent URL is printed; open it in a browser ON THAT MACHINE (the
# callback listens on its localhost) and sign in as the channel's Google
# account. The refresh token is saved on the host as ~/.config/yt/<channel>-token.json.
# Afterwards every yt.mjs command on that host needs:
#   YT_CLIENT_JSON=~/.config/yt/client.json YT_TOKEN_JSON=~/.config/yt/<channel>-token.json
set -euo pipefail
HOST=${1:?ssh host}; CH=${2:?channel name}
cd "$(dirname "$0")/../.."
CLIENT=aesthetic-computer-vault/youtube/client.json
[[ -f $CLIENT ]] || { echo "missing $CLIENT (decrypt client.json.gpg first)" >&2; exit 2; }
ssh "$HOST" 'mkdir -p ~/.local/share/yt/toolchain/youtube ~/.config/yt && chmod 700 ~/.config/yt'
scp -q toolchain/youtube/yt.mjs "$HOST":~/.local/share/yt/toolchain/youtube/yt.mjs
scp -q "$CLIENT" "$HOST":~/.config/yt/client.json
ssh "$HOST" 'chmod 600 ~/.config/yt/client.json'
echo "→ starting consent flow on $HOST; a browser window should open there (or open the printed URL on $HOST)."
ssh "$HOST" "export PATH=/opt/homebrew/bin:\$PATH YT_CLIENT_JSON=~/.config/yt/client.json YT_TOKEN_JSON=~/.config/yt/$CH-token.json; cd ~/.local/share/yt && node toolchain/youtube/yt.mjs auth --as $CH && node toolchain/youtube/yt.mjs whoami --as $CH"
