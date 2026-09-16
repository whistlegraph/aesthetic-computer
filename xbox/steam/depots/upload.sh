#!/bin/sh
# Upload the three depots with steamcmd.
#
#   STEAM_BUILD_USER=<steam login> sh xbox/steam/depots/upload.sh
#
# steamcmd prompts for the password and Steam Guard code on the first run and
# caches a login token after that. Run depots.mjs first so out/app_build.vdf
# exists; the build shows up under Steamworks → SteamPipe → Builds, where a
# human sets it live on the default branch and presses "mark as ready".
set -eu
here="$(cd "$(dirname "$0")" && pwd)"
[ -f "$here/out/app_build.vdf" ] || { echo "run depots.mjs --appid=... first" >&2; exit 1; }
: "${STEAM_BUILD_USER:?set STEAM_BUILD_USER to the Steamworks login}"
exec steamcmd +login "$STEAM_BUILD_USER" +run_app_build "$here/out/app_build.vdf" +quit
