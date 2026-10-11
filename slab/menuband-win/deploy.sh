#!/bin/bash
# deploy.sh — push the Windows Menu Band sources to a Windows box over ssh
# and build (and optionally run) them there.
#
#   ./deploy.sh            copy + build
#   ./deploy.sh --run      copy + build + (re)launch
#
# Host defaults to the `minibook` ssh alias; override with MB_HOST=...
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
host="${MB_HOST:-minibook}"
dest='src/menuband-win'

ssh "$host" "New-Item -ItemType Directory -Force \$HOME\\src\\menuband-win | Out-Null"
scp -q "$here/menuband.c" "$here/build.ps1" \
       "$root/fedac/native/src/gm_synth.c" "$root/fedac/native/src/gm_synth.h" \
       "$host:$dest/"
ssh "$host" "powershell -NoProfile -ExecutionPolicy Bypass -File \$HOME\\src\\menuband-win\\build.ps1"
if [[ "${1:-}" == "--run" ]]; then
  # A process started from the ssh session has no desktop. Relaunch through
  # the ac-desk scheduled task, which runs inside the signed-in session.
  ssh "$host" "Set-Content \$HOME\\desk-cmd.ps1 'Get-Process MenuBand -ErrorAction SilentlyContinue | Stop-Process -Force; Start-Sleep -Milliseconds 400; Start-Process \$HOME\\src\\menuband-win\\build\\MenuBand.exe; Start-Sleep 2; \"menuband running: \$((Get-Process MenuBand -ErrorAction SilentlyContinue) -ne \$null)\"'; Start-ScheduledTask ac-desk; Start-Sleep 4; Get-Content \$HOME\\desk.log | Select -Last 1"
fi
