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
scp -q "$here/menuband.c" "$here/build.ps1" "$here/percussion.h" "$here/percussion_stub.c" \
       "$root/fedac/native/src/gm_synth.c" "$root/fedac/native/src/gm_synth.h" \
       "$host:$dest/"
[[ -f "$here/percussion.c" ]] && scp -q "$here/percussion.c" "$host:$dest/"
# The AC grand piano bank (fedac/native/samples/piano, 14 MB) sits beside the exe; copy once.
if ! ssh "$host" "Test-Path \$HOME\\src\\menuband-win\\build\\samples\\piano\\60.raw" | grep -q True; then
  ssh "$host" "New-Item -ItemType Directory -Force \$HOME\\src\\menuband-win\\build\\samples\\piano | Out-Null"
  scp -q "$root"/fedac/native/samples/piano/*.raw "$host:$dest/build/samples/piano/"
fi
ssh "$host" "powershell -NoProfile -ExecutionPolicy Bypass -File \$HOME\\src\\menuband-win\\build.ps1"
if [[ "${1:-}" == "--run" ]]; then
  # A process started from the ssh session has no desktop. Relaunch through
  # the ac-desk scheduled task, which runs queued scripts inside the signed-in
  # session (~\desk-queue\*.ps1, so other agents' launches don't collide).
  stamp="$(date +%s)-menuband"
  ssh "$host" "Set-Content \$HOME\\desk-queue\\$stamp.ps1 'Get-Process MenuBand -ErrorAction SilentlyContinue | Stop-Process -Force; Start-Sleep -Milliseconds 400; Start-Process \$HOME\\src\\menuband-win\\build\\MenuBand.exe; Start-Sleep 2; \"menuband running: \$((Get-Process MenuBand -ErrorAction SilentlyContinue) -ne \$null)\"'; Start-ScheduledTask ac-desk; Start-Sleep 4; Get-Content \$HOME\\desk.log | Select -Last 1"
fi
