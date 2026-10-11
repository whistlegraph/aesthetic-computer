#!/bin/bash
# deploy.sh — push the slab live wallpaper to a Windows box over ssh, build it
# there, and (with --run) relaunch it inside the signed-in desktop session.
#
#   ./deploy.sh            copy + build
#   ./deploy.sh --run      copy + build + (re)launch behind the desktop icons
#
# Host defaults to the `minibook` ssh alias; override with MB_HOST=...
# Regenerate pals_sprite.h with:  python3 gen-sprite.py
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
host="${MB_HOST:-minibook}"
dest='src/wallpaper'

ssh "$host" "New-Item -ItemType Directory -Force \$HOME\\src\\wallpaper | Out-Null"
scp -q "$here/wallpaper.c" "$here/pals_sprite.h" "$here/build.ps1" "$here/app.manifest" "$host:$dest/"
ssh "$host" "powershell -NoProfile -ExecutionPolicy Bypass -File \$HOME\\src\\wallpaper\\build.ps1"
if [[ "${1:-}" == "--run" ]]; then
  # A process started from the ssh session has no desktop. Relaunch through
  # the ac-desk scheduled task's queue (~\desk-queue\*.ps1), which runs inside
  # the signed-in session without colliding with other agents' launches.
  stamp="$(date +%s)-wallpaper"
  ssh "$host" "Set-Content \$HOME\\desk-queue\\$stamp.ps1 'Get-Process SlabWallpaper -ErrorAction SilentlyContinue | Stop-Process -Force; Start-Sleep -Milliseconds 400; Start-Process \$HOME\\src\\wallpaper\\build\\SlabWallpaper.exe; Start-Sleep 2; \"wallpaper running: \$((Get-Process SlabWallpaper -ErrorAction SilentlyContinue) -ne \$null)\"'; Start-ScheduledTask ac-desk; Start-Sleep 5; Get-Content \$HOME\\desk.log | Select -Last 1"
fi
