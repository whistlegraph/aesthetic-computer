#!/bin/bash
# deploy.sh — push the Windows Menu Band sources to a Windows box over ssh
# and build (and optionally run) them there.
#
#   ./deploy.sh            copy + build
#   ./deploy.sh --run      copy + build + (re)launch
#
# Host defaults to the `choowee` ssh alias; override with MB_HOST=...
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
root="$(cd "$here/../.." && pwd)"
host="${MB_HOST:-choowee}"
dest='src/menuband-win'
# Every step, and the compiler's own output, lands in ~\ac-progress.log on the
# Windows box — the progress window on its screen (progress-window.ps1) tails it.
progress() { ssh "$host" "Add-Content \$HOME\\ac-progress.log ((Get-Date -Format HH:mm:ss) + '  $1')" >/dev/null 2>&1 || true; }

progress "menu band: copying sources"

ssh "$host" "New-Item -ItemType Directory -Force \$HOME\\src\\menuband-win | Out-Null"
scp -q "$here/menuband.c" "$here/build.ps1" "$here/percussion.h" "$here/percussion_stub.c" "$here/tsf.h" \
       "$root/fedac/native/src/gm_synth.c" "$root/fedac/native/src/gm_synth.h" \
       "$host:$dest/"
[[ -f "$here/percussion.c" ]] && scp -q "$here/percussion.c" "$host:$dest/"
# The AC grand piano bank (fedac/native/samples/piano, 14 MB) sits beside the exe; copy once.
if ! ssh "$host" "Test-Path \$HOME\\src\\menuband-win\\build\\samples\\piano\\60.raw" | grep -q True; then
  ssh "$host" "New-Item -ItemType Directory -Force \$HOME\\src\\menuband-win\\build\\samples\\piano | Out-Null"
  scp -q "$root"/fedac/native/samples/piano/*.raw "$host:$dest/build/samples/piano/"
fi
# SoundFont banks (GeneralUser GS, 32 MB) sit in build\soundfonts beside the exe; copy once.
if ! ssh "$host" "Test-Path \$HOME\\src\\menuband-win\\build\\soundfonts\\GeneralUser-GS.sf2" | grep -q True; then
  ssh "$host" "New-Item -ItemType Directory -Force \$HOME\\src\\menuband-win\\build\\soundfonts | Out-Null"
  scp -q "$here"/soundfonts/* "$host:$dest/build/soundfonts/"
fi
progress "menu band: compiling"
# The build's own lines stream into the progress log as they come, and back here.
# The remote login shell is PowerShell, so the pipeline runs there around the build.
ssh "$host" 'powershell -NoProfile -ExecutionPolicy Bypass -File $HOME\src\menuband-win\build.ps1 2>&1 | ForEach-Object { Add-Content $HOME\ac-progress.log ("         " + $_); $_ }; exit $LASTEXITCODE' \
  && progress "menu band: built" || { progress "menu band: BUILD FAILED"; exit 1; }
if [[ "${1:-}" == "--run" ]]; then
  progress "menu band: relaunching"
  # A process started from the ssh session has no desktop. Relaunch through
  # the ac-desk scheduled task, which runs queued scripts inside the signed-in
  # session (~\desk-queue\*.ps1, so other agents' launches don't collide).
  stamp="$(date +%s)-menuband"
  ssh "$host" "Set-Content \$HOME\\desk-queue\\$stamp.ps1 'Get-Process MenuBand -ErrorAction SilentlyContinue | Stop-Process -Force; Start-Sleep -Milliseconds 400; Start-Process \$HOME\\src\\menuband-win\\build\\MenuBand.exe; Start-Sleep 2; \"menuband running: \$((Get-Process MenuBand -ErrorAction SilentlyContinue) -ne \$null)\"'; Start-ScheduledTask ac-desk; Start-Sleep 4; Get-Content \$HOME\\desk.log | Select -Last 1"
fi
