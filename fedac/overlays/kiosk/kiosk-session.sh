#!/bin/bash
# FedAC Kiosk Session — prefer cage (black background, no window animations).
# NOTE: The authoritative version is generated inline by make-kiosk-piece-usb.sh.
# This file is a reference copy.
set -euo pipefail
export XDG_SESSION_TYPE=wayland
export XDG_RUNTIME_DIR="/run/user/$(id -u)"
export MOZ_ENABLE_WAYLAND=1
export MOZ_DBUS_REMOTE=0
export NO_AT_BRIDGE=1
export CLUTTER_DISABLE_ANIMATIONS=1
PROFILE="/home/liveuser/.mozilla/firefox/kiosk"

exec > /tmp/kiosk.log 2>&1

# Paint PALS logo to framebuffer (visible during PipeWire/cage startup)
if [ -x /usr/local/bin/pals-fb-splash ]; then
  /usr/local/bin/pals-fb-splash 2>/dev/null || true
fi

# Start PipeWire audio stack (cage doesn't launch a full desktop session).
mkdir -p "$XDG_RUNTIME_DIR"
if command -v pipewire >/dev/null 2>&1; then
  pipewire &
  sleep 0.3
  command -v wireplumber >/dev/null 2>&1 && wireplumber &
  command -v pipewire-pulse >/dev/null 2>&1 && pipewire-pulse &
  sleep 0.2
fi

if command -v cage >/dev/null 2>&1; then
  TTY_DEV="$(tty 2>/dev/null || true)"
  [ -n "$TTY_DEV" ] && printf '\033[2J\033[3J\033[H\033[?25l' >"$TTY_DEV" 2>/dev/null || true
  exec cage -- firefox --kiosk --no-remote --new-instance \
    --profile "$PROFILE" --private-window \
    file:///usr/local/share/kiosk/piece.html
fi

exec mutter --wayland --no-x11 --sm-disable -- firefox --kiosk --no-remote \
  --new-instance --profile "$PROFILE" --private-window \
  file:///usr/local/share/kiosk/piece.html
