#!/usr/bin/env bash
set -euo pipefail

AC_URL="${AC_URL:-https://aesthetic.computer?tv=true&nogap=true&nolabel=true}"
LOG_DIR="/home/feralfile/.logs"
mkdir -p "$LOG_DIR"

log() { echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" >> "$LOG_DIR/aesthetic-kiosk.log"; }

log "Starting Aesthetic Computer kiosk"
log "URL: $AC_URL"

# Read saved rotation (if any)
ROTATION="normal"
if [ -f /home/feralfile/.state/screen-orientation ]; then
  ROTATION=$(cat /home/feralfile/.state/screen-orientation)
fi

# Detect GPU vendor for optimal Chromium flags
VENDOR=$(grep -m1 vendor_id /proc/cpuinfo | awk '{print $3}' || echo "Unknown")
log "CPU vendor: $VENDOR"

if [ "$VENDOR" = "GenuineIntel" ]; then
  FEATURES="UseOzonePlatform,AcceleratedVideoDecodeLinuxGL,AcceleratedVideoDecodeLinuxZeroCopyGL"
else
  FEATURES="UseOzonePlatform,VaapiVideoDecoder,VaapiIgnoreDriverChecks,Vulkan,DefaultANGLEVulkan,VulkanFromANGLE"
fi

DISABLE_FEATURES="TranslateUI,InterestFeedContentSuggestions,GlobalMediaControls"

# Wait for display / Wayland if needed
for i in $(seq 1 30); do
  if [ -n "${WAYLAND_DISPLAY:-}" ] || [ -S "${XDG_RUNTIME_DIR:-/run/user/1000}/wayland-0" ] || [ -S "${XDG_RUNTIME_DIR:-/run/user/1000}/wayland-1" ]; then
    log "Wayland display ready"
    break
  fi
  log "Waiting for Wayland display... ($i/30)"
  sleep 1
done

# Try to detect if we're inside cage already, or need to start it
if [ -n "${WAYLAND_DISPLAY:-}" ]; then
  # Already inside a Wayland session (e.g., cage launched us)
  log "Wayland session detected, launching Chromium directly"
  exec /usr/bin/chromium \
    --kiosk \
    --ozone-platform=wayland \
    --enable-features="$FEATURES" \
    --ignore-gpu-blocklist \
    --enable-gpu-rasterization \
    --no-first-run \
    --disable-sync \
    --disable-translate \
    --disable-infobars \
    --disable-features="$DISABLE_FEATURES" \
    --disable-background-networking \
    --noerrdialogs \
    --disable-extensions \
    --autoplay-policy=no-user-gesture-required \
    --disable-client-side-phishing-detection \
    --enable-logging=stderr \
    --v=0 \
    --hide-scrollbars \
    --no-default-browser-check \
    --disable-background-timer-throttling \
    --disable-renderer-backgrounding \
    --disable-hang-monitor \
    --deny-permission-prompts \
    --disable-session-crashed-bubble \
    "$AC_URL" 2>>"$LOG_DIR/aesthetic-kiosk.log"
else
  # No Wayland session — launch cage as our compositor
  log "No Wayland session, launching cage compositor"
  exec cage -- /bin/bash -c "
    wlr-randr --output HDMI-A-1 --transform $ROTATION 2>/dev/null || true
    exec /usr/bin/chromium \
      --kiosk \
      --ozone-platform=wayland \
      --enable-features=$FEATURES \
      --ignore-gpu-blocklist \
      --enable-gpu-rasterization \
      --no-first-run \
      --disable-sync \
      --disable-translate \
      --disable-infobars \
      --disable-features=$DISABLE_FEATURES \
      --disable-background-networking \
      --noerrdialogs \
      --disable-extensions \
      --autoplay-policy=no-user-gesture-required \
      --disable-client-side-phishing-detection \
      --enable-logging=stderr \
      --v=0 \
      --hide-scrollbars \
      --no-default-browser-check \
      --disable-background-timer-throttling \
      --disable-renderer-backgrounding \
      --disable-hang-monitor \
      --deny-permission-prompts \
      --disable-session-crashed-bubble \
      '$AC_URL'
  " 2>>"$LOG_DIR/aesthetic-kiosk.log"
fi
