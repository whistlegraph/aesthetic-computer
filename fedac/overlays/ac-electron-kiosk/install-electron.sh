#!/bin/bash
# install-electron.sh — Download and install the latest AC Electron AppImage
set -euo pipefail

INSTALL_DIR="/opt/ac/bin"
APP_NAME="aesthetic-computer.AppImage"
REPO="whistlegraph/aesthetic-computer"

echo "FedAC: Installing AC Electron..."

# Ensure install directory exists
sudo mkdir -p "$INSTALL_DIR"

# Get latest release AppImage URL from GitHub
echo "Fetching latest release from GitHub..."
RELEASE_URL=$(curl -sL "https://api.github.com/repos/${REPO}/releases/latest" | \
  python3 -c "
import sys, json
try:
    r = json.load(sys.stdin)
    for a in r.get('assets', []):
        if a['name'].endswith('.AppImage'):
            print(a['browser_download_url'])
            break
except:
    pass
" 2>/dev/null)

if [ -z "$RELEASE_URL" ]; then
  echo "ERROR: Could not find AppImage in latest release."
  echo "Check: https://github.com/${REPO}/releases"
  exit 1
fi

echo "Downloading: $RELEASE_URL"
sudo curl -L --progress-bar -o "${INSTALL_DIR}/${APP_NAME}" "$RELEASE_URL"
sudo chmod +x "${INSTALL_DIR}/${APP_NAME}"

# Verify it runs
if "${INSTALL_DIR}/${APP_NAME}" --no-sandbox --version 2>/dev/null; then
  echo "AC Electron installed successfully."
else
  echo "Installed (could not verify version — may need FUSE or --appimage-extract-and-run)."
fi

echo "Location: ${INSTALL_DIR}/${APP_NAME}"
