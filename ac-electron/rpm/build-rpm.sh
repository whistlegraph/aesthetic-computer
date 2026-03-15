#!/usr/bin/env bash
set -euo pipefail

# Build a native Fedora RPM from the electron-builder linux-unpacked output.
#
# Usage:
#   bash rpm/build-rpm.sh                     # build from local linux-unpacked
#   bash rpm/build-rpm.sh --install           # build + install locally
#   bash rpm/build-rpm.sh --from-host         # build + install on Docker host
#
# Prerequisites:
#   - rpmbuild (sudo dnf install rpm-build)
#   - electron-builder linux-unpacked build (auto-created if missing)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AC_ELECTRON="$(cd "$SCRIPT_DIR/.." && pwd)"

FROM_HOST=0
DO_INSTALL=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --from-host)  FROM_HOST=1; DO_INSTALL=1; shift ;;
    --install)    DO_INSTALL=1; shift ;;
    -h|--help)    sed -n '3,13p' "$0"; exit 0 ;;
    *)            echo "Unknown: $1"; exit 1 ;;
  esac
done

VERSION="$(node -p "require('$AC_ELECTRON/package.json').version")"
echo "==> Aesthetic Computer v${VERSION} — Fedora RPM build"

if [[ "$FROM_HOST" -eq 1 ]]; then
  MACHINES_FILE="$(cd "$AC_ELECTRON/.." && pwd)/aesthetic-computer-vault/machines.json"
  HOST_TARGET="$(jq -r '.machines["jas-fedora"].host // .machines["jas-fedora"].ip' "$MACHINES_FILE" 2>/dev/null || echo "172.17.0.1")"
  HOST_USER="$(jq -r '.machines["jas-fedora"].user // "me"' "$MACHINES_FILE" 2>/dev/null || echo "me")"
  REMOTE="$HOST_USER@$HOST_TARGET"
  HOST_REPO="/home/$HOST_USER/aesthetic-computer"

  echo "==> Building on host ($REMOTE)..."
  ssh -o StrictHostKeyChecking=no "$REMOTE" bash -s -- "$VERSION" "$HOST_REPO" <<'HOSTSCRIPT'
set -euo pipefail
VERSION="$1"
HOST_REPO="$2"
cd "$HOST_REPO/ac-electron"

# Ensure repo is up to date
git pull -q

# Build electron linux-unpacked if needed
if [[ ! -d dist/linux-unpacked ]] || [[ dist/linux-unpacked -ot package.json ]]; then
  echo "==> electron-builder --linux dir..."
  npm install --no-audit --no-fund -q 2>/dev/null
  npx electron-builder --linux dir --x64
fi

# Ensure rpmbuild is available
command -v rpmbuild >/dev/null 2>&1 || sudo dnf install -y rpm-build

RPMBUILD_DIR="$HOME/rpmbuild"
mkdir -p "$RPMBUILD_DIR"/{SOURCES,SPECS,BUILD,RPMS,SRPMS}

echo "==> Packaging sources..."
(cd dist && tar czf "$RPMBUILD_DIR/SOURCES/aesthetic-computer-${VERSION}-linux-unpacked.tar.gz" linux-unpacked/)
cp rpm/aesthetic-computer.desktop "$RPMBUILD_DIR/SOURCES/aesthetic-computer.desktop"
(cd build && tar czf "$RPMBUILD_DIR/SOURCES/aesthetic-computer-icons.tar.gz" icons/)
cp rpm/aesthetic-computer.spec "$RPMBUILD_DIR/SPECS/"

echo "==> rpmbuild..."
rpmbuild -bb \
  --define "rpm_version $VERSION" \
  --define "_topdir $RPMBUILD_DIR" \
  "$RPMBUILD_DIR/SPECS/aesthetic-computer.spec"

RPM_PATH="$(ls "$RPMBUILD_DIR/RPMS/x86_64/aesthetic-computer-${VERSION}"*.rpm | head -1)"
echo "==> Built: $RPM_PATH"

echo "==> Installing..."
sudo dnf install -y --allowerasing "$RPM_PATH"

echo "==> Done! aesthetic-computer v${VERSION} installed."
rpm -q aesthetic-computer
HOSTSCRIPT

  exit 0
fi

# --- Local build ---

UNPACKED="$AC_ELECTRON/dist/linux-unpacked"

if [[ ! -d "$UNPACKED" ]]; then
  echo "==> No linux-unpacked dir. Building with electron-builder..."
  cd "$AC_ELECTRON"
  npx electron-builder --linux dir --x64
fi

if [[ ! -f "$UNPACKED/aesthetic-computer" ]]; then
  echo "Error: $UNPACKED/aesthetic-computer not found"
  exit 1
fi

command -v rpmbuild >/dev/null 2>&1 || { echo "rpmbuild not found. Install: sudo dnf install rpm-build"; exit 1; }

RPMBUILD_DIR="$HOME/rpmbuild"
mkdir -p "$RPMBUILD_DIR"/{SOURCES,SPECS,BUILD,RPMS,SRPMS}

echo "==> Packaging sources..."
(cd "$AC_ELECTRON/dist" && tar czf "$RPMBUILD_DIR/SOURCES/aesthetic-computer-${VERSION}-linux-unpacked.tar.gz" linux-unpacked/)
cp "$SCRIPT_DIR/aesthetic-computer.desktop" "$RPMBUILD_DIR/SOURCES/aesthetic-computer.desktop"
(cd "$AC_ELECTRON/build" && tar czf "$RPMBUILD_DIR/SOURCES/aesthetic-computer-icons.tar.gz" icons/)
cp "$SCRIPT_DIR/aesthetic-computer.spec" "$RPMBUILD_DIR/SPECS/"

echo "==> rpmbuild..."
rpmbuild -bb \
  --define "rpm_version $VERSION" \
  --define "_topdir $RPMBUILD_DIR" \
  "$RPMBUILD_DIR/SPECS/aesthetic-computer.spec"

RPM_FILE="$(ls "$RPMBUILD_DIR/RPMS/x86_64/aesthetic-computer-${VERSION}"*.rpm | head -1)"
cp "$RPM_FILE" "$AC_ELECTRON/dist/"
echo "==> RPM: $AC_ELECTRON/dist/$(basename "$RPM_FILE")"

if [[ "$DO_INSTALL" -eq 1 ]]; then
  echo "==> Installing..."
  sudo dnf install -y --allowerasing "$RPM_FILE"
  echo "==> Done!"
fi
