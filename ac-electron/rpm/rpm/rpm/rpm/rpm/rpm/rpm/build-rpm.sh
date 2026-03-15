#!/usr/bin/env bash
set -euo pipefail

# Build a native Fedora RPM from the electron-builder linux-unpacked output.
#
# Usage:
#   bash rpm/build-rpm.sh                     # build from local linux-unpacked
#   bash rpm/build-rpm.sh --from-host         # build on Docker host via SSH
#   bash rpm/build-rpm.sh --install           # build + install locally
#   bash rpm/build-rpm.sh --from-host --install  # build on host + install
#
# Prerequisites:
#   - electron-builder linux-unpacked build must exist (npm run build:linux or
#     npx electron-builder --linux dir --x64)
#   - rpmbuild (sudo dnf install rpm-build)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
AC_ELECTRON="$(cd "$SCRIPT_DIR/.." && pwd)"
RPM_DIR="$SCRIPT_DIR"
SPEC="$RPM_DIR/aesthetic-computer.spec"
DESKTOP="$RPM_DIR/aesthetic-computer.desktop"
ICONS_DIR="$AC_ELECTRON/build/icons"

FROM_HOST=0
DO_INSTALL=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --from-host)  FROM_HOST=1; shift ;;
    --install)    DO_INSTALL=1; shift ;;
    -h|--help)
      sed -n '3,12p' "$0"
      exit 0
      ;;
    *) echo "Unknown: $1"; exit 1 ;;
  esac
done

VERSION="$(node -p "require('$AC_ELECTRON/package.json').version")"
echo "Building RPM for Aesthetic Computer v${VERSION}"

UNPACKED="$AC_ELECTRON/dist/linux-unpacked"

# If --from-host, ensure electron-builder runs on the Docker host
if [[ "$FROM_HOST" -eq 1 ]]; then
  MACHINES_FILE="$(cd "$AC_ELECTRON/.." && pwd)/aesthetic-computer-vault/machines.json"
  HOST_TARGET="$(jq -r '.machines["jas-fedora"].host // .machines["jas-fedora"].ip' "$MACHINES_FILE" 2>/dev/null || echo "172.17.0.1")"
  HOST_USER="$(jq -r '.machines["jas-fedora"].user // "me"' "$MACHINES_FILE" 2>/dev/null || echo "me")"
  HOST_REPO="$(jq -r '.machines["jas-fedora"].repoPath // empty' "$MACHINES_FILE" 2>/dev/null)"
  REMOTE="$HOST_USER@$HOST_TARGET"

  if [[ -z "$HOST_REPO" ]]; then
    HOST_REPO="/home/$HOST_USER/aesthetic-computer"
  fi

  echo "Building linux-unpacked on host ($REMOTE)..."
  ssh -o StrictHostKeyChecking=no "$REMOTE" \
    "cd '$HOST_REPO/ac-electron' && git pull -q && npm install --no-audit --no-fund -q && npx electron-builder --linux dir --x64"

  UNPACKED="$HOST_REPO/ac-electron/dist/linux-unpacked"

  # Build the RPM on the host too (it has native rpmbuild)
  echo "Building RPM on host..."

  # Create tarball sources on host
  ssh -o StrictHostKeyChecking=no "$REMOTE" bash -s <<REMOTE_SCRIPT
set -euo pipefail
cd "$HOST_REPO/ac-electron"
VERSION="$VERSION"
RPMBUILD_DIR="\$HOME/rpmbuild"
mkdir -p "\$RPMBUILD_DIR"/{SOURCES,SPECS,BUILD,RPMS,SRPMS}

# Source 0: linux-unpacked tarball
cd dist
tar czf "\$RPMBUILD_DIR/SOURCES/aesthetic-computer-\${VERSION}-linux-unpacked.tar.gz" linux-unpacked/
cd ..

# Source 1: desktop file
cp rpm/aesthetic-computer.desktop "\$RPMBUILD_DIR/SOURCES/aesthetic-computer.desktop"

# Source 2: icons tarball
cd build
tar czf "\$RPMBUILD_DIR/SOURCES/aesthetic-computer-icons.tar.gz" icons/
cd ..

# Spec file
cp rpm/aesthetic-computer.spec "\$RPMBUILD_DIR/SPECS/"

# Build RPM
rpmbuild -bb \
  --define "rpm_version \$VERSION" \
  --define "_topdir \$RPMBUILD_DIR" \
  "\$RPMBUILD_DIR/SPECS/aesthetic-computer.spec"

echo "RPM built:"
ls -lh "\$RPMBUILD_DIR/RPMS/x86_64/"*aesthetic*
REMOTE_SCRIPT

  RPM_PATH="$(ssh -o StrictHostKeyChecking=no "$REMOTE" "ls $HOST_USER_HOME/rpmbuild/RPMS/x86_64/aesthetic-computer-${VERSION}*.rpm 2>/dev/null || ls /home/$HOST_USER/rpmbuild/RPMS/x86_64/aesthetic-computer-${VERSION}*.rpm" | head -1)"

  if [[ "$DO_INSTALL" -eq 1 ]]; then
    echo "Installing RPM on host..."
    ssh -o StrictHostKeyChecking=no "$REMOTE" "sudo dnf install -y --allowerasing '$RPM_PATH'"
    echo "Installed! Run 'aesthetic-computer' to launch."
  else
    echo "RPM ready on host: $RPM_PATH"
    echo "Install with: sudo dnf install -y --allowerasing $RPM_PATH"
  fi
  exit 0
fi

# --- Local build (inside devcontainer or on Fedora host directly) ---

if [[ ! -d "$UNPACKED" ]]; then
  echo "No linux-unpacked dir found. Building..."
  cd "$AC_ELECTRON"
  npx electron-builder --linux dir --x64
fi

if [[ ! -f "$UNPACKED/aesthetic-computer" ]]; then
  echo "Error: $UNPACKED/aesthetic-computer not found"
  exit 1
fi

if ! command -v rpmbuild >/dev/null 2>&1; then
  echo "rpmbuild not found. Install with: sudo dnf install rpm-build"
  exit 1
fi

RPMBUILD_DIR="$HOME/rpmbuild"
mkdir -p "$RPMBUILD_DIR"/{SOURCES,SPECS,BUILD,RPMS,SRPMS}

echo "Packaging sources..."

# Source 0: linux-unpacked tarball
cd "$AC_ELECTRON/dist"
tar czf "$RPMBUILD_DIR/SOURCES/aesthetic-computer-${VERSION}-linux-unpacked.tar.gz" linux-unpacked/
cd ..

# Source 1: desktop file
cp "$DESKTOP" "$RPMBUILD_DIR/SOURCES/aesthetic-computer.desktop"

# Source 2: icons tarball
cd "$AC_ELECTRON/build"
tar czf "$RPMBUILD_DIR/SOURCES/aesthetic-computer-icons.tar.gz" icons/
cd ..

# Spec
cp "$SPEC" "$RPMBUILD_DIR/SPECS/"

echo "Running rpmbuild..."
rpmbuild -bb \
  --define "rpm_version $VERSION" \
  --define "_topdir $RPMBUILD_DIR" \
  "$RPMBUILD_DIR/SPECS/aesthetic-computer.spec"

RPM_FILE="$(ls "$RPMBUILD_DIR/RPMS/x86_64/aesthetic-computer-${VERSION}"*.rpm | head -1)"
echo ""
echo "RPM built: $RPM_FILE"

# Copy to dist/ for convenience
cp "$RPM_FILE" "$AC_ELECTRON/dist/"
echo "Copied to: $AC_ELECTRON/dist/$(basename "$RPM_FILE")"

if [[ "$DO_INSTALL" -eq 1 ]]; then
  echo "Installing..."
  sudo dnf install -y --allowerasing "$RPM_FILE"
  echo "Installed! Run 'aesthetic-computer' to launch."
fi
