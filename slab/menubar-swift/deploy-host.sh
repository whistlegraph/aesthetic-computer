#!/usr/bin/env bash
# Install the already-built Slab menubar on a fleet Mac without compiling there.
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
TARGET="${1:-}"
PREBUILT="${SLAB_PREBUILT:-$HERE/.build/release/slab-menubar-swift}"

if [[ ! "$TARGET" =~ ^[A-Za-z0-9._-]+$ ]]; then
  echo "usage: deploy-host.sh SSH_ALIAS" >&2
  exit 2
fi
[[ -x "$PREBUILT" ]] || {
  echo "missing release binary: $PREBUILT (run: cd slab/menubar-swift && swift build -c release)" >&2
  exit 1
}

REMOTE_HOME="$(ssh -o BatchMode=yes -o ConnectTimeout=6 "$TARGET" 'printf %s "$HOME"')"
[[ "$REMOTE_HOME" =~ ^/Users/[A-Za-z0-9._-]+$ ]] || {
  echo "refusing unexpected remote home from $TARGET: $REMOTE_HOME" >&2
  exit 1
}
STAGE="$REMOTE_HOME/.local/share/slab-menubar-deploy"

ssh "$TARGET" "mkdir -p '$STAGE/menubar-swift' '$STAGE/bin'"
scp -q "$PREBUILT" "$TARGET:$STAGE/slab-menubar-swift"
scp -q "$HERE/install.sh" "$HERE/Info.plist" \
  "$HERE/computer.slab.menubar.plist.tmpl" "$TARGET:$STAGE/menubar-swift/"
scp -q "$REPO/slab/bin/build-lock.sh" "$TARGET:$STAGE/bin/"
if [[ -f "$HERE/AppIcon.icns" ]]; then
  scp -q "$HERE/AppIcon.icns" "$TARGET:$STAGE/menubar-swift/"
fi
if [[ -d "$HERE/Resources" ]]; then
  ssh "$TARGET" "mkdir -p '$STAGE/menubar-swift/Resources'"
  scp -q "$HERE/Resources/"*.ttf "$TARGET:$STAGE/menubar-swift/Resources/" 2>/dev/null || true
fi

ssh "$TARGET" "chmod 755 '$STAGE/slab-menubar-swift' '$STAGE/menubar-swift/install.sh' '$STAGE/bin/build-lock.sh'; SLAB_PREBUILT='$STAGE/slab-menubar-swift' '$STAGE/menubar-swift/install.sh'"
echo "deployed persistent Slab stats to $TARGET"
