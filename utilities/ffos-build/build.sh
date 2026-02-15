#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_DIR="$ROOT_DIR/utilities/ffos-build"
CACHE_DIR="$WORK_DIR/.ffos-cache"
OUT_DIR="$CACHE_DIR/out"

FFOS_REPO="${FFOS_REPO:-https://github.com/feral-file/ffos.git}"
FFOS_USER_REPO="${FFOS_USER_REPO:-https://github.com/feral-file/ffos-user.git}"
FFOS_BRANCH="${FFOS_BRANCH:-develop}"
FFOS_USER_BRANCH="${FFOS_USER_BRANCH:-develop}"
VERSION="${VERSION:-1.0.0}"

# Ensure Docker is available
if ! sudo docker info >/dev/null 2>&1; then
  echo "❌ Docker is not available from this environment."
  echo "   - Ensure the Docker daemon is running on the host."
  echo "   - Ensure you have sudo access to docker."
  exit 1
fi

mkdir -p "$CACHE_DIR" "$OUT_DIR"

echo "📦 Cloning/updating FFOS repos..."

# Clone or update ffos
if [ ! -d "$CACHE_DIR/ffos/.git" ]; then
  git clone --branch "$FFOS_BRANCH" --depth 1 "$FFOS_REPO" "$CACHE_DIR/ffos"
else
  git -C "$CACHE_DIR/ffos" fetch origin "$FFOS_BRANCH"
  git -C "$CACHE_DIR/ffos" reset --hard "origin/$FFOS_BRANCH"
fi

# Clone or update ffos-user
if [ ! -d "$CACHE_DIR/ffos-user/.git" ]; then
  git clone --branch "$FFOS_USER_BRANCH" --depth 1 "$FFOS_USER_REPO" "$CACHE_DIR/ffos-user"
else
  git -C "$CACHE_DIR/ffos-user" fetch origin "$FFOS_USER_BRANCH"
  git -C "$CACHE_DIR/ffos-user" reset --hard "origin/$FFOS_USER_BRANCH"
fi

# Apply local overlays (optional)
if [ -d "$WORK_DIR/overlays/ffos-user" ]; then
  echo "📝 Applying local overlays..."
  rsync -a "$WORK_DIR/overlays/ffos-user/" "$CACHE_DIR/ffos-user/"
fi

echo "🔨 Building Docker image..."
IMAGE_NAME="ffos-local-builder"
docker build -t "$IMAGE_NAME" -f "$WORK_DIR/Dockerfile" "$WORK_DIR"

echo "🏗️ Building components and ISO inside container..."

# Run build-inner.sh inside the container (avoids nested quoting issues)
docker run --rm \
  --privileged \
  -v "$CACHE_DIR/ffos:/work/ffos" \
  -v "$CACHE_DIR/ffos-user:/work/ffos-user" \
  -v "$WORK_DIR/overlays:/work/overlays:ro" \
  -v "$WORK_DIR/build-inner.sh:/work/build-inner.sh:ro" \
  -v "$OUT_DIR:/work/out" \
  -e VERSION="$VERSION" \
  "$IMAGE_NAME" 'bash /work/build-inner.sh'

echo "✅ Build complete. ISO output: $OUT_DIR"
ls -lh "$OUT_DIR"/*.iso 2>/dev/null || echo "No ISO files found"

# Show verification info
echo ""
echo "════════════════════════════════════════════════════════════════"
echo "  To verify and flash the ISO, use:"
echo "    bash utilities/ffos-build/verify-iso.sh $OUT_DIR/*.iso /dev/sdX"
echo ""
echo "  Or verify only:"
echo "    bash utilities/ffos-build/verify-iso.sh --verify-only $OUT_DIR/*.iso"
echo "════════════════════════════════════════════════════════════════"