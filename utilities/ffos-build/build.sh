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
FFOS_VERSION="${FFOS_VERSION:-}"

# Ensure Docker is available
if ! docker info >/dev/null 2>&1; then
  echo "❌ Docker is not доступible from this environment."
  echo "   - Ensure the Docker daemon is running on the host."
  echo "   - Ensure your user can access /var/run/docker.sock (docker group)."
  echo "   - Or run this script on the host instead of inside a devcontainer."
  exit 1
fi

mkdir -p "$CACHE_DIR" "$OUT_DIR"

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
  rsync -a "$WORK_DIR/overlays/ffos-user/" "$CACHE_DIR/ffos-user/"
fi

# Build Docker image
IMAGE_NAME="ffos-local-builder"
docker build -t "$IMAGE_NAME" -f "$WORK_DIR/Dockerfile" "$WORK_DIR"

# Run mkarchiso inside container
# The ffos repo expects ffos-user content merged into archiso profile during build.
# We mirror that workflow by mounting both repos and a build output directory.
# Run as root to avoid permission issues with mounted volumes.

docker run --rm \
  --user root \
  -v "$CACHE_DIR/ffos:/work/ffos" \
  -v "$CACHE_DIR/ffos-user:/work/ffos-user" \
  -v "$OUT_DIR:/work/out" \
  "$IMAGE_NAME" "\
    set -euo pipefail; \
    cd /work/ffos; \
    # Use archiso profile in repo
    PROFILE=archiso-ff1; \
    # Ensure airootfs/home exists
    mkdir -p /work/ffos/\$PROFILE/airootfs/home; \
    # Merge ffos-user data into airootfs
    rsync -a /work/ffos-user/users/ /work/ffos/\$PROFILE/airootfs/home/; \
    # Build ISO
    mkarchiso -v -o /work/out /work/ffos/\$PROFILE; \
    # Rename ISO if version provided
    if [ -n \"$FFOS_VERSION\" ]; then \
      ISO=\$(ls -1 /work/out/*.iso | head -n 1); \
      mv \"\$ISO\" \"/work/out/FF1-local-\${FFOS_VERSION}.iso\"; \
    fi; \
  "

echo "✅ Build complete. ISO output: $OUT_DIR"