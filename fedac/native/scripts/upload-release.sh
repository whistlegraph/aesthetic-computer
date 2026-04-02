#!/bin/bash
# upload-release.sh — Upload a built notepat vmlinuz to Digital Ocean Spaces
# Publishes: native-notepat-latest.vmlinuz, .sha256, .version, and updates releases.json
#
# Usage: ./upload-release.sh [vmlinuz_path]
#        ./upload-release.sh --iso [iso_path]    # Upload ISO only
# Credentials auto-loaded from aesthetic-computer-vault/fedac/native/upload.env

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ISO_ONLY=0
if [ "${1:-}" = "--iso" ]; then
  ISO_ONLY=1
  ISO_PATH="${2:-${SCRIPT_DIR}/../build/ac-os.iso}"
  if [ ! -f "$ISO_PATH" ]; then
    echo "Error: ISO not found at $ISO_PATH" >&2
    exit 1
  fi
  VMLINUZ="/dev/null"  # not used but needed for cred loading below
else
  VMLINUZ="${1:-${SCRIPT_DIR}/../build/vmlinuz}"
  if [ ! -f "$VMLINUZ" ]; then
    echo "Error: vmlinuz not found at $VMLINUZ" >&2
    exit 1
  fi
fi

# Credentials — check env (set by ac-os), session cache, plaintext vault, or GPG decrypt
if [ -z "${DO_SPACES_KEY:-}" ] || [ -z "${DO_SPACES_SECRET:-}" ]; then
  # Session cache (written by ac-os load_vault_creds)
  [ -f "/tmp/.ac-upload-env" ] && { set -a; source "/tmp/.ac-upload-env"; set +a; }
fi
if [ -z "${DO_SPACES_KEY:-}" ] || [ -z "${DO_SPACES_SECRET:-}" ]; then
  # Plaintext vault file
  VAULT_ENV="${SCRIPT_DIR}/../../../aesthetic-computer-vault/fedac/native/upload.env"
  [ -f "$VAULT_ENV" ] && { set -a; source "$VAULT_ENV"; set +a; }
fi
if [ -z "${DO_SPACES_KEY:-}" ] || [ -z "${DO_SPACES_SECRET:-}" ]; then
  # GPG decrypt from vault
  for gpg_file in \
      "${SCRIPT_DIR}/../upload.env.gpg" \
      "${SCRIPT_DIR}/../../../aesthetic-computer-vault/fedac/native/upload.env.gpg"; do
    if [ -f "$gpg_file" ]; then
      echo "[upload] Decrypting $(basename "$gpg_file")..."
      DECRYPTED=$(gpg --pinentry-mode loopback -d "$gpg_file" 2>/dev/null | grep "=") || true
      if [ -n "$DECRYPTED" ]; then
        set -a; eval "$DECRYPTED"; set +a
        break
      fi
    fi
  done
fi

: "${DO_SPACES_KEY:?DO_SPACES_KEY not set}"
: "${DO_SPACES_SECRET:?DO_SPACES_SECRET not set}"
DO_SPACES_BUCKET="${DO_SPACES_BUCKET:-releases-aesthetic-computer}"
DO_SPACES_REGION="${DO_SPACES_REGION:-sfo3}"

BASE_URL="https://${DO_SPACES_BUCKET}.${DO_SPACES_REGION}.digitaloceanspaces.com"

# Build version string from git. Dirty/conflicted uploads are blocked by default.
GIT_HASH=$(git -C "$SCRIPT_DIR" rev-parse --short HEAD 2>/dev/null || echo "unknown")
CONFLICT_FILES=$(git -C "$SCRIPT_DIR" diff --name-only --diff-filter=U 2>/dev/null || true)
if [ -n "$CONFLICT_FILES" ]; then
  echo "Error: refusing upload with unresolved merge conflicts:" >&2
  echo "$CONFLICT_FILES" >&2
  exit 1
fi
DIRTY_TRACKED=0
if ! git -C "$SCRIPT_DIR" diff --quiet HEAD 2>/dev/null; then
  DIRTY_TRACKED=1
fi
if [ "$DIRTY_TRACKED" -eq 1 ] && [ "${ALLOW_DIRTY_UPLOAD:-0}" != "1" ]; then
  echo "Error: refusing dirty upload. Commit/stash/reset native changes first." >&2
  git -C "$SCRIPT_DIR" status --porcelain --untracked-files=no -- fedac/native 2>/dev/null >&2 || true
  echo "Override only for emergencies: ALLOW_DIRTY_UPLOAD=1 ./scripts/upload-release.sh ..." >&2
  exit 1
fi
if [ "$DIRTY_TRACKED" -eq 1 ]; then
  GIT_HASH="${GIT_HASH}-dirty"
fi
BUILD_TS=$(date -u '+%Y-%m-%dT%H:%M')
# Format must match AC_GIT_HASH "-" AC_BUILD_TS in js-bindings.c / Makefile
VERSION="${GIT_HASH}-${BUILD_TS}"

# Compute SHA256
SHA256=$(sha256sum "$VMLINUZ" | awk '{print $1}')
SIZE=$(stat -c%s "$VMLINUZ")

# Build name: prefer AC_BUILD_NAME (set by oven/Makefile at compile time)
# so the uploaded name matches what the kernel displays on boot.
# Falls back to MongoDB counter if not set (local builds).
BUILD_NAME="${AC_BUILD_NAME:-}"
BUILD_NUM=""
if [ -z "$BUILD_NAME" ] && command -v node &>/dev/null; then
  NAME_JSON=$(node "$SCRIPT_DIR/track-build.mjs" next-name 2>/dev/null || echo '{}')
  BUILD_NAME=$(echo "$NAME_JSON" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('name',''))" 2>/dev/null || true)
  BUILD_NUM=$(echo "$NAME_JSON" | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('buildNum',''))" 2>/dev/null || true)
fi
if [ -z "$BUILD_NAME" ]; then
  BUILD_NAME="local-$(date -u +%s)"
fi

echo "Uploading notepat release: $VERSION"
echo "  name:    $BUILD_NAME (#${BUILD_NUM:-?})"
echo "  vmlinuz: $(du -sh "$VMLINUZ" | cut -f1)"
echo "  sha256:  $SHA256"

# Helper: upload file to DO Spaces via curl (AWS Sig v2)
# x-amz-acl must be in CanonicalizedAmzHeaders in the string-to-sign
do_upload() {
  local src="$1"
  local dest_key="$2"
  local content_type="${3:-application/octet-stream}"
  local acl="public-read"

  local date_val
  date_val=$(date -u '+%a, %d %b %Y %H:%M:%S GMT')
  local md5_val
  md5_val=$(openssl md5 -binary < "$src" | base64)
  local sig
  sig=$(printf 'PUT\n%s\n%s\n%s\nx-amz-acl:%s\n/%s/%s' \
    "$md5_val" "$content_type" "$date_val" "$acl" "$DO_SPACES_BUCKET" "$dest_key" \
    | openssl dgst -sha1 -hmac "$DO_SPACES_SECRET" -binary | base64)

  # Use -T (upload-file) for streaming — avoids loading entire file into memory
  # (--data-binary loads the whole file into RAM, OOM on 1GB+ files)
  curl -sf -X PUT \
    -H "Date: $date_val" \
    -H "Content-Type: $content_type" \
    -H "Content-MD5: $md5_val" \
    -H "x-amz-acl: $acl" \
    -H "Authorization: AWS ${DO_SPACES_KEY}:${sig}" \
    -T "$src" \
    "${BASE_URL}/${dest_key}" \
    && echo "  uploaded: $dest_key" \
    || { echo "  ERROR uploading $dest_key" >&2; return 1; }
}

# Write .version and .sha256 to temp files
TMP=$(mktemp -d)
trap "rm -rf $TMP" EXIT

# Include build name in version string for display on device
# Line 1: version string, Line 2: kernel size in bytes
FULL_VERSION="${BUILD_NAME} ${VERSION}"
printf '%s\n%s' "$FULL_VERSION" "$SIZE" > "$TMP/version.txt"
printf '%s' "$SHA256"  > "$TMP/sha256.txt"

# OTA channel prefix (empty for default C build, "cl-" for Common Lisp variant)
CHANNEL_PREFIX=""
if [ -n "${OTA_CHANNEL:-}" ]; then
  CHANNEL_PREFIX="${OTA_CHANNEL}-"
  echo "  channel: ${OTA_CHANNEL}"
fi

# ISO-only mode: upload ISO + version + sha256, then exit
if [ "$ISO_ONLY" = "1" ]; then
  ISO_SHA256=$(sha256sum "$ISO_PATH" | awk '{print $1}')
  ISO_SIZE=$(stat -c%s "$ISO_PATH" 2>/dev/null || stat -f%z "$ISO_PATH")
  printf '%s\n%s' "${FULL_VERSION}" "$ISO_SIZE" > "$TMP/version.txt"
  printf '%s' "$ISO_SHA256" > "$TMP/sha256.txt"
  echo "Uploading ISO: $(du -sh "$ISO_PATH" | cut -f1) sha256=${ISO_SHA256:0:16}..."
  do_upload "$TMP/version.txt" "os/${CHANNEL_PREFIX}native-notepat-latest.version" "text/plain"
  do_upload "$TMP/sha256.txt"  "os/${CHANNEL_PREFIX}native-notepat-latest.sha256"  "text/plain"
  do_upload "$ISO_PATH"        "os/${CHANNEL_PREFIX}native-notepat-latest.iso"     "application/octet-stream"
  echo "ISO published: ${BASE_URL}/os/${CHANNEL_PREFIX}native-notepat-latest.iso"
  exit 0
fi

# Upload files (vmlinuz last — it's large, others are the canary)
do_upload "$TMP/version.txt"  "os/${CHANNEL_PREFIX}native-notepat-latest.version"  "text/plain"
do_upload "$TMP/sha256.txt"   "os/${CHANNEL_PREFIX}native-notepat-latest.sha256"   "text/plain"
do_upload "$VMLINUZ"          "os/${CHANNEL_PREFIX}native-notepat-latest.vmlinuz"  "application/octet-stream"

# Fetch existing releases.json (or start fresh)
RELEASES_JSON="$TMP/releases.json"
curl -sf "${BASE_URL}/os/${CHANNEL_PREFIX}releases.json" -o "$RELEASES_JSON" 2>/dev/null \
  || echo '{"releases":[]}' > "$RELEASES_JSON"

# Append new entry (keep last 50)
COMMIT_MSG=$(git -C "$SCRIPT_DIR" log -1 --format="%s" 2>/dev/null || echo "")
BUILD_HANDLE="${AC_HANDLE:-}"

python3 - "$RELEASES_JSON" "$FULL_VERSION" "$SHA256" "$SIZE" "$GIT_HASH" "$BUILD_TS" "$BUILD_NAME" "$CHANNEL_PREFIX" "$COMMIT_MSG" "$BUILD_HANDLE" <<'PYEOF'
import sys, json
path, version, sha256, size, git_hash, build_ts, name, channel_prefix, commit_msg, handle = sys.argv[1:]
with open(path) as f:
    data = json.load(f)
releases = data.get("releases", [])
entry = {
    "version": version,
    "name": name,
    "sha256": sha256,
    "size": int(size),
    "git_hash": git_hash,
    "build_ts": build_ts,
    "url": f"https://releases-aesthetic-computer.sfo3.digitaloceanspaces.com/os/{channel_prefix}native-notepat-latest.vmlinuz",
    "commit_msg": commit_msg,
    "handle": handle,
}
releases.insert(0, entry)
data["releases"] = releases[:50]
data["latest"] = version
data["latest_name"] = name
with open(path, "w") as f:
    json.dump(data, f, indent=2)
PYEOF

do_upload "$RELEASES_JSON" "os/${CHANNEL_PREFIX}releases.json" "application/json"

# Record build in MongoDB
if command -v node &>/dev/null; then
  echo "{\"name\":\"$BUILD_NAME\",\"buildNum\":$BUILD_NUM,\"version\":\"$FULL_VERSION\",\"sha256\":\"$SHA256\",\"size\":$SIZE,\"git_hash\":\"$GIT_HASH\",\"build_ts\":\"$BUILD_TS\",\"url\":\"${BASE_URL}/os/native-notepat-latest.vmlinuz\"}" \
    | node "$SCRIPT_DIR/track-build.mjs" record 2>&1 || true
fi

# Upload slim kernel + initramfs for universal Mac/ThinkPad boot
SLIM_SIBLING="$(dirname "$VMLINUZ")/vmlinuz-slim"
INITRAMFS_SIBLING="$(dirname "$VMLINUZ")/initramfs.cpio.gz"
if [ -f "$SLIM_SIBLING" ]; then
  echo "  Uploading slim kernel ($(du -sh "$SLIM_SIBLING" | cut -f1))..."
  do_upload "$SLIM_SIBLING" "os/${CHANNEL_PREFIX}native-notepat-latest.vmlinuz-slim" "application/octet-stream"
fi
if [ -f "$INITRAMFS_SIBLING" ]; then
  echo "  Uploading initramfs ($(du -sh "$INITRAMFS_SIBLING" | cut -f1))..."
  do_upload "$INITRAMFS_SIBLING" "os/${CHANNEL_PREFIX}native-notepat-latest.initramfs.cpio.gz" "application/octet-stream"
fi

# Also upload ISO if it exists (non-fatal — ISO is optional)
ISO_SIBLING="$(dirname "$VMLINUZ")/ac-os.iso"
if [ -f "$ISO_SIBLING" ]; then
  echo "  Uploading ISO ($(du -sh "$ISO_SIBLING" | cut -f1))..."
  do_upload "$ISO_SIBLING" "os/${CHANNEL_PREFIX}native-notepat-latest.iso" "application/octet-stream" || echo "  ISO upload failed (non-fatal)"
fi

echo ""
echo "Release published: $BUILD_NAME ($FULL_VERSION)"
echo "  ${BASE_URL}/os/${CHANNEL_PREFIX}native-notepat-latest.vmlinuz"
if [ -f "$SLIM_SIBLING" ]; then
  echo "  ${BASE_URL}/os/${CHANNEL_PREFIX}native-notepat-latest.vmlinuz-slim"
  echo "  ${BASE_URL}/os/${CHANNEL_PREFIX}native-notepat-latest.initramfs.cpio.gz"
fi
echo "  ${BASE_URL}/os/${CHANNEL_PREFIX}releases.json"
if [ -f "$ISO_SIBLING" ]; then
  echo "  ${BASE_URL}/os/${CHANNEL_PREFIX}native-notepat-latest.iso"
fi
