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

# Build version string from git (append -dirty if uncommitted changes)
GIT_HASH=$(git -C "$SCRIPT_DIR" rev-parse --short HEAD 2>/dev/null || echo "unknown")
if ! git -C "$SCRIPT_DIR" diff --quiet HEAD 2>/dev/null; then
  GIT_HASH="${GIT_HASH}-dirty"
fi
BUILD_TS=$(date -u '+%Y-%m-%dT%H:%M')
# Format must match AC_GIT_HASH "-" AC_BUILD_TS in js-bindings.c / Makefile
VERSION="${GIT_HASH}-${BUILD_TS}"

# Compute SHA256
SHA256=$(sha256sum "$VMLINUZ" | awk '{print $1}')
SIZE=$(stat -c%s "$VMLINUZ")

# Generate build name from MongoDB (global counter + day-of-year animal)
BUILD_NAME=""
BUILD_NUM=""
if command -v node &>/dev/null; then
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

  curl -sf -X PUT \
    -H "Date: $date_val" \
    -H "Content-Type: $content_type" \
    -H "Content-MD5: $md5_val" \
    -H "x-amz-acl: $acl" \
    -H "Authorization: AWS ${DO_SPACES_KEY}:${sig}" \
    --data-binary "@$src" \
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

# Upload files (vmlinuz last — it's large, others are the canary)
do_upload "$TMP/version.txt"  "os/${CHANNEL_PREFIX}native-notepat-latest.version"  "text/plain"
do_upload "$TMP/sha256.txt"   "os/${CHANNEL_PREFIX}native-notepat-latest.sha256"   "text/plain"
do_upload "$VMLINUZ"          "os/${CHANNEL_PREFIX}native-notepat-latest.vmlinuz"  "application/octet-stream"

# Fetch existing releases.json (or start fresh)
RELEASES_JSON="$TMP/releases.json"
curl -sf "${BASE_URL}/os/${CHANNEL_PREFIX}releases.json" -o "$RELEASES_JSON" 2>/dev/null \
  || echo '{"releases":[]}' > "$RELEASES_JSON"

# Append new entry (keep last 50)
python3 - "$RELEASES_JSON" "$FULL_VERSION" "$SHA256" "$SIZE" "$GIT_HASH" "$BUILD_TS" "$BUILD_NAME" <<'PYEOF'
import sys, json
path, version, sha256, size, git_hash, build_ts, name = sys.argv[1:]
with open(path) as f:
    data = json.load(f)
releases = data.get("releases", [])
releases.insert(0, {
    "version": version,
    "name": name,
    "sha256": sha256,
    "size": int(size),
    "git_hash": git_hash,
    "build_ts": build_ts,
    "url": "https://releases.aesthetic.computer/os/${CHANNEL_PREFIX}native-notepat-latest.vmlinuz"
})
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

# Upload ISO if available (from --iso flag or alongside vmlinuz)
if [ "$ISO_ONLY" = "1" ]; then
  echo "Uploading ISO: $(du -sh "$ISO_PATH" | cut -f1)"
  do_upload "$ISO_PATH" "os/${CHANNEL_PREFIX}native-notepat-latest.iso" "application/octet-stream"
  echo "ISO published: ${BASE_URL}/os/${CHANNEL_PREFIX}native-notepat-latest.iso"
  exit 0
fi

# Also upload ISO if it exists next to vmlinuz
ISO_SIBLING="$(dirname "$VMLINUZ")/ac-os.iso"
if [ -f "$ISO_SIBLING" ]; then
  echo "  Uploading ISO ($(du -sh "$ISO_SIBLING" | cut -f1))..."
  do_upload "$ISO_SIBLING" "os/${CHANNEL_PREFIX}native-notepat-latest.iso" "application/octet-stream"
fi

echo ""
echo "Release published: $BUILD_NAME ($FULL_VERSION)"
echo "  ${BASE_URL}/os/${CHANNEL_PREFIX}native-notepat-latest.vmlinuz"
echo "  ${BASE_URL}/os/${CHANNEL_PREFIX}releases.json"
[ -f "$ISO_SIBLING" ] && echo "  ${BASE_URL}/os/${CHANNEL_PREFIX}native-notepat-latest.iso"
