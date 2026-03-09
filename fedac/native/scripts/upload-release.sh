#!/bin/bash
# upload-release.sh — Upload a built notepat vmlinuz to Digital Ocean Spaces
# Publishes: native-notepat-latest.vmlinuz, .sha256, .version, and updates releases.json
#
# Usage: ./upload-release.sh [vmlinuz_path]
# Credentials auto-loaded from aesthetic-computer-vault/fedac/native/upload.env

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
VMLINUZ="${1:-${SCRIPT_DIR}/../build/vmlinuz}"
if [ ! -f "$VMLINUZ" ]; then
  echo "Error: vmlinuz not found at $VMLINUZ" >&2
  exit 1
fi

# Credentials — source from vault if not already in env
VAULT_ENV="${SCRIPT_DIR}/../../../aesthetic-computer-vault/fedac/native/upload.env"
if [ -f "$VAULT_ENV" ]; then
  set -a; source "$VAULT_ENV"; set +a
fi

: "${DO_SPACES_KEY:?DO_SPACES_KEY not set}"
: "${DO_SPACES_SECRET:?DO_SPACES_SECRET not set}"
DO_SPACES_BUCKET="${DO_SPACES_BUCKET:-releases-aesthetic-computer}"
DO_SPACES_REGION="${DO_SPACES_REGION:-sfo3}"

BASE_URL="https://${DO_SPACES_BUCKET}.${DO_SPACES_REGION}.digitaloceanspaces.com"

# Build version string from git
GIT_HASH=$(git -C "$SCRIPT_DIR" rev-parse --short HEAD 2>/dev/null || echo "unknown")
BUILD_TS=$(date -u '+%Y-%m-%dT%H:%M')
# Format must match AC_GIT_HASH "-" AC_BUILD_TS in js-bindings.c / Makefile
VERSION="${GIT_HASH}-${BUILD_TS}"

# Compute SHA256
SHA256=$(sha256sum "$VMLINUZ" | awk '{print $1}')
SIZE=$(stat -c%s "$VMLINUZ")

echo "Uploading notepat release: $VERSION"
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

printf '%s' "$VERSION" > "$TMP/version.txt"
printf '%s' "$SHA256"  > "$TMP/sha256.txt"

# Upload files (vmlinuz last — it's large, others are the canary)
do_upload "$TMP/version.txt"  "os/native-notepat-latest.version"  "text/plain"
do_upload "$TMP/sha256.txt"   "os/native-notepat-latest.sha256"   "text/plain"
do_upload "$VMLINUZ"          "os/native-notepat-latest.vmlinuz"  "application/octet-stream"

# Fetch existing releases.json (or start fresh)
RELEASES_JSON="$TMP/releases.json"
curl -sf "${BASE_URL}/os/releases.json" -o "$RELEASES_JSON" 2>/dev/null \
  || echo '{"releases":[]}' > "$RELEASES_JSON"

# Append new entry (keep last 50)
python3 - "$RELEASES_JSON" "$VERSION" "$SHA256" "$SIZE" "$GIT_HASH" "$BUILD_TS" <<'PYEOF'
import sys, json
path, version, sha256, size, git_hash, build_ts = sys.argv[1:]
with open(path) as f:
    data = json.load(f)
releases = data.get("releases", [])
releases.insert(0, {
    "version": version,
    "sha256": sha256,
    "size": int(size),
    "git_hash": git_hash,
    "build_ts": build_ts,
    "url": "https://releases.aesthetic.computer/os/native-notepat-latest.vmlinuz"
})
data["releases"] = releases[:50]
data["latest"] = version
with open(path, "w") as f:
    json.dump(data, f, indent=2)
PYEOF

do_upload "$RELEASES_JSON" "os/releases.json" "application/json"

echo ""
echo "Release published: $VERSION"
echo "  ${BASE_URL}/os/native-notepat-latest.vmlinuz"
echo "  ${BASE_URL}/os/releases.json"
