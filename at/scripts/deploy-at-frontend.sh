#!/usr/bin/env bash
set -euo pipefail

# Deploy AT frontend pages to PDS caddy container.
# Intended for both local use and CI (GitHub Actions).
#
# Required env (or defaults):
#   AT_PDS_HOST              (default: 165.227.120.137)
#   AT_PDS_USER              (default: root)
#   AT_PDS_SSH_KEY_PATH      (default: ~/.ssh/aesthetic_pds)
#   AT_PDS_CONTAINER         (default: caddy)
#   AT_PDS_CONTAINER_WEBROOT (default: /data/www)
#   AT_FRONTEND_FILE_MAP     (default: at/index.html:index.html;at/user-page.html:user.html;at/media-modal.js:media-modal.js;at/media-records.js:media-records.js)
#
# AT_FRONTEND_FILE_MAP format:
#   "repo/source/path:container/target/path;repo/source2:path2"

AT_PDS_HOST="${AT_PDS_HOST:-165.227.120.137}"
AT_PDS_USER="${AT_PDS_USER:-root}"
AT_PDS_SSH_KEY_PATH="${AT_PDS_SSH_KEY_PATH:-$HOME/.ssh/aesthetic_pds}"
AT_PDS_CONTAINER="${AT_PDS_CONTAINER:-caddy}"
AT_PDS_CONTAINER_WEBROOT="${AT_PDS_CONTAINER_WEBROOT:-/data/www}"
AT_FRONTEND_FILE_MAP="${AT_FRONTEND_FILE_MAP:-at/index.html:index.html;at/user-page.html:user.html;at/media-modal.js:media-modal.js;at/media-records.js:media-records.js}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

trim() {
  local value="$1"
  value="${value#"${value%%[![:space:]]*}"}"
  value="${value%"${value##*[![:space:]]}"}"
  printf "%s" "${value}"
}

declare -a SOURCE_FILES=()
declare -a TARGET_FILES=()
IFS=';' read -r -a FILE_MAP_ENTRIES <<< "$AT_FRONTEND_FILE_MAP"
for raw_entry in "${FILE_MAP_ENTRIES[@]}"; do
  entry="$(trim "$raw_entry")"
  [[ -z "$entry" ]] && continue

  if [[ "$entry" != *:* ]]; then
    echo "Invalid AT_FRONTEND_FILE_MAP entry: '$entry' (expected source:target)" >&2
    exit 1
  fi

  source_path="$(trim "${entry%%:*}")"
  target_path="$(trim "${entry#*:}")"
  local_source="$REPO_ROOT/$source_path"

  if [[ -z "$source_path" || -z "$target_path" ]]; then
    echo "Invalid AT_FRONTEND_FILE_MAP entry: '$entry' (empty source or target)" >&2
    exit 1
  fi

  if [[ ! -f "$local_source" ]]; then
    echo "Missing source file: $local_source" >&2
    exit 1
  fi

  SOURCE_FILES+=("$source_path")
  TARGET_FILES+=("$target_path")
done

if [[ "${#SOURCE_FILES[@]}" -eq 0 ]]; then
  echo "No frontend files configured. Set AT_FRONTEND_FILE_MAP." >&2
  exit 1
fi

if [[ ! -f "$AT_PDS_SSH_KEY_PATH" ]]; then
  echo "Missing SSH key: $AT_PDS_SSH_KEY_PATH" >&2
  exit 1
fi

SSH_TARGET="$AT_PDS_USER@$AT_PDS_HOST"
SSH_OPTS=(
  -i "$AT_PDS_SSH_KEY_PATH"
  -o StrictHostKeyChecking=no
  -o UserKnownHostsFile=/dev/null
)

STAMP="${GITHUB_SHA:-$(date +%Y%m%d%H%M%S)}"
REMOTE_DIR="/tmp/at-frontend-${STAMP}"

echo "Deploying AT frontend to ${SSH_TARGET}"
echo "Uploading staging files..."
ssh "${SSH_OPTS[@]}" "$SSH_TARGET" "mkdir -p '$REMOTE_DIR'"

for i in "${!SOURCE_FILES[@]}"; do
  source_path="${SOURCE_FILES[$i]}"
  target_path="${TARGET_FILES[$i]}"
  local_source="$REPO_ROOT/$source_path"
  remote_stage="$REMOTE_DIR/${i}-$(basename "$target_path")"
  target_dir="$(dirname "$target_path")"

  scp "${SSH_OPTS[@]}" "$local_source" "${SSH_TARGET}:${remote_stage}"
  if [[ "$target_dir" != "." ]]; then
    ssh "${SSH_OPTS[@]}" "$SSH_TARGET" "docker exec '${AT_PDS_CONTAINER}' mkdir -p '${AT_PDS_CONTAINER_WEBROOT}/$target_dir'"
  fi
  ssh "${SSH_OPTS[@]}" "$SSH_TARGET" "docker cp '$remote_stage' '${AT_PDS_CONTAINER}:${AT_PDS_CONTAINER_WEBROOT}/$target_path'"
done

ssh "${SSH_OPTS[@]}" "$SSH_TARGET" "rm -rf '$REMOTE_DIR'"

echo "Deployment complete."
echo "Landing: https://at.aesthetic.computer"
echo "User page: https://jeffrey.at.aesthetic.computer"
