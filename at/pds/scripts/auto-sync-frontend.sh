#!/usr/bin/env bash
set -euo pipefail

# Auto-sync AT frontend pages from GitHub to the PDS Caddy container.
# Intended for cron/systemd on the PDS server.
#
# Env overrides:
#   AC_REPO="whistlegraph/aesthetic-computer"
#   AC_BRANCH="main"
#   AC_CONTAINER="caddy"
#   AC_CONTAINER_WEBROOT="/data/www"
#   AC_STATE_DIR="/var/lib/at-frontend-sync"
#   AC_HEALTH_URL="https://at.aesthetic.computer/xrpc/_health"
#   AC_FILE_MAP="at/index.html:index.html;at/user-page.html:user.html"
#   AC_FORCE="1"  # force deploy even if SHA unchanged
#
# AC_FILE_MAP format:
#   "repo/source/path:container/target/path;repo/source2:path2"

AC_REPO="${AC_REPO:-whistlegraph/aesthetic-computer}"
AC_BRANCH="${AC_BRANCH:-main}"
AC_CONTAINER="${AC_CONTAINER:-caddy}"
AC_CONTAINER_WEBROOT="${AC_CONTAINER_WEBROOT:-/data/www}"
AC_STATE_DIR="${AC_STATE_DIR:-/var/lib/at-frontend-sync}"
AC_HEALTH_URL="${AC_HEALTH_URL:-https://at.aesthetic.computer/xrpc/_health}"
AC_FILE_MAP="${AC_FILE_MAP:-at/index.html:index.html;at/user-page.html:user.html}"
AC_FORCE="${AC_FORCE:-0}"

RAW_BASE="https://raw.githubusercontent.com/${AC_REPO}"
REPO_URL="https://github.com/${AC_REPO}.git"
STATE_FILE="${AC_STATE_DIR}/last_deployed_sha"
LOCK_FILE="${AC_STATE_DIR}/sync.lock"
TMP_DIR="$(mktemp -d /tmp/at-frontend-sync.XXXXXX)"
COMPARE_JSON="${TMP_DIR}/compare.json"

trim() {
  local value="$1"
  value="${value#"${value%%[![:space:]]*}"}"
  value="${value%"${value##*[![:space:]]}"}"
  printf "%s" "${value}"
}

declare -a SOURCE_FILES=()
declare -a TARGET_FILES=()
IFS=';' read -r -a FILE_MAP_ENTRIES <<< "${AC_FILE_MAP}"
for raw_entry in "${FILE_MAP_ENTRIES[@]}"; do
  entry="$(trim "${raw_entry}")"
  [[ -z "${entry}" ]] && continue

  if [[ "${entry}" != *:* ]]; then
    echo "Invalid AC_FILE_MAP entry: '${entry}' (expected source:target)" >&2
    exit 1
  fi

  source_path="$(trim "${entry%%:*}")"
  target_path="$(trim "${entry#*:}")"

  if [[ -z "${source_path}" || -z "${target_path}" ]]; then
    echo "Invalid AC_FILE_MAP entry: '${entry}' (empty source or target)" >&2
    exit 1
  fi

  SOURCE_FILES+=("${source_path}")
  TARGET_FILES+=("${target_path}")
done

if [[ "${#SOURCE_FILES[@]}" -eq 0 ]]; then
  echo "No frontend files configured. Set AC_FILE_MAP." >&2
  exit 1
fi

cleanup() {
  rm -rf "${TMP_DIR}" || true
}
trap cleanup EXIT

mkdir -p "${AC_STATE_DIR}"

exec 9>"${LOCK_FILE}"
if ! flock -n 9; then
  echo "Auto-sync already running; exiting."
  exit 0
fi

if ! command -v docker >/dev/null 2>&1; then
  echo "docker is required but not found." >&2
  exit 1
fi

if ! command -v curl >/dev/null 2>&1; then
  echo "curl is required but not found." >&2
  exit 1
fi

if ! command -v jq >/dev/null 2>&1; then
  echo "jq is required but not found." >&2
  exit 1
fi

if ! docker ps --format '{{.Names}}' | grep -Fxq "${AC_CONTAINER}"; then
  echo "Container '${AC_CONTAINER}' is not running." >&2
  exit 1
fi

echo "Checking latest commit for ${AC_REPO}@${AC_BRANCH}..."
LATEST_SHA="$(git ls-remote "${REPO_URL}" "refs/heads/${AC_BRANCH}" | awk '{print $1}')"
if [[ -z "${LATEST_SHA}" ]]; then
  echo "Could not resolve latest SHA for ${AC_REPO}@${AC_BRANCH}" >&2
  exit 1
fi

LAST_SHA=""
if [[ -f "${STATE_FILE}" ]]; then
  LAST_SHA="$(cat "${STATE_FILE}")"
fi

if [[ "${AC_FORCE}" != "1" && "${LATEST_SHA}" == "${LAST_SHA}" ]]; then
  echo "No new commit (${LATEST_SHA:0:12}); frontend is up to date."
  exit 0
fi

if [[ "${AC_FORCE}" != "1" && -n "${LAST_SHA}" ]]; then
  COMPARE_URL="https://api.github.com/repos/${AC_REPO}/compare/${LAST_SHA}...${LATEST_SHA}"
  if curl -fsSL "${COMPARE_URL}" -o "${COMPARE_JSON}"; then
    SHOULD_DEPLOY=0
    for source_path in "${SOURCE_FILES[@]}"; do
      if jq -e --arg source_path "${source_path}" '.files[]? | select(.filename == $source_path)' "${COMPARE_JSON}" >/dev/null; then
        SHOULD_DEPLOY=1
        break
      fi
    done

    if [[ "${SHOULD_DEPLOY}" == "0" ]]; then
      echo "No tracked frontend files changed; skipping deploy for ${LATEST_SHA:0:12}."
      echo "${LATEST_SHA}" > "${STATE_FILE}"
      exit 0
    fi
  else
    echo "Could not compare commit diff. Continuing with deploy."
  fi
fi

echo "Deploying frontend for commit ${LATEST_SHA:0:12}..."

declare -a TMP_FILES=()
for i in "${!SOURCE_FILES[@]}"; do
  source_path="${SOURCE_FILES[$i]}"
  target_path="${TARGET_FILES[$i]}"
  source_url="${RAW_BASE}/${LATEST_SHA}/${source_path}"
  tmp_file="${TMP_DIR}/${i}-$(basename "${target_path}")"

  curl -fsSL "${source_url}" -o "${tmp_file}"
  if [[ "${source_path}" == *.html ]] && ! grep -qi "<!doctype html>" "${tmp_file}"; then
    echo "Downloaded ${source_path} does not look like HTML." >&2
    exit 1
  fi

  TMP_FILES+=("${tmp_file}")
done

for i in "${!TARGET_FILES[@]}"; do
  target_path="${TARGET_FILES[$i]}"
  tmp_file="${TMP_FILES[$i]}"
  target_dir="$(dirname "${target_path}")"
  if [[ "${target_dir}" != "." ]]; then
    docker exec "${AC_CONTAINER}" mkdir -p "${AC_CONTAINER_WEBROOT}/${target_dir}"
  fi
  docker cp "${tmp_file}" "${AC_CONTAINER}:${AC_CONTAINER_WEBROOT}/${target_path}"
done

echo "${LATEST_SHA}" > "${STATE_FILE}"

if [[ -n "${AC_HEALTH_URL}" ]]; then
  echo "Running health check..."
  curl -fsSL "${AC_HEALTH_URL}" >/dev/null
fi

echo "AT frontend sync complete (${LATEST_SHA:0:12})."
