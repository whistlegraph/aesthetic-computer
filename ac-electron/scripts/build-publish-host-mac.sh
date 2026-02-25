#!/usr/bin/env bash
set -euo pipefail

# Build/sign macOS artifacts on a remote Mac host (via GUI Terminal session),
# then optionally publish them to Spaces + register with silo.
#
# Usage examples:
#   bash scripts/build-publish-host-mac.sh
#   bash scripts/build-publish-host-mac.sh --notes "Desktop update"
#   bash scripts/build-publish-host-mac.sh --skip-publish
#   bash scripts/build-publish-host-mac.sh --dry-run-publish
#   bash scripts/build-publish-host-mac.sh --host-key mac-mini

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
MACHINES_FILE="$REPO_ROOT/aesthetic-computer-vault/machines.json"

HOST_KEY="jeffrey-macbook"
SKIP_PUBLISH=0
DRY_RUN_PUBLISH=0
RELEASE_NOTES=""
TIMEOUT_MIN=90

while [[ $# -gt 0 ]]; do
  case "$1" in
    --host-key)
      HOST_KEY="${2:-}"
      shift 2
      ;;
    --notes)
      RELEASE_NOTES="${2:-}"
      shift 2
      ;;
    --skip-publish)
      SKIP_PUBLISH=1
      shift
      ;;
    --dry-run-publish)
      DRY_RUN_PUBLISH=1
      shift
      ;;
    --timeout-min)
      TIMEOUT_MIN="${2:-90}"
      shift 2
      ;;
    -h|--help)
      sed -n '1,24p' "$0"
      exit 0
      ;;
    *)
      echo "Unknown argument: $1"
      exit 1
      ;;
  esac
done

if [[ ! -f "$MACHINES_FILE" ]]; then
  echo "machines.json not found: $MACHINES_FILE"
  exit 1
fi

if ! command -v jq >/dev/null 2>&1; then
  echo "jq is required"
  exit 1
fi

HOST_USER="$(jq -r ".machines[\"$HOST_KEY\"].user // empty" "$MACHINES_FILE")"
HOST_IP="$(jq -r ".machines[\"$HOST_KEY\"].ip // empty" "$MACHINES_FILE")"
HOST_HOSTNAME="$(jq -r ".machines[\"$HOST_KEY\"].host // empty" "$MACHINES_FILE")"
HOST_REPO="$(jq -r ".machines[\"$HOST_KEY\"].repoPath // empty" "$MACHINES_FILE")"

if [[ -z "$HOST_USER" || -z "$HOST_REPO" || ( -z "$HOST_IP" && -z "$HOST_HOSTNAME" ) ]]; then
  echo "Machine '$HOST_KEY' is missing user/repoPath/ip|host in $MACHINES_FILE"
  exit 1
fi

HOST_TARGET="$HOST_IP"
if [[ -n "$HOST_HOSTNAME" ]]; then
  HOST_TARGET="$HOST_HOSTNAME"
fi

REMOTE="$HOST_USER@$HOST_TARGET"
REMOTE_AC_DIR="$HOST_REPO/ac-electron"
BUILD_ID="ac-mac-release-$(date +%s)"
REMOTE_RUN="/tmp/${BUILD_ID}.sh"
REMOTE_LOG="/tmp/${BUILD_ID}.log"
REMOTE_STATUS="/tmp/${BUILD_ID}.status"
REMOTE_DONE="/tmp/${BUILD_ID}.done"

VERSION="$(node -p "require('$REPO_ROOT/ac-electron/package.json').version")"

echo "Host: $HOST_KEY ($REMOTE)"
echo "Repo: $HOST_REPO"
echo "Version: $VERSION"
echo "Build id: $BUILD_ID"

cat > "/tmp/${BUILD_ID}.remote.sh" <<EOF
#!/usr/bin/env bash
set -uo pipefail
cd "$REMOTE_AC_DIR" || exit 99
if [[ -f "$HOST_REPO/aesthetic-computer-vault/ac-electron/.env" ]]; then
  set -a
  # shellcheck disable=SC1091
  source "$HOST_REPO/aesthetic-computer-vault/ac-electron/.env"
  set +a
fi
npm run build:mac >"$REMOTE_LOG" 2>&1
status=\$?
echo "\$status" >"$REMOTE_STATUS"
date -u +"%Y-%m-%dT%H:%M:%SZ" >"$REMOTE_DONE"
exit "\$status"
EOF

scp -q "/tmp/${BUILD_ID}.remote.sh" "$REMOTE:$REMOTE_RUN"

ssh -o StrictHostKeyChecking=no "$REMOTE" "bash -lc 'chmod +x \"$REMOTE_RUN\" && rm -f \"$REMOTE_STATUS\" \"$REMOTE_DONE\" \"$REMOTE_LOG\" && osascript -e '\"'\"'tell application \"Terminal\" to do script \"$REMOTE_RUN\"'\"'\"''"

echo "Started signed build in host Terminal. Waiting for completion..."
deadline=$(( $(date +%s) + TIMEOUT_MIN * 60 ))
last_log_size=-1
stagnant_checks=0

while true; do
  if ssh -o StrictHostKeyChecking=no "$REMOTE" "test -f \"$REMOTE_DONE\" || test -f \"$REMOTE_STATUS\""; then
    break
  fi

  now="$(date +%s)"
  if [[ "$now" -ge "$deadline" ]]; then
    echo "Timed out waiting for host build after ${TIMEOUT_MIN} minutes"
    ssh -o StrictHostKeyChecking=no "$REMOTE" "bash -lc 'tail -n 80 \"$REMOTE_LOG\" 2>/dev/null || true'"
    exit 1
  fi

  log_size="$(ssh -o StrictHostKeyChecking=no "$REMOTE" "bash -lc 'stat -f %z \"$REMOTE_LOG\" 2>/dev/null || echo 0'")"
  if [[ "$log_size" != "$last_log_size" ]]; then
    ssh -o StrictHostKeyChecking=no "$REMOTE" "bash -lc 'tail -n 6 \"$REMOTE_LOG\" 2>/dev/null || true'"
    last_log_size="$log_size"
    stagnant_checks=0
  else
    stagnant_checks=$((stagnant_checks + 1))
  fi

  if [[ "$stagnant_checks" -ge 6 ]]; then
    has_builder="$(ssh -o StrictHostKeyChecking=no "$REMOTE" "bash -lc 'pgrep -f \"electron-builder --mac --universal|npm run build:mac|$REMOTE_RUN\" >/dev/null && echo yes || echo no'")"
    if [[ "$has_builder" == "no" ]]; then
      echo "No active build process detected and no completion marker was written."
      ssh -o StrictHostKeyChecking=no "$REMOTE" "bash -lc 'tail -n 120 \"$REMOTE_LOG\" 2>/dev/null || true'"
      exit 1
    fi
  fi

  sleep 5
done

build_status="$(ssh -o StrictHostKeyChecking=no "$REMOTE" "bash -lc 'cat \"$REMOTE_STATUS\" 2>/dev/null || echo 1'")"

if [[ "$build_status" != "0" ]]; then
  echo "Signed build failed (status $build_status)."
  ssh -o StrictHostKeyChecking=no "$REMOTE" "bash -lc 'tail -n 120 \"$REMOTE_LOG\" 2>/dev/null || true'"
  exit "$build_status"
fi

echo "Signed build complete."
ssh -o StrictHostKeyChecking=no "$REMOTE" "bash -lc 'cd \"$REMOTE_AC_DIR/dist\" && ls -lh \"Aesthetic Computer-$VERSION-universal.dmg\" \"Aesthetic Computer-$VERSION-universal.zip\" latest-mac.yml 2>/dev/null'"

if [[ "$SKIP_PUBLISH" -eq 1 ]]; then
  echo "Skipping publish (--skip-publish)."
  exit 0
fi

publish_cmd=(node scripts/publish-release.mjs)
if [[ -n "$RELEASE_NOTES" ]]; then
  publish_cmd+=(--notes "$RELEASE_NOTES")
fi
if [[ "$DRY_RUN_PUBLISH" -eq 1 ]]; then
  publish_cmd+=(--dry-run)
fi
publish_quoted="$(printf '%q ' "${publish_cmd[@]}")"

echo "Publishing release to Spaces + silo..."
ssh -o StrictHostKeyChecking=no "$REMOTE" "bash -lc 'cd \"$REMOTE_AC_DIR\" && $publish_quoted'"

echo "Publish flow complete."
