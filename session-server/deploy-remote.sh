#!/usr/bin/env bash
set -uo pipefail

: "${NODE_BIN:?NODE_BIN is required}"
: "${REMOTE:?REMOTE is required}"
: "${REF:?REF is required}"
: "${BOOT_BUDGET:?BOOT_BUDGET is required}"

export PATH="$NODE_BIN:$PATH"
cd "$REMOTE" || { echo 'RESULT=fail:cd'; exit 1; }

PRE=$(git rev-parse HEAD)
echo "rollback point: $(git rev-parse --short HEAD)"

echo "→ fetch + checkout $REF"
git fetch origin --quiet || { echo 'RESULT=fail:fetch'; exit 1; }
git reset --hard "$REF" --quiet || { echo 'RESULT=fail:reset'; exit 1; }
echo "  now at $(git rev-parse --short HEAD) ($(git log -1 --format=%s | head -c 55))"

cd session-server || { echo 'RESULT=fail:cd-ss'; exit 1; }

deploy_and_check() {
  echo "→ npm ci --omit=dev"
  npm ci --omit=dev >/tmp/ss-deploy-npm.log 2>&1 || {
    echo '  npm ci FAILED'
    tail -3 /tmp/ss-deploy-npm.log
    return 1
  }
  echo "→ systemctl restart session-server"
  systemctl restart session-server || return 1
  # Health gate: wait for :8889 to bind (slow boot loads chat history first).
  for i in $(seq 1 "$BOOT_BUDGET"); do
    if ss -ltn 2>/dev/null | grep -q :8889; then
      echo "  ✓ bound :8889 after ${i}s"
      return 0
    fi
    if [ "$(systemctl show session-server -p NRestarts --value)" -gt 3 ]; then
      echo "  ✗ crash-looping (NRestarts>3)"
      return 1
    fi
    sleep 1
  done
  echo "  ✗ did not bind :8889 within ${BOOT_BUDGET}s"
  return 1
}

if deploy_and_check; then
  echo "RESULT=ok:$(git rev-parse --short HEAD)"
  exit 0
fi

echo "→ ⏮  ROLLING BACK to $(git rev-parse --short "$PRE")"
cd "$REMOTE" && git reset --hard "$PRE" --quiet && cd session-server
if deploy_and_check; then
  echo "RESULT=rolledback:$(git rev-parse --short "$PRE")"
else
  echo "RESULT=DOWN:rollback-also-failed"
fi
exit 1
