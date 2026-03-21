#!/usr/bin/env bash
set -euo pipefail

# One-time setup:
# - Export Developer ID identity from host Mac keychain to vault .p12
# - Create vault env file with CSC_LINK / CSC_KEY_PASSWORD
#
# Runs export via host GUI Terminal session because non-interactive SSH cannot
# access private keys in many macOS keychain setups.
#
# Usage:
#   bash scripts/setup-host-mac-signing.sh
#   bash scripts/setup-host-mac-signing.sh --host-key mac-mini

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
MACHINES_FILE="$REPO_ROOT/aesthetic-computer-vault/machines.json"

HOST_KEY="jeffrey-macbook"
IDENTITY_NAME="Developer ID Application: Jeffrey Scudder (FB5948YR3S)"
TIMEOUT_MIN=20

while [[ $# -gt 0 ]]; do
  case "$1" in
    --host-key)
      HOST_KEY="${2:-}"
      shift 2
      ;;
    --identity)
      IDENTITY_NAME="${2:-}"
      shift 2
      ;;
    --timeout-min)
      TIMEOUT_MIN="${2:-20}"
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
REMOTE_VAULT_DIR="$HOST_REPO/aesthetic-computer-vault/ac-electron"
REMOTE_P12="$REMOTE_VAULT_DIR/mac-developer-id.p12"
REMOTE_ENV="$REMOTE_VAULT_DIR/.env"
SETUP_ID="ac-sign-setup-$(date +%s)"
REMOTE_RUN="/tmp/${SETUP_ID}.sh"
REMOTE_LOG="/tmp/${SETUP_ID}.log"
REMOTE_STATUS="/tmp/${SETUP_ID}.status"
REMOTE_DONE="/tmp/${SETUP_ID}.done"

if command -v openssl >/dev/null 2>&1; then
  CERT_PASSWORD="$(openssl rand -base64 36 | tr -d '\n')"
else
  CERT_PASSWORD="$(date +%s)-$(od -An -N8 -tx1 /dev/urandom | tr -d ' \n')"
fi

echo "Host: $HOST_KEY ($REMOTE)"
echo "Identity: $IDENTITY_NAME"
echo "Will write:"
echo "  $REMOTE_P12"
echo "  $REMOTE_ENV"

cat > "/tmp/${SETUP_ID}.remote.sh" <<EOF
#!/usr/bin/env bash
set -uo pipefail
mkdir -p "$REMOTE_VAULT_DIR"
rm -f "$REMOTE_P12"

security export \\
  -k /Library/Keychains/System.keychain \\
  -t identities \\
  -f pkcs12 \\
  -P "$CERT_PASSWORD" \\
  -o "$REMOTE_P12" \\
  >"$REMOTE_LOG" 2>&1
status=\$?

if [[ "\$status" -eq 0 ]]; then
  chmod 600 "$REMOTE_P12"
  cat > "$REMOTE_ENV" <<'ENV_EOF'
CSC_LINK=$REMOTE_P12
CSC_KEY_PASSWORD=$CERT_PASSWORD
ENV_EOF
  chmod 600 "$REMOTE_ENV"
fi

echo "\$status" > "$REMOTE_STATUS"
date -u +"%Y-%m-%dT%H:%M:%SZ" > "$REMOTE_DONE"
exit "\$status"
EOF

scp -q "/tmp/${SETUP_ID}.remote.sh" "$REMOTE:$REMOTE_RUN"
ssh -o StrictHostKeyChecking=no "$REMOTE" "bash -lc 'chmod +x \"$REMOTE_RUN\" && rm -f \"$REMOTE_STATUS\" \"$REMOTE_DONE\" \"$REMOTE_LOG\" && osascript -e '\"'\"'tell application \"Terminal\" to do script \"$REMOTE_RUN\"'\"'\"''"

echo "Started key export in host Terminal. Approve keychain prompts once."

deadline=$(( $(date +%s) + TIMEOUT_MIN * 60 ))
while true; do
  if ssh -o StrictHostKeyChecking=no "$REMOTE" "test -f \"$REMOTE_DONE\" || test -f \"$REMOTE_STATUS\""; then
    break
  fi
  if [[ "$(date +%s)" -ge "$deadline" ]]; then
    echo "Timed out waiting for setup completion after ${TIMEOUT_MIN} minutes"
    ssh -o StrictHostKeyChecking=no "$REMOTE" "bash -lc 'tail -n 120 \"$REMOTE_LOG\" 2>/dev/null || true'"
    exit 1
  fi
  sleep 3
done

setup_status="$(ssh -o StrictHostKeyChecking=no "$REMOTE" "bash -lc 'cat \"$REMOTE_STATUS\" 2>/dev/null || echo 1'")"
if [[ "$setup_status" != "0" ]]; then
  echo "Setup failed (status $setup_status)"
  ssh -o StrictHostKeyChecking=no "$REMOTE" "bash -lc 'tail -n 200 \"$REMOTE_LOG\" 2>/dev/null || true'"
  exit "$setup_status"
fi

ssh -o StrictHostKeyChecking=no "$REMOTE" "bash -lc 'ls -lh \"$REMOTE_P12\" \"$REMOTE_ENV\"'"
echo "Host signing setup complete."

