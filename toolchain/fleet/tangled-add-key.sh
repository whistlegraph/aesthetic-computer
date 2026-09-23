#!/usr/bin/env bash
# tangled-add-key.sh — register this machine's SSH public key with Tangled.
#
# Tangled stores SSH keys as sh.tangled.publicKey records in the account's
# ATProto repo; the appview picks them up from the firehose and the knot
# starts accepting the key within a minute or two. This writes that record
# directly, which is what the tangled.org settings page does under the hood.
#
#   toolchain/fleet/tangled-add-key.sh            # name = this machine's hostname
#   toolchain/fleet/tangled-add-key.sh blueberry  # explicit name
#   KEY=~/.ssh/other.pub toolchain/fleet/tangled-add-key.sh
#
# Credentials come from the unlocked vault (aesthetic-computer-vault/at/.env:
# BSKY_SERVICE, BSKY_IDENTIFIER, BSKY_APP_PASSWORD). Nothing secret is printed.
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ENV_FILE="$REPO/aesthetic-computer-vault/at/.env"
KEY="${KEY:-$HOME/.ssh/id_ed25519.pub}"
NAME="${1:-$(hostname -s)}"
DID="did:plc:k3k3wknzkcnekbnyde4dbatz" # aesthetic.computer — the identity the knot trusts

[ -f "$ENV_FILE" ] || { echo "vault locked or missing: $ENV_FILE" >&2; exit 1; }
[ -f "$KEY" ] || { echo "no public key at $KEY" >&2; exit 1; }

# Plain loop rather than `. <(grep …)`: macOS ships bash 3.2, which drops
# variables sourced from a process substitution.
while IFS='=' read -r k v; do
  case "$k" in
    BSKY_SERVICE|BSKY_IDENTIFIER|BSKY_APP_PASSWORD)
      v="${v%$'\r'}"; v="${v#\"}"; v="${v%\"}"; v="${v#\'}"; v="${v%\'}"
      export "$k=$v" ;;
  esac
done < "$ENV_FILE"
: "${BSKY_SERVICE:?missing in $ENV_FILE}" "${BSKY_IDENTIFIER:?missing}" "${BSKY_APP_PASSWORD:?missing}"
PUB="$(tr -d '\n' < "$KEY")"

# Already registered? (public read)
if curl -sS -m 20 "$BSKY_SERVICE/xrpc/com.atproto.repo.listRecords?repo=$DID&collection=sh.tangled.publicKey&limit=100" \
   | jq -e --arg k "$PUB" '.records[] | select(.value.key == $k)' >/dev/null; then
  echo "key already registered on Tangled (nothing to do)"
  exit 0
fi

SESS="$(curl -sS -m 20 -X POST "$BSKY_SERVICE/xrpc/com.atproto.server.createSession" \
  -H 'content-type: application/json' \
  -d "$(jq -nc --arg i "$BSKY_IDENTIFIER" --arg p "$BSKY_APP_PASSWORD" '{identifier:$i,password:$p}')")"
JWT="$(jq -r '.accessJwt // empty' <<<"$SESS")"
SDID="$(jq -r '.did // empty' <<<"$SESS")"
[ -n "$JWT" ] || { echo "login failed: $(jq -c '{error,message}' <<<"$SESS")" >&2; exit 1; }
[ "$SDID" = "$DID" ] || { echo "logged in as $SDID, expected $DID — wrong account in vault" >&2; exit 1; }

OUT="$(curl -sS -m 20 -X POST "$BSKY_SERVICE/xrpc/com.atproto.repo.createRecord" \
  -H 'content-type: application/json' -H "authorization: Bearer $JWT" \
  -d "$(jq -nc --arg did "$DID" --arg key "$PUB" --arg name "$NAME" --arg now "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
        '{repo:$did,collection:"sh.tangled.publicKey",record:{"$type":"sh.tangled.publicKey",key:$key,name:$name,createdAt:$now}}')")"
URI="$(jq -r '.uri // empty' <<<"$OUT")"
[ -n "$URI" ] || { echo "createRecord failed: $(jq -c '{error,message}' <<<"$OUT")" >&2; exit 1; }
echo "registered '$NAME' → $URI"
echo "the knot usually accepts it within a minute: ssh -T git@knot.aesthetic.computer"
