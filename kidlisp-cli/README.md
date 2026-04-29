# kidlisp-cli

Home for the public `kidlisp` CLI implementation and distribution specs.

## Specs (planning)

- `INSTALLER-SPEC.md` — Website one-liner installer contract (`curl | sh`, PowerShell).
- `RELEASE-SPEC.md` — Release asset names, checksums, channels, and update model.

## Tools

### `rebake.mjs` — Refresh a piece's oven artifact + thumbnail

Admin-only. Re-runs the oven bake (HTML bundle + WebP thumbnail) for any
KidLisp piece — minted or not — and writes the new IPFS URIs to
`kidlisp.<code>.pendingRebake` (or `ipfsMedia` on first bake). Use this
when an oven thumbnail came back broken (e.g. an all-black 528-byte WebP
from a Puppeteer flake) and you want a fresh pin without walking the
keep-mint UI.

```bash
node kidlisp-cli/rebake.mjs $2un
node kidlisp-cli/rebake.mjs $2un --api https://localhost:8888
```

Auth: requires an admin AC login. The CLI reads `~/.ac-token` and
auto-launches `tezos/ac-login.mjs` if the token is missing or expired.

Backed by `POST /api/admin-rebake` (`system/netlify/functions/admin-rebake.mjs`).
Direct API call:

```bash
curl -X POST "https://aesthetic.computer/api/admin-rebake" \
  -H "Authorization: Bearer $(jq -r .access_token < ~/.ac-token)" \
  -H "Content-Type: application/json" \
  -d '{"piece":"$2un"}'
# → { jobId, status: "preparing", statusUrl: "/api/keep-status?jobId=…" }

curl "https://aesthetic.computer/api/keep-status?jobId=<jobId>"
```

For minted-token regen with on-chain `edit_metadata` (full chain update),
use `regenerate-media.mjs` instead.

### `regenerate-media.mjs` — Rebake + on-chain update for minted Keeps

Looks up minted tokens on TzKT, rebakes media via `/api/keep-prepare`,
then signs and broadcasts `edit_metadata` locally (Taquito). Requires both
an AC login and a Tezos signing key.

```bash
# Single piece (looks up tokenId/minter on TzKT first)
node kidlisp-cli/regenerate-media.mjs $cow

# All tokens on the active keeps contract
node kidlisp-cli/regenerate-media.mjs --all

# Dry run, or rebake-only (no chain tx)
node kidlisp-cli/regenerate-media.mjs $cow --dry-run
node kidlisp-cli/regenerate-media.mjs $cow --skip-update

# Filter --all to one minter
node kidlisp-cli/regenerate-media.mjs --all --creator tz1...
```

Flags: `--dry-run`, `--skip-update`, `--api URL`, `--contract KT1...`,
`--creator tz1...`, `--delay MS`, `--tezos-key <sk...>`. Tezos key sources
checked in order: `--tezos-key`, `$TEZOS_PRIVATE_KEY`, `~/.ac-tezos-key`.

## Status

Spec-first planning directory; the tools above are working today.
