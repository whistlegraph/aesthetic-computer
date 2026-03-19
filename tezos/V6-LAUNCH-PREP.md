# Keeps v6 Launch Prep (Archived)

Date: 2026-03-07
Updated: 2026-03-08

This runbook was used for the v6 production cutover.

## Status

- v6 contract is live on mainnet.
- Keeps flow is running against the Mongo-backed active contract config.
- First production keep minted on v6 (token `#0`, `$roz`).
- Next release track moved to v7 (see `tezos/V7-LAUNCH-PLAN.md`).

## 1. Preflight

- Confirm `keeps.tez` wallet has enough XTZ for origination + first mints.
- Confirm compiled v6 artifact exists:
  - `tezos/KeepsFA2v6/step_002_cont_0_contract.tz`

## 2. Compile Artifact

```fish
cd tezos
./compile.fish v6
```

## 3. Deploy v6 Profile

```bash
node tezos/keeps.mjs deploy mainnet --wallet=kidlisp --contract=v6
```

Notes:
- `--contract=v6` applies production metadata (`KidLisp`, `6.0.0`, keep.kidlisp.com).
- Initial storage uses fee `2_500_000` mutez (2.5 XTZ), royalty `1000` bps, paused `false`.
- Mint flow reads `default_royalty_bps` from contract storage and applies it to new token metadata.

## 4. Cutover Files

After deploy, keep the active contract in Mongo `secrets` as source of truth:

- `tezos/contract-address-mainnet.txt`
- Mongo `secrets` document `_id: "tezos-kidlisp"`:
  - preferred: `keepsContract.mainnet = <new KT1>`
  - profile metadata: `currentKeepsProfile`, `currentKeepsVersion`, `currentKeepsNetwork`

Deploy now auto-syncs this when `MONGODB_CONNECTION_STRING` + `MONGODB_NAME` are set.
Manual sync command:

```bash
node tezos/keeps.mjs sync-secrets mainnet --wallet=kidlisp --contract=v6
```

Direct Mongo update example:

```javascript
db.secrets.updateOne(
  { _id: "tezos-kidlisp" },
  {
    $set: {
      "keepsContract.mainnet": "<new KT1>",
      currentKeepsContract: "<new KT1>",
      currentKeepsProfile: "v6",
      currentKeepsVersion: "6.0.0",
      currentKeepsNetwork: "mainnet",
    },
  }
);
```

The following functions resolve the contract from Mongo `secrets` (with a legacy fallback):

- `system/netlify/functions/keep-mint.mjs`
- `system/netlify/functions/keep-update.mjs`
- `system/netlify/functions/keep-update-confirm.mjs`
- `system/netlify/functions/kidlisp-keep.mjs`

## 5. Smoke Tests

Run after deploy + env cutover:

```bash
node tezos/keeps.mjs status mainnet --wallet=kidlisp
node tezos/keeps.mjs fee mainnet --wallet=kidlisp
```

Validate on chain:

- Contract storage shows:
  - `administrator = keeps.tez wallet`
  - `keep_fee = 2500000`
  - `default_royalty_bps = 1000`
  - `paused = false`
- First mint works via kidlisp.com Keep flow and appears on objkt.
