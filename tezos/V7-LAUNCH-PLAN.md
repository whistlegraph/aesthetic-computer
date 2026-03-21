# Keeps v7 Launch Plan

Date: 2026-03-08

Goal: deploy the final KidLisp Keeps contract as **v7** from `keeps.tez`, preserving FA2 compatibility while adding creator-safe refresh rights.

## Contract Delta (v7 vs v6)

- `edit_metadata` policy split:
  - owner: full metadata edit
  - original creator (after transfer): refresh-only update path
- Immutable forever: `content_hash`, `royalties`
- Mint writes both metadata pointers on-chain:
  - `""` (indexer standard pointer)
  - `metadata_uri` (explicit helper key)

## Bill of Rights (Protocol)

- Artist rights:
  - Original creator may refresh presentation metadata after resale (artifact/display/thumbnail/metadata pointer paths)
  - Creator attribution is tracked in `token_creators`
- Collector rights:
  - Core identity and economics are immutable: `content_hash` + `royalties`
  - Owner can lock metadata permanently with `lock_metadata`
- Admin rights:
  - Emergency controls only (pause/unpause, fee/admin settings, contract metadata governance)

## Deployment Sequence

1. Compile and verify artifacts
   - `cd tezos && ./compile.fish v7`
   - Ensure `tezos/KeepsFA2v7/step_002_cont_0_contract.tz` exists
2. Stage validation pass
   - Deploy with staging wallet/network profile
   - Run keep + transfer + creator refresh + owner full edit + lock flow
3. Mainnet deployment
   - `node tezos/keeps.mjs deploy mainnet --wallet=kidlisp --contract=v7`
4. Secret cutover
   - `node tezos/keeps.mjs sync-secrets mainnet --wallet=kidlisp --contract=v7`
   - Confirm `currentKeepsProfile = v7`, `currentKeepsVersion = 7.0.0`
5. Post-deploy smoke
   - Mint, transfer, creator refresh, owner edit, metadata lock, and objkt/TzKT verification

## Success Criteria

- v7 contract live and referenced by Mongo `secrets`
- Keep flow works without Netlify env contract overrides
- Creator refresh succeeds after transfer
- Owner full edit works when owner signs
- Immutable fields unchanged across edits
