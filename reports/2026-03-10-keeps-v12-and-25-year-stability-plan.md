# Keeps v12 + 25-Year Stability Plan

Date: 2026-03-10  
Current production contract: `KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB` (v11, mainnet)

## Goals

1. Keep v11 safe and operational for as long as needed.
2. Design v12 deliberately (not reactively) with clear migration guarantees.
3. Ensure holders always have a stable, optional upgrade path.
4. Maintain a repeatable audit protocol for the next 25 years.

## Current State (Day 0 Baseline)

1. v11 is active and has live volume/owners.
2. Permit signer model is fixed in-contract (non-rotatable key design).
3. Admin + signer are intentionally centralized today.
4. Mint tooling moat now enforces preflight checks and storage-derived royalties.

## Track A: v11 Long-Horizon Operations

### A1. Runtime Guardrails (active now)

1. Keep fail-closed preflight in mint path.
2. Keep storage-derived royalty and treasury reads.
3. Keep recent-op monitoring in mint path.

### A2. Incident Protocol

If compromise is suspected:

1. Call `pause` immediately.
2. Disable keep-mint endpoint.
3. Post status update publicly with timestamp and scope.
4. Investigate operations since last known-good window.
5. Resume only after root cause is understood and mitigated.

### A3. Cadence

1. Daily: quick on-chain drift check.
2. Weekly: full audit report.
3. Monthly: key custody/process review.
4. Quarterly: recovery drill (pause/unpause runbook dry run).

## Track B: v12 Design Requirements

v12 should include:

1. Rotatable permit signer set (single signer by policy, rotatable by design).
2. Explicit role separation: admin role and permit role.
3. Same creator guarantees (owner-only burn, immutable content hash + royalties).
4. Clear emergency controls with transparent auditability.
5. Backward-compatible metadata/provenance fields for indexers and marketplaces.

## Track C: Holder Upgrade Path (Optional, Not Forced)

1. Keep v11 live for provenance and trading history.
2. Launch v12 for new mints.
3. Offer opt-in `claim` migration from v11 to v12:
   - verify current ownership of v11 token
   - mark `old_token_id` as claimed in v12 to prevent replay
   - mint v12 token to current holder
   - store provenance mapping (`upgraded_from`)
4. Keep no forced burn requirement for holders by default.
5. Show both v11 and v12 collections in UI with explicit labels.

## Decision Gates

### Gate 1: Stay v11-only

Stay on v11 if all are true:

1. No key or protocol incidents.
2. No required feature blocked by fixed signer model.
3. Operational protocol remains healthy.

### Gate 2: Start v12 rollout

Start v12 when any is true:

1. Need signer rotation support on-chain.
2. Need stronger role/governance model than v11 can express.
3. Strategic need for a cleaner long-term trust model.

## Audit Protocol (Repeatable)

Use the new script:

```bash
node tezos/keeps-audit.mjs --network=mainnet --contract=KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB
```

Recommended strict mode inputs:

```bash
node tezos/keeps-audit.mjs \
  --network=mainnet \
  --contract=KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB \
  --expected-admin=tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC \
  --expected-code-hash=1692834636 \
  --expected-type-hash=399679480
```

The script writes a report in `reports/` and exits with non-zero when critical alerts are detected.

## 90-Day Execution Plan

### Days 0-7

1. Run weekly audit reports and review deltas.
2. Lock in key custody SOP and incident contacts.
3. Freeze v12 requirement doc (must-have vs nice-to-have).

### Days 8-30

1. Draft v12 contract spec and threat model.
2. Write invariant tests and migration test vectors.
3. Build claim-path prototype on testnet.

### Days 31-60

1. Complete internal review and adversarial test pass.
2. Dry run holder upgrade UX end-to-end on testnet.
3. Decide go/no-go using decision gates.

### Days 61-90

1. If go: launch v12 for new mints with opt-in migration.
2. If no-go: continue v11 with formalized long-horizon ops cadence.

## Non-Negotiables

1. No private keys in docs, logs, or reports.
2. No forced holder migration.
3. No emergency action without written postmortem timeline.
4. Preserve provenance readability across versions.

