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

### A1.5. Deferred v11 Upgrade Banner (approved text, not executed)

Decision on 2026-03-10:

1. Keep this as a planned action only for now.
2. Do not lock collection metadata yet.
3. Do not send any on-chain operation yet.

Planned command (exact text approved):

```bash
node tezos/keeps.mjs set-collection-media mainnet --wallet=keeps --name="KidLisp -1" --description="Keeps on this contract are one version behind. To upgrade your keep, visit https://keep.kidlisp.com and connect."
```

Explicitly deferred until manual go-ahead:

```bash
node tezos/keeps.mjs lock-collection mainnet --wallet=keeps
```

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

## Track B.5: v12 Security Cut (Start Now)

### B5.1. Security objectives

1. Eliminate fixed signer risk by making permit signers rotatable on-chain.
2. Separate powers: admin, permit manager, and treasury controls.
3. Preserve v11 creator guarantees (owner-only burn, immutable content hash + royalties).
4. Keep migration optional, replay-safe, and transparent.

### B5.2. Proposed contract deltas from v11

1. Replace single hardcoded permit signer key with signer registry:
   - `permit_signers : big_map<key, bool>`
   - `set_permit_signer(key, enabled)` entrypoint (admin-governed)
2. Introduce explicit role governance:
   - admin can change policy
   - permit manager role can rotate signer keys
   - treasury role can withdraw fees
3. Add governance safety rails:
   - optional delayed role change (`propose_role_change` + `accept_role_change`)
   - explicit event-style metadata fields for rotation history
4. Maintain transfer behavior parity:
   - pause still blocks keep/edit only, never FA2 transfer
5. Keep metadata compatibility:
   - preserve token_info keys used by objkt/TZIP-21 indexers

### B5.3. Implementation kickoff checklist

1. Write v12 threat model doc (`tezos/`):
   - key compromise scenarios
   - signer rotation failure modes
   - migration replay/impersonation risks
   - initial draft: `tezos/V12-SECURITY-THREAT-MODEL.md`
2. Baseline v11 gap analysis before coding:
   - `reports/2026-03-10-keeps-v11-security-gap-analysis.md`
3. Draft `kidlisp_keeps_fa2_v12.py` from v11 baseline.
4. Add SmartPy tests for:
   - signer rotation before/after keep
   - unauthorized signer updates
   - role separation permissions
   - migration replay rejection
5. Add migration proof tests:
   - claim succeeds only for current v11 owner
   - one-time claim per `(old_contract, old_token_id)`
6. Run testnet dry run before any mainnet v12 deployment.

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

## v12 Addendum: Trustless Upgradeability + Deprecatability

Date: 2026-03-10

This addendum reflects the current v12 draft implementation (`tezos/kidlisp_keeps_fa2_v12.py`):

1. Contract-level metadata changes are holder-governed, not admin-key controlled.
2. New proposal lifecycle:
   - `propose_contract_upgrade(metadata_updates, metadata_updates_hash, successor, deprecate)`
   - `vote_contract_upgrade(proposal_id, token_ids, support)`
   - `execute_contract_upgrade(proposal_id)`
3. Voting model:
   - one vote per live `token_id` per proposal
   - denominator = `active_token_count`
   - quorum gate: `cast_votes / active_token_count` vs `governance_quorum_bps`
   - approval gate: `yes_votes / cast_votes` vs `governance_approval_bps`
4. Deprecation behavior:
   - deprecation is irreversible once an approved proposal executes
   - `contract_state` flips `ACTIVE -> DEPRECATED`
   - `deprecated_successor` and `deprecated_at` are stored on-chain
   - `keep`/`register_keep_commitment` are blocked after deprecation
   - transfer/edit/burn semantics remain available
5. Existing creator guarantees remain:
   - owner-only `burn_keep`
   - immutable `content_hash` + `royalties` after metadata edits
   - owner metadata lock unchanged
6. Migration claim path (v11 -> v12):
   - new `claim_from_v11(old_token_id, token_info, creator_pubkey)`
   - verifies current ownership on source contract via `get_balance_of` view
   - enforces one-time claim per old token id (`migration_claims`)
   - writes provenance tags in token metadata:
     `upgraded_from_contract`, `upgraded_from_token_id`, `migration_kind`
7. Remaining migration hardening:
   - current draft trusts claimant-supplied `token_info` payload
   - if required, add merkle-proof metadata attestation for fully trustless content parity
