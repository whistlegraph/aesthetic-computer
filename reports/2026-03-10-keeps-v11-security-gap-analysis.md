# Keeps v11 Security Gap Analysis (for v12 Patch Planning)

Date: 2026-03-10  
Scope: v11 contract + mint tooling trust boundaries  
Production contract: `KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB`

## Executive Summary

v11 is operational and materially safer than earlier versions (no admin mint path, owner-only burn, transfer unaffected by pause). The largest remaining risks are concentrated in signer/admin centralization and partial permit binding. These are patchable in v12 without forcing holder migration.

## Findings (ranked)

### 1) Permit does not bind full mint economics/metadata (High)

Why it matters:

1. The signed payload binds `contract + owner + content_hash + deadline`, but does not bind `royalties`, `creators`, or `metadata_uri`.
2. A user with a valid permit can mutate unbound fields before submitting `keep`.

Evidence:

1. Permit payload fields in v11: [kidlisp_keeps_fa2_v11.py](/workspaces/aesthetic-computer/tezos/kidlisp_keeps_fa2_v11.py:133)
2. User-supplied `royalties` accepted in keep params: [kidlisp_keeps_fa2_v11.py](/workspaces/aesthetic-computer/tezos/kidlisp_keeps_fa2_v11.py:122)
3. User-supplied `royalties` written directly to token metadata: [kidlisp_keeps_fa2_v11.py](/workspaces/aesthetic-computer/tezos/kidlisp_keeps_fa2_v11.py:168)

v12 patch:

1. Bind a `payload_hash` (or full fields) in permit verification.
2. Include at least `royalties_hash`, `creators_hash`, `metadata_uri_hash` in signed payload.
3. Reject keeps where on-chain params do not match signed hashes.

### 2) Permit signer key is hardcoded and non-rotatable (High)

Why it matters:

1. A compromised signer key cannot be rotated in v11.
2. Main mitigation is operational pause/migration, not in-contract rotation.

Evidence:

1. Hardcoded signer key constant: [kidlisp_keeps_fa2_v11.py](/workspaces/aesthetic-computer/tezos/kidlisp_keeps_fa2_v11.py:37)

v12 patch:

1. Replace hardcoded key with `permit_signers` allowlist.
2. Add explicit signer rotate/revoke entrypoint with audit events.

### 3) Single admin authority controls many sensitive paths (High)

Why it matters:

1. One key can change fee policy, royalty split policy, pause state, treasury routing metadata, admin, and fee withdrawal behavior.
2. This is the largest trust concentration after signer risk.

Evidence:

1. Admin controls metadata, fee, treasury, pause, royalty split, withdrawals: [kidlisp_keeps_fa2_v11.py](/workspaces/aesthetic-computer/tezos/kidlisp_keeps_fa2_v11.py:335)
2. `set_administrator` exists in interface (inherited admin primitive): [step_002_cont_0_types.py](/workspaces/aesthetic-computer/tezos/KeepsFA2v11/step_002_cont_0_types.py:4)

v12 patch:

1. Split roles: `admin`, `permit_manager`, `treasury_manager`.
2. Add two-step role transfer (`propose`/`accept`) and optional delay for critical role changes.
3. Put each role behind multisig.

### 4) `withdraw_fees` ignores stored treasury and allows arbitrary destination (Medium-High)

Why it matters:

1. `set_treasury` updates storage, but fee withdrawal is still any destination chosen at call time.
2. Treasury intent and actual fee routing can diverge.

Evidence:

1. Arbitrary destination withdrawal: [kidlisp_keeps_fa2_v11.py](/workspaces/aesthetic-computer/tezos/kidlisp_keeps_fa2_v11.py:352)
2. Treasury state is independent: [kidlisp_keeps_fa2_v11.py](/workspaces/aesthetic-computer/tezos/kidlisp_keeps_fa2_v11.py:369)

v12 patch:

1. Replace `withdraw_fees(destination)` with `withdraw_fees()` to stored treasury only.
2. Make treasury changes delayed/two-step.

### 5) Creator refresh authority persists after secondary sale unless owner locks (Medium)

Why it matters:

1. Original creator can continue updating URI/presentation fields after transfer.
2. This is intentional in v11, but may exceed collector expectations unless clearly surfaced.

Evidence:

1. Creator is authorized alongside owner: [kidlisp_keeps_fa2_v11.py](/workspaces/aesthetic-computer/tezos/kidlisp_keeps_fa2_v11.py:234)
2. Creator mutable field set includes URI/presentation keys: [kidlisp_keeps_fa2_v11.py](/workspaces/aesthetic-computer/tezos/kidlisp_keeps_fa2_v11.py:254)

v12 patch:

1. Add owner-controlled switch: `disable_creator_refresh(token_id)`.
2. Optional policy: auto-disable creator refresh on first transfer.

### 6) Contract metadata currently unlocked on mainnet (Medium)

Why it matters:

1. Unlocked metadata allows banner/description changes (useful now), but also increases admin-key phishing surface.

Evidence:

1. Mainnet snapshot shows unlocked metadata: [2026-03-10-keeps-audit-mainnet.md](/workspaces/aesthetic-computer/reports/2026-03-10-keeps-audit-mainnet.md:13)
2. Audit warning emitted for this state: [2026-03-10-keeps-audit-mainnet.md](/workspaces/aesthetic-computer/reports/2026-03-10-keeps-audit-mainnet.md:19)

v12 patch:

1. Keep explicit lifecycle metadata states (`active`, `deprecated`, `archival`) and lock policy controls.

### 7) `keep-confirm` trusts client-provided `tokenId` when present (Tooling / Medium)

Why it matters:

1. DB record integrity can be spoofed by authenticated piece owner if `tokenId` is supplied.
2. Function does not require chain-derived token ID when request includes one.

Evidence:

1. Token ID from request is accepted first: [keep-confirm.mjs](/workspaces/aesthetic-computer/system/netlify/functions/keep-confirm.mjs:158)
2. Chain resolution only happens when request tokenId is null: [keep-confirm.mjs](/workspaces/aesthetic-computer/system/netlify/functions/keep-confirm.mjs:160)

v12/tooling patch:

1. Always resolve tokenId from chain (`content_hashes`) regardless of request tokenId.
2. Verify tx target/entrypoint/sender before persisting.

### 8) Contract resolver has legacy fallback path on secret/DB failure (Tooling / Medium)

Why it matters:

1. On secret fetch failure, write paths may fall back to legacy contract address unless additional guards block it.
2. This increases misrouting risk during outages/misconfig.

Evidence:

1. Fallback behavior: [tezos-keeps-contract.mjs](/workspaces/aesthetic-computer/system/backend/tezos-keeps-contract.mjs:79)
2. Catch-path fallback on resolution error: [tezos-keeps-contract.mjs](/workspaces/aesthetic-computer/system/backend/tezos-keeps-contract.mjs:86)

v12/tooling patch:

1. Fail-closed for production write paths (no legacy fallback for mint/update/confirm).
2. Keep fallback only for read-only diagnostics if needed.

## Trust Minimization Ladder (Eliminate Trust in “You” Over Time)

### Level 1: Key Separation

1. Distinct keys for `admin`, `permit_manager`, `treasury_manager`.
2. No shared private key material across roles.

### Level 2: Multisig Governance

1. Put each privileged role behind multisig.
2. Enforce quorum for signer rotation, treasury changes, and pause.

### Level 3: Time-Delayed Critical Actions

1. Delay role transfers and treasury destination changes.
2. Emit pending/activation events for transparent monitoring.

### Level 4: Holder-Visible Safety

1. On-chain or indexer-readable policy state (signer set hash, role addresses, change ETA).
2. Public status endpoint + alert feed for role/signer changes.

### Level 5: Migration Integrity

1. Optional trust-minimized `claim` path from v11 using ownership proof.
2. Replay-proof claim registry by `(old_contract, old_token_id)`.

## v12 Priority Patch Set

1. Rotatable signer registry + signer revoke path.
2. Permit payload binding for economic metadata (`royalties/creators/metadata_uri`).
3. Role split + multisig + delayed critical changes.
4. Treasury-safe withdrawal semantics (`withdraw_fees()` to stored treasury only).
5. Owner opt-out from creator refresh authority after sale.
6. Tooling hardening (`keep-confirm` verification and fail-closed contract resolution).

## Notes

1. This analysis focuses on security/trust boundaries, not UI concerns.
2. v11 remains serviceable while v12 is designed, provided monitoring and incident protocol remain active.

