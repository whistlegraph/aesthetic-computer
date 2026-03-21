# KidLisp Keeps v12 Security Threat Model (Draft)

Date: 2026-03-10  
Status: Draft v0.1 (planning)  
Baseline contract: v11 (`KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB`)

## 1) Purpose

Define a security-first design target for v12 before implementation.

## 2) Scope

In scope:

1. Keep permit verification model.
2. Role separation and key rotation.
3. Fee withdrawal authorization.
4. Optional v11 -> v12 holder migration path.
5. Emergency controls and auditability.

Out of scope:

1. Frontend styling and marketplace UI behavior.
2. Third-party marketplace contract risk.
3. Wallet UX trust model outside contract boundaries.

## 3) Security Objectives

1. Remove fixed, non-rotatable signer risk from v11.
2. Minimize blast radius of any single key compromise.
3. Preserve creator/owner guarantees from v11.
4. Keep migration optional and replay-safe.
5. Keep behavior legible to holders and indexers.

## 4) Roles (Target)

1. `admin`: protocol policy and emergency controls.
2. `permit_manager`: rotate keep permit signers.
3. `treasury_manager`: withdraw fees to treasury destination.
4. `owner`: token owner rights (transfer, burn_keep, owner metadata edits).
5. `creator`: refresh-only metadata path (as in v11).

Optional hardening:

1. Use multisig for `admin`, `permit_manager`, and `treasury_manager`.

## 5) Assets and Trust Boundaries

Critical assets:

1. Mint authorization path (permit validation).
2. Token ownership ledger.
3. Immutable token fields (`content_hash`, `royalties`).
4. Fee balance and treasury routing.
5. Migration provenance mapping.

Trust boundaries:

1. Off-chain signer infrastructure vs on-chain permit validation.
2. Admin operations vs holder operations.
3. v11 contract state vs v12 migration claims.

## 6) Threats and Required Mitigations

### T1: Permit signer compromise

Risk:

1. Unauthorized permits could mint unexpected keeps.

Mitigations:

1. Store signer allowlist on-chain (`permit_signers` big_map).
2. Support immediate signer revoke/replace.
3. Bind permit payload to:
   - `contract`
   - `owner`
   - `content_hash`
   - `permit_deadline`
4. Keep short permit TTL and enforce expiry.

### T2: Admin compromise

Risk:

1. Malicious role changes, pause misuse, or metadata tampering.

Mitigations:

1. Split admin powers across dedicated roles.
2. Optional delayed role transfer (`propose` + `accept`).
3. Explicit pause scope: no transfer blocking.
4. Keep contract metadata lock behavior explicit.

### T3: Treasury key compromise

Risk:

1. Fee drainage beyond expected governance control.

Mitigations:

1. Restrict `withdraw_fees` to `treasury_manager`.
2. Support treasury destination updates behind admin policy.
3. Recommend multisig treasury manager address.

### T4: Migration replay / false claim

Risk:

1. Same v11 token claimed multiple times in v12.
2. Non-owner claiming v11 provenance.

Mitigations:

1. Record `claimed[(old_contract, old_token_id)] = true`.
2. Require proof of current v11 ownership at claim time.
3. Mint v12 token only to verified owner.
4. Store immutable provenance fields:
   - `upgraded_from_contract`
   - `upgraded_from_token_id`

### T5: Metadata integrity regression

Risk:

1. v12 accidentally allows mutation of immutable fields.

Mitigations:

1. Preserve v11 invariant: always re-inject original `content_hash` and `royalties`.
2. Preserve lock semantics for token metadata.
3. Keep owner-only `burn_keep`.

## 7) Non-Negotiable Invariants (Must Hold)

1. Transfers remain live even when paused.
2. `burn_keep` callable only by current owner.
3. `content_hash` and `royalties` are immutable after keep.
4. Keep permit must validate against active signer allowlist.
5. Signer rotation cannot mint or transfer tokens directly.
6. Migration claim is one-time per v11 token provenance tuple.
7. No admin transfer path over holder tokens.

## 8) Required Test Matrix

1. Authorization tests:
   - unauthorized signer rotation fails
   - unauthorized fee withdrawal fails
   - unauthorized role updates fail
2. Permit tests:
   - active signer succeeds
   - revoked signer fails
   - expired permit fails
3. Invariant tests:
   - immutable fields stay immutable after edits
   - pause blocks keep/edit but not transfer
4. Migration tests:
   - valid owner claim succeeds once
   - replay claim fails
   - non-owner claim fails
5. Fuzz/adversarial tests:
   - malformed metadata map keys
   - boundary token IDs and map lookups

## 9) Cut Plan (Execution)

1. Draft `kidlisp_keeps_fa2_v12.py` from v11 baseline.
2. Implement role storage and signer registry first.
3. Port v11 invariants without behavior changes.
4. Add migration claim path after core invariants pass.
5. Run SmartPy tests and publish first security review report.

## 10) Open Decisions

1. Role model:
   - single admin with internal ops policy
   - or fully separated on-chain role addresses
2. Governance delay:
   - immediate role changes
   - or delayed/two-step acceptance
3. Migration proof method:
   - direct cross-contract read/view
   - or signed proof flow validated by v12 policy

