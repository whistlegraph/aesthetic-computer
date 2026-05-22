# Tezos Foundation Ecosystem Grant — Application Draft

> **Program:** Tezos Foundation Ecosystem Grants Program
> **Vertical:** Infrastructure & Tooling
> **Apply:** https://tezos.foundation/grants/ (register, pick vertical, submit)
> **Deadline:** rolling — submit Q2 2026 for July review
>
> *This is a v0 spine. Trim and re-shape once the grants platform shows the
> exact form fields and word caps. Drafts-only: @jeffrey submits.*

---

## Project Title

**KidLisp Keeps v12 — a preservation contract for executable generative art**

(Alternate: *"Keeping runnable art on-chain: an FA2 preservation standard."*)

---

## One-paragraph summary (~120 words)

KidLisp Keeps is a live Tezos FA2 collection that mints *executable* generative
art — runnable KidLisp programs, not static images — as permanent on-chain
tokens. The production contract (`KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB`, v11)
has been on mainnet since March 2026. This grant funds **v12**: a deliberate
upgrade that replaces the contract's single fixed permit signer with a
rotatable signer registry, separates admin/permit/treasury roles, adds
holder-governed contract metadata, and ships an opt-in, replay-safe v11→v12
migration path. The contract, its audit protocol, and a 25-year stability plan
are then published as a **reusable preservation template** any Tezos creative
project can adopt — turning one collection's hardening work into shared
ecosystem infrastructure.

---

## Purpose / problem

Most NFT art on any chain is a static image plus a metadata pointer. KidLisp
Keeps mints the *program* — the token carries source that re-runs. That makes
preservation a harder, more interesting problem: the contract has to stay
trustworthy and upgradeable across decades without ever forcing holders to
move or trusting a single key.

v11 works but has documented structural gaps (see
`reports/2026-03-10-keeps-v11-security-gap-analysis.md`):

- the permit signer key is **hardcoded and non-rotatable** — a single point of
  failure with no recovery path;
- **one admin role** controls minting policy, fee withdrawal, and metadata;
- `withdraw_fees` accepts an arbitrary destination, not the stored treasury;
- contract-level metadata is admin-controlled, not holder-governed.

v12 fixes all of these. The work is already specced — this grant funds its
completion, audit, and open release.

---

## Roadmap / milestones

*(Milestone payment amounts in XTZ-equivalent — adjust to Foundation norms.)*

**Milestone 1 — Spec + threat model + invariant tests.**
Freeze the v12 requirement doc (must-have vs nice-to-have). Write
`tezos/V12-SECURITY-THREAT-MODEL.md` (key compromise, signer-rotation failure
modes, migration replay/impersonation risks). Author SmartPy invariant tests:
signer rotation before/after `keep`, unauthorized signer updates, role-
separation permissions, migration-replay rejection.
*Deliverable:* threat model doc + passing test suite. *~3 weeks.*

**Milestone 2 — v12 contract on testnet.**
Finalize `kidlisp_keeps_fa2_v12.py`: rotatable `permit_signers` big_map +
admin-governed `set_permit_signer`; explicit admin / permit-manager / treasury
roles; optional delayed role change (`propose_role_change` / `accept_role_change`);
holder-governed contract-upgrade proposal lifecycle (`propose_contract_upgrade`
/ `vote_contract_upgrade` / `execute_contract_upgrade`). Deploy to Ghostnet.
Build + test the `claim_from_v11` migration path (one-time claim per source
token, provenance tags). Adversarial test pass.
*Deliverable:* audited v12 contract live on Ghostnet + migration prototype.
*~4 weeks.*

**Milestone 3 — Mainnet deployment + opt-in migration.**
Deploy v12 to mainnet for new mints. Ship the opt-in v11→v12 holder migration
in the keep UI (both collections shown, explicitly labeled, no forced burn).
v11 stays live for provenance and trading history.
*Deliverable:* v12 on mainnet, migration claim path live end-to-end. *~3 weeks.*

**Milestone 4 — Open preservation template + audit protocol.**
Publish the v12 FA2 contract, the repeatable audit script
(`keeps-audit.mjs`), and the 25-year stability/ops plan as an openly licensed
**preservation template** with documentation, so any Tezos creative project
can mint executable or long-lived art on a hardened, auditable contract.
*Deliverable:* public repo + docs + a short write-up for the Tezos community.
*~2 weeks.*

---

## Anticipated value to the Tezos ecosystem

- A **hardened, openly licensed FA2 preservation contract** other Tezos art
  and culture projects can fork — most reach for ad-hoc contracts or marketplace
  defaults; this gives them an audited, governance-aware baseline.
- A **repeatable on-chain audit protocol** (`keeps-audit.mjs` + cadence) usable
  by any FA2 collection.
- A worked, public example of **holder-governed contract metadata** and a
  **replay-safe migration path** — patterns the wider ecosystem can study.
- Keeps onboarding: KidLisp Keeps brings creative-coding artists and their
  audiences onto Tezos through a low-friction Beacon mint flow.

---

## Longevity beyond the grant

This is the section the Foundation explicitly asks for — and we already wrote
it. `reports/2026-03-10-keeps-v12-and-25-year-stability-plan.md` documents:

- a **25-year operations cadence** — daily drift check, weekly audit report,
  monthly key-custody review, quarterly pause/unpause recovery drill;
- a **documented incident protocol** (pause → disable mint → public status →
  investigate → resume only after root cause);
- an **optional, never-forced** holder upgrade path, so provenance survives
  every future version;
- non-negotiables: no keys in logs, no forced migration, no emergency action
  without a written postmortem.

The keep contract is designed to outlive the grant by decades; the grant just
funds getting it onto v12's footing.

---

## Team

**Jeffrey Alan Scudder** — artist, educator, technologist (Los Angeles).
Creator of Aesthetic Computer, KidLisp, Whistlegraph, and No Paint. Yale MFA
(2013), Ringling BFA (2011). Author in Residence at UCLA Social Software with
Casey Reas and Lauren Lee McCarthy. Work in the collections of KADIST (San
Francisco) and SMK (Copenhagen). Designed, deployed, and operates the KidLisp
Keeps FA2 contracts (v2 → v11) on Tezos, including the mint pipeline, Beacon
wallet integration, and the repeatable audit tooling.

Applicant entity: **Aesthetic Inc** (C-corp, EIN 33-1483385) — the grant is
held by the company, consistent with Aesthetic Computer's other contracted
work. Jeffrey is the sole developer on the grant scope.

---

## Evidence of Tezos ecosystem familiarity

- Live mainnet FA2 contract `KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB` (v11),
  TZIP-012/016/021 compliant, 88 token definitions, 309 transactions.
- Production contract history v2 → v11, including a prior migration.
- Beacon wallet integration (Temple / Kukai / Ledger), objkt-compatible
  metadata, SmartPy contract authorship, TzKT-based tooling.
- A standing audit script and a documented multi-year operations plan.

---

## Ask / budget

**Total: ~$30,000 USD-equivalent, milestone-based.** A single-developer
infrastructure grant covering ~3 months (~12 weeks) of focused contract and
tooling work, plus Ghostnet/mainnet gas. The Foundation pays per completed,
reported milestone; the split below tracks the roadmap above:

| Milestone | Work | Weeks | Payment |
|---|---|---|---|
| M1 | v12 spec + threat model + SmartPy invariant tests | ~3 | $8,000 |
| M2 | v12 contract on Ghostnet + migration prototype + adversarial pass | ~4 | $11,000 |
| M3 | v12 mainnet deployment + opt-in v11→v12 holder migration | ~3 | $7,000 |
| M4 | Open preservation template + audit protocol + community write-up | ~2 | $4,000 |
| | **Total** | **~12** | **$30,000** |

Self-audited: v12 is hardened with the existing repeatable audit script
(`tezos/keeps-audit.mjs`), a written threat model (M1), and an adversarial
test pass (M2). A funded third-party security review is **not** included in
this ask — if the Foundation's reviewers would prefer one, it can be added as
a separable follow-on line item rather than holding up this scope.

---

## Submission checklist

- [x] Decide applicant entity — **Aesthetic Inc** (C-corp, EIN 33-1483385)
- [x] Set the milestone budget figures — **~$30k, self-audited** (4 milestones)
- [ ] Register on the Tezos Foundation grants platform; confirm the
      Infrastructure & Tooling form fields + word caps
- [ ] Confirm willingness to open-source the preservation template (Milestone 4)
- [ ] Trim each section to the form's actual limits
- [ ] Link the three keeps reports + contract source as supporting docs
- [ ] Submit (Q2 2026 → July review)
