# Tezos Foundation — Ecosystem Grant (KidLisp Keeps) — Research

> **Program:** Tezos Foundation Ecosystem Grants Program
> **Apply:** https://tezos.foundation/grants/ → "Apply now" (register on the
>   grants platform, pick a vertical, submit a proposal)
> **Contact:** grants@tezos.foundation
> **Deadline:** rolling — submissions accepted any time; reviewed the month
>   after each quarter ends. Submitting now (Q2: Apr–Jun 2026) → review in July.
> **Amount:** not fixed; milestone-structured. See DRAFT for our proposed ask.

## The seven verticals

Core Development · Tech.Rel · **Infrastructure & Tooling** · **Arts & Culture**
· DeFi · Business Development · Gaming.

KidLisp Keeps fits two: it is an arts project *and* a piece of preservation
tooling. **Per the one-spine rule we lead with Infrastructure & Tooling**
("Nodes, block explorers, SDKs, and APIs" — i.e. builder tooling) — because
the deliverable is a concrete, milestone-able contract + tooling upgrade. The
arts dimension is the *context* that makes it matter, not the spine.

## What a proposal must include (their words)

- Functional and technical details of the proposed solution
- Its purpose
- A roadmap
- An introduction of the project team
- **Evidence of familiarity with the Tezos ecosystem**
- Anticipated value added to the Tezos ecosystem
- **Measures to assure the longevity of the solution beyond the grant duration**

## Why we are an unusually strong applicant

1. **It already exists on mainnet.** Contract `KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB`
   (v11), FA2, TZIP-012/016/021 compliant. 88 token definitions, 309 txns,
   live since 2026-03-09. Most applicants pitch vapor; we ship a chain link.
2. **The longevity section is pre-written.** They explicitly require "measures
   to assure longevity beyond the grant duration." We already have
   `reports/2026-03-10-keeps-v12-and-25-year-stability-plan.md` — a documented
   25-year operations + audit cadence. Almost no applicant can produce that.
3. **Evidence of ecosystem familiarity is built-in:** deployed FA2 contract,
   objkt integration, Beacon wallet flow, a repeatable audit script
   (`tezos/keeps-audit.mjs`), and contract versions v2→v11 in production history.
4. **The roadmap already has a spec.** `tezos/kidlisp_keeps_fa2_v12.py` is
   drafted; `reports/2026-03-10-keeps-v11-security-gap-analysis.md` is the gap
   analysis. The grant funds work that is already scoped.

## The review process (3 stages)

1. **Proposal** — ~1 week initial vetting by Foundation staff.
2. **Award** — ~3 weeks in-depth review by the Technical Advisory Committee +
   Executive Committee.
3. **Post-Award** — ~4+ weeks for legal paperwork. Payments are milestone-based:
   complete a milestone → submit a report → reviewed → payment issued.

So realistic cash timing: submit ~late May/June → reviewed July → paperwork
August → first milestone payment late summer / early fall 2026.

## Source materials in repo

- `reports/2026-03-10-keeps-v11-security-gap-analysis.md` — 7 ranked findings
- `reports/2026-03-10-keeps-v12-and-25-year-stability-plan.md` — v12 design +
  25-year ops plan (this *is* the longevity section)
- `reports/2026-03-10-keeps-audit-mainnet.md` — mainnet audit snapshot
- `tezos/kidlisp_keeps_fa2_v11.py` — production contract source
- `tezos/kidlisp_keeps_fa2_v12.py` — v12 draft (rotatable signer, holder
  governance, v11→v12 migration claim path)
- `tezos/keeps-audit.mjs` — repeatable audit script
- `system/public/aesthetic.computer/disks/keep.mjs` — the mint UI (3,798 lines)
- `system/netlify/functions/keep-confirm.mjs`, `keep-update.mjs` — backend

## Decisions made (2026-05-22)

- **Applicant entity:** Aesthetic Inc (C-corp, EIN 33-1483385). Grant held by
  the company, consistent with AC's other contracted work.
- **Ask amount:** ~$30,000 USD-equivalent, milestone-based, self-audited
  (4 milestones over ~12 weeks). Breakdown in DRAFT.

## Still open before submission

- **Register on the grants platform** (`grants.tezos.foundation`) — needs
  @jeffrey to create the account; then capture the real I&T form fields and
  word caps so the draft can be trimmed to fit.
- **Open-sourcing confirmation:** Milestone 4 commits to publishing the keeps
  FA2 contract + audit protocol as a reusable preservation template. That
  commitment is what makes this ecosystem infrastructure rather than "fund my
  project" — confirm @jeffrey is good with releasing it openly licensed.

## Sources

- [Grants — Priorities & Process](https://tezos.foundation/grants/)
- [Ecosystem Grants Program](https://tezos.foundation/ecosystem-grants-program/)
- [Tezos Foundation Grants platform](https://grants.tezos.foundation/)
