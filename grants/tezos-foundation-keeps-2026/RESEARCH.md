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

## SUBMITTED 2026-05-22

**Management page:** <https://grants.tezos.foundation/proposals/detail/a2c89b4d-a1ed-42cc-b2df-0e3e59954a7f/>

The form's actual vertical taxonomy turned out NOT to be the 7 marketing-site
verticals — the platform routes by working-group UID and exposes a different
category list (Collectibles · Communication apps · Creator tokenization ·
Crowdfunding · DeFi · Gaming · Payment Solutions · **Smart Contract Templates**).
We landed in **Smart Contract Templates** because Milestone 4 is literally
"release v12 as a reusable contract template," which gave us a clean spine.

### Final scope (as submitted)

- **Applicant entity:** Aesthetic Inc (US C-corp, EIN 33-1483385).
- **Ask:** USD $45,000, milestone-based, 5 milestones over ~14 weeks.
- **Budget split:** ~47% contract work (M1–M3, $21k) · ~53% adoption / media /
  events (M4–M5, $24k) — deliberately weighted toward the artifacts that
  actually get a smart-contract template adopted, including the custom keeps
  wallet, tutorial videos, NELA Computer Club + LA workshop, and an outreach
  campaign into the existing fxhash/objkt creative-coding community.
- **Self-audited:** no external security review in this ask; left as a
  separable follow-on if Foundation reviewers prefer one.
- **Submitted PDF:** `proposal.pdf` (4 pages, AC paper style — xelatex + YWFT
  Processing + Berkeley Mono + AC palette). Source: `proposal.tex` + `build.fish`.

### What happens next

Tezos Foundation grants run on a rolling submission, quarterly review cycle:
proposals submitted in a given quarter are reviewed the month following
quarter-end. Submission landed in Q2 (Apr–Jun 2026) → reviewed July 2026.
Three-stage process: ~1 week initial vetting → ~3 weeks Technical Advisory
Committee + Executive Committee review → ~4+ weeks post-award legal /
paperwork. Earliest realistic first-milestone payment: late summer 2026.

### Open commitment carried into the work

Milestone 4 commits to releasing the keeps FA2 contract + audit protocol as
an openly licensed preservation template. That single commitment is what
makes this ecosystem infrastructure rather than "fund my project."

## Sources

- [Grants — Priorities & Process](https://tezos.foundation/grants/)
- [Ecosystem Grants Program](https://tezos.foundation/ecosystem-grants-program/)
- [Tezos Foundation Grants platform](https://grants.tezos.foundation/)
