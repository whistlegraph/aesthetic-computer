# Proposal step (form step 3) — proposed values

Drafted 2026-05-22 from `DRAFT.md` + repo facts. Review before fill.

---

## Category
**Smart Contract Templates** *(form id `d477a92d-9758-4bec-bf87-6997d451659c`)*
— maps cleanly to Milestone 4 (publishing the v12 FA2 contract as a reusable template).

## Name of the proposal *(≤128)*
> KidLisp Keeps v12 — A Reusable FA2 Preservation Contract for Executable Generative Art on Tezos

## Purpose sentence
> To harden an already-deployed Tezos FA2 contract — KidLisp Keeps — into v12 with rotatable signer keys, holder-governed metadata, and a replay-safe migration path, then release it as a reusable smart-contract template any Tezos creative project can fork to mint executable code with multi-decade longevity guarantees.

## Executive summary
> KidLisp Keeps is a Tezos FA2 collection that mints *executable* generative art — runnable KidLisp programs, not static images — as permanent on-chain tokens. The production contract (KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB, v11) has been on Tezos mainnet since March 2026 and currently holds 88 tokens across 6+ owners. This grant funds v12: an upgrade that replaces the contract's single fixed permit signer with a rotatable signer registry, separates admin / permit / treasury roles, adds holder-governed contract metadata via an on-chain proposal lifecycle, and ships an opt-in, replay-safe v11 → v12 migration claim path. The contract, its repeatable audit script (`keeps-audit.mjs`), and a 25-year stability and operations plan are then released as a reusable Tezos smart-contract template — turning one collection's hardening work into shared ecosystem infrastructure.

## Value proposition to Tezos ecosystem
> 1) A hardened, openly licensed FA2 preservation contract that any Tezos arts-and-culture project can fork — most reach for ad-hoc contracts or marketplace defaults; this provides a governance-aware, audit-ready baseline.
> 2) A repeatable on-chain audit protocol (`keeps-audit.mjs` + cadence) usable by any FA2 collection on mainnet.
> 3) A worked, public example of holder-governed contract metadata and a replay-safe inter-contract migration — patterns the wider ecosystem can study.
> 4) Onboarding for creative-coding artists into Tezos via a low-friction Beacon mint flow that takes the program itself (not just a render) on-chain.

## Market demand
> Generative-art collectors on Tezos already buy executable work — fxhash and objkt host thousands of code-based projects. But existing offerings store the *output* of code or its runtime artifacts; the *source program* lives off-chain in a marketplace's database. As marketplaces come and go (the hic et nunc → its successors lineage is the canonical example), works lose runnability. Keeps demonstrates a different model: the program itself lives on-chain, as the token. Mainnet adoption is already underway with 88 tokens minted across 6+ owners since the v11 launch in March 2026. Demand will grow as more generative artists, collectors, and institutions look for preservation paths independent of any single marketplace's lifecycle — a question the Musée d'Orsay, Serpentine, and others have already raised publicly about Tezos-resident work.

## 3 key KPIs
> 1. v12 contract deployed to Tezos mainnet, with the opt-in v11 → v12 migration claim path live end-to-end (binary completion). Verifiable on TzKT.
> 2. v12-template adoption demonstrated through LA hands-on sessions: 4 NELA Computer Club appointments (biweekly demos at Plot.Place, Chinatown LA) plus 1 public workshop, guiding 5+ generative artists through forking the v12 template and minting their first keeps. Documented as a public onboarding playbook. Stretch target: at least 3 independent Tezos creative-coding projects evaluating or forking the template within 12 months of M4 publication.
> 3. Repeatable audit cadence operational — weekly machine-generated audit reports (via `keeps-audit.mjs`) published for 26 consecutive weeks post-v12 deployment, demonstrating that the 25-year operations plan is real, not aspirational.

## Current follower base on social media
> @whistlegraph (the artist's prior project, on TikTok) reached ~2.6 million followers during 2019–2023 and its archive still moves ~48,000 video views per day in 2026 (~17.7M views in the 2025-04 → 2026-04 window). Aesthetic Computer (aesthetic.computer) currently has 2,800+ registered users, 600+ published pieces, and tens of thousands of boot sessions. The KidLisp Keeps collection itself has 6+ on-chain owners across 88 tokens on Tezos mainnet. Handles: @promptDOTac (Twitter / X), @whistlegraph (Instagram and TikTok).

## Alternatives
> On Tezos, generative-art minting is dominated by fxhash (parameterized JavaScript art stored as fxparams plus a renderer iframe), objkt (a marketplace; most contracts store IPFS pointers to static outputs), and various per-project ad-hoc FA2 contracts. On Ethereum, Art Blocks plays a similar role (parameterized Solidity-tracked code, but gas-expensive and renderer-mediated). For preservation of the *runnable source program* — not just its outputs or parameters — there is no widely adopted standard on any chain; most artists rely on personal backups, GitHub repos, or marketplace stewardship.

## Better than alternatives
> Keeps stores the program itself, on-chain, as the token. Re-running requires only a KidLisp interpreter — open source, runs in any browser, with several language ports already in existence (WASM, Game Boy, Nintendo 64, Playdate) — and does not require the keep mint UI, a specific marketplace, or any particular renderer to keep working. v12 adds holder-governed contract metadata so the collection's identity is never under unilateral admin control, and ships with a published 25-year operations plan. The combination — runnable program on-chain + governance independence + documented multi-decade ops — is unusual on any chain and currently absent on Tezos.

## Technical design
> v12 is an FA2 (TZIP-012) smart contract authored in SmartPy, with TZIP-016 (contract metadata) and TZIP-021 (token metadata) compliance, derived from the production v11 contract (`kidlisp_keeps_fa2_v11.py`).
>
> Key changes from v11:
> - Replace the single hardcoded permit signer key with a signer registry: `permit_signers : big_map<key, bool>` + an admin-governed `set_permit_signer(key, enabled)` entrypoint.
> - Explicit role separation: admin (policy), permit manager (signer rotation), treasury (fee withdrawal). Optional delayed role change via `propose_role_change` + `accept_role_change`.
> - Holder-governed contract metadata via an on-chain proposal lifecycle: `propose_contract_upgrade` → `vote_contract_upgrade` → `execute_contract_upgrade`. One vote per live `token_id`; quorum and approval gates configurable.
> - Opt-in v11 → v12 migration claim: `claim_from_v11(old_token_id, token_info, creator_pubkey)`, one-time per source token, with provenance tags (`upgraded_from_contract`, `upgraded_from_token_id`, `migration_kind`), verifying current ownership of the source token via the `get_balance_of` view.
> - Preserved from v11: owner-only `burn_keep`, immutable `content_hash` + royalties after metadata edits, owner metadata lock.
>
> Surrounding tooling: Beacon SDK wallet integration (Temple, Kukai, Ledger) in the mint UI; a repeatable audit script (`keeps-audit.mjs`) that reads TzKT and verifies admin, code hash, and type hash against an expected baseline; SmartPy invariant tests for signer rotation, role-separation permissions, and migration-replay rejection.

## Why is Tezos relevant
> Tezos hosts the largest concentration of generative-art creators and collectors per chain — fxhash, objkt, the hic et nunc lineage, the Musée d'Orsay and Serpentine programs. Low gas costs make minting accessible at scale (a 2.5 XTZ keep fee is sustainable for individual artists; the same operation on Ethereum mainnet would be prohibitive). Proof-of-stake aligns with the energy concerns of the creative-coding community. Mature FA2 + TZIP standards give the contract a surface that objkt and other indexers already understand. Keeps lives where its audience and its peer projects already live; no other chain offers the same combination of cost, ecosystem maturity, and art-community fit.

## Grant Duration
**3** (months)

## Team Size
**1**

## Proposal Team
> Sole developer and applicant: Jeffrey Alan Scudder.
> Applicant entity: Aesthetic Inc (US C-corporation, EIN 33-1483385).
> Aesthetic Inc is the company behind Aesthetic Computer (aesthetic.computer), the creative-coding platform inside which KidLisp and the Keeps collection are designed, built, and operated.

## Proposal Team Experience
> Jeffrey Alan Scudder is an artist, educator, and technologist based in Los Angeles. He is the sole designer and developer of:
> - KidLisp Keeps (Tezos FA2 collection): v2 → v11 contract series in SmartPy, the mint pipeline, Beacon wallet integration, IPFS upload with redundancy, the repeatable audit script (`keeps-audit.mjs`), and the published security gap analysis + 25-year stability plan.
> - Aesthetic Computer (aesthetic.computer): a mobile-first creative-coding platform; KidLisp, a Lisp dialect for generative art with 118 built-in functions, with ports to WASM, Game Boy, Nintendo 64, and Playdate.
> - Open-source tools that have reached the front page of Hacker News (No Paint, notepat).
>
> Credentials: Yale MFA in Graphic Design (2013), Ringling BFA (2011). Author in Residence at UCLA Social Software with Casey Reas and Lauren Lee McCarthy. Work in the collections of KADIST (San Francisco) and SMK (the National Gallery of Denmark). v11 on Tezos mainnet since March 2026.

## Project website
**https://aesthetic.computer**

## Have you already started?
**Yes** *(form value `1`)*

## Code Repository
**https://github.com/whistlegraph/aesthetic-computer**

## Demo URL
**https://keeps.kidlisp.com**

## Is your proposal below USD 50,000?
**Yes** *(form value `1`)*

## If above 50K, describe why
*(blank — we're below 50K)*

## Requested Grant Amount USD
**45000**

## Other funding from third parties USD
**0**

## Any other administrative detail
> Applicant entity: Aesthetic Inc (US C-corporation, EIN 33-1483385). The grant would be held by Aesthetic Inc and disbursed per milestone. A detailed milestone breakdown (M1–M5, totaling USD 45,000 over ~14 weeks) accompanies this proposal on the Roadmap step.
>
> Scope summary: M1–M3 are v12 contract engineering (spec, threat model, Ghostnet deployment, mainnet deployment, opt-in v11→v12 migration). M4 publishes the v12 contract as a reusable preservation template under an OSI license, accompanied by a 3–5 video tutorial series walking developers through forking, minting, and the 25-year audit cadence. M5 funds the LA adoption arm — 4 NELA Computer Club appointments at Plot.Place (Chinatown, LA) plus 1 public workshop — guiding 5+ LA-based generative artists through forking the v12 template and minting their first keeps, published as a public onboarding playbook.
>
> Self-audited scope: this proposal does not request funding for a third-party security review; v12 is hardened with an internal threat model document, SmartPy invariant tests, an adversarial test pass, and the existing `keeps-audit.mjs` cadence. If the Foundation's reviewers prefer a funded external audit, it can be added as a separable follow-on line item rather than holding up this scope.

## Previous contact to Tezos ecosystem
> No prior formal contact with the Tezos Foundation or with named ecosystem advisors. The contact is the work itself: KidLisp Keeps is live on Tezos mainnet (contract KT1Q1irsjSZ7EfUN4qHzAB2t7xLBPsAWYwBB, v11) and on Ghostnet (KT1StXrQNvRd9dNPpHdCGEstcGiBV6neq79K, v3). The mint flow uses the Beacon SDK and is interoperable with Temple, Kukai, and Ledger wallets. Keep tokens appear and trade on objkt.com. The contract series (v2 → v9 → v11) has been operated on mainnet since March 2026.
