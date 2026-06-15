# Restless Egg — Research

**Status:** researching → outline (decision: APPLY). Sourced 2026-06-15.
**Deadline:** **2026-06-30** (Batch 2; live portal `apply.restlessegg.com`).
**Program starts:** September 2026. **Contact:** eggs@restlessegg.com.

> NOTE: detailed Aesthetic Inc. financials (bank balances, account numbers,
> EIN, cap table) live in the **vault**, not this public file. Everything below
> is either public (the aesthetic.direct page, `papers/arxiv-sustainability`) or
> general enough to be safe in the public repo.

## What Restless Egg is

A **London/Berlin-based accelerator/incubator for "artist-founders"** — NOT a
residency or grant. It funds creative technologists turning "mature ideas,
prototypes, and creative practices into products, companies, and markets."

- **Thesis:** "luxury technologies" — "products using AI and emerging tech to
  enhance human experience **beyond mere efficiency**"; "category-defining
  technology products" that go beyond productivity/automation/efficiency.
- **Founders:** William Morgan (PhD, "philosopher of technology"), Sylvan
  Rackham, Sam Lipnick. UK company RESTLESS EGG LTD (Companies House #15864981);
  salons in Berlin. London-run; applying remotely from LA is fine.
- **Terms (a VC deal, not a stipend):** ~**$100k** initial + up to **$175–200k**
  follow-on for **~5% equity**, with a **"Shared Earnings" side letter** that
  allows EITHER a venture exit OR an independent profitable outcome.
- **6-month program**, starts Sept 2026.
- **What they look for:** work that "began as an obsession, experiment, or
  frontier practice"; evidence of real users / market demand; "technical or
  experiential originality"; potential to become "a **durable business** rather
  than remain an artwork, service, or research project." No location limits.

**Sources:** restlessegg.com/page/opportunities · apply.restlessegg.com · the
Dazed profile · UK Companies House #15864981 · founders' LinkedIns ·
@Restless_Egg on X.

## Why this fits (and why the commons tension dissolves)

The fit is unusually clean because **the company already exists** and its stated
mission is already a productization thesis:

- **Aesthetic, Inc.** — a real **C-corp** (EIN on file in vault; files Form 1120;
  Mercury banking opened 2024-10-15; officer Jeffrey Alan Scudder). Announced
  **2024-11-18** on the `aesthetic.direct` page.
- **`aesthetic.direct`** — AC's **existing equity-investment page**. Early
  supporters already converted into **SAFE holders** via the "booted-by" model
  (`disks/booted-by.mjs`; `GRAVEYARD.txt` "Aesthetic Direct No. 1" spec; the
  honest assessment is in `papers/arxiv-sustainability/sustainability.tex:272` —
  "Neither mechanism has produced institutional-scale funding"). **Restless Egg
  would be the lead SAFE/equity investor into a vehicle that already exists.**
- **Mission already on file** (`aesthetic-direct.html`): *"to lift the
  computational literacy level of mass audiences by making computing more
  playable and accessible through the invention, distribution and
  **productization of new media instruments**."* ← reads as a Restless Egg thesis.

**The resolution of the commons-vs-company tension:** the AC *platform* stays an
open commons (the SSRC framing); **Aesthetic Inc. productizes instruments out of
that commons** (the Restless Egg framing). Same company, two honest stories, no
double-spend of the same idea.

## The lead product — notepat (flagship), with a pipeline behind it

`notepat.mjs` is the most product-shaped asset in the system (9,210 lines, "the
most complex piece"):
- Live branded domain **notepat.com** (edge-rewritten).
- **Default boot piece (PID 1) on the bare-metal AC Native OS** — instrument-grade
  sub-10ms latency on the metal (browser latency ~417ms is the open problem).
- 10 synth engines incl. full **General MIDI** (128 patches via `lib/gm.mjs` +
  the 3,870-line physical-modeling `fedac/native/src/gm_synth.c`), WebHID
  analog-pressure keyboard, MIDI publish/relay.
- Distribution surfaces: **Max for Live device (alpha)**, **VST3 (`ac-vst/`,
  scaffold — engine not yet functional)**, App Store iOS+iMessage wrapper
  (shipping, wraps whole app), Electron desktop (v0.1.48).
- **HN front page twice** (No Paint 2020, notepat 2024) = real-user evidence
  (need external HN permalinks to substantiate; not archived in-repo).

**Pipeline behind the flagship:**
- **The "nom" games** — Number-Munchers-style educational games on a shared
  2,062-line engine (`lib/nom.mjs`): numbnom (math), engnom (English), mexinom
  (Spanish), dannom (Danish), rusnom (Russian), notenom (music), catnom. Built,
  **web-only, ~1 week old, standalone packaging is net-new work**. Ideal "family
  of apps" architecture (shared engine + locale packs).
- **KidLisp** — accessible creative-coding language (118 built-ins) + shareable
  `$codes`; 16,000+ programs created.
- **AC Native + the gm_synth engine** — the technical moat under the instruments.

## Traction — the honest numbers (a VC will probe these)

- **~18,000 registered accounts** (5,373 verified emails); **~2,600 claimed
  handles**; **16,000+ KidLisp programs**, 4,400 paintings, 18,000 chat messages.
- **The weakness, stated plainly:** heavy dormancy — ~1,457 active/year,
  129/month, 34/week; ~92% logged in once. Signups front-loaded (2023: 10.9k →
  2025: 1.2k). **No analytics pipeline (no GA/Plausible/PostHog)** — engagement
  numbers are ad-hoc queries. (Source: vault `user-reports/`,
  `papers/arxiv-ac/ac.tex`.)
- **Revenue is real but small and diversified:** a live Stripe subscription
  (SOTCE NET, co-run; price set in Stripe, not repo), `give` donations
  ($1–$2,500), tiny on-chain NFT volume (~134.5 XTZ lifetime), services gigs
  ($600–$1,200). Merch + minting are cost-recovery. Shopify store offline since
  Mar 2026. (Source: vault banking + `system/netlify/functions/`.)

**Honest framing for the app:** the validated asset is *creative output + a
novel instrument engine + a built audience*, not yet *retained DAU or material
revenue*. The investment funds turning the flagship instrument into a retained,
revenue-generating product. The "Shared Earnings" structure is the right
instrument precisely because this is a durable-instrument-business path, not an
obvious hypergrowth one.

## Company history (for the founder narrative)

- **Lineage:** 94 predecessor projects 2007–2020 (`arxiv-archaeology`). Direct
  parent = **No Paint** (2014–2020, five incarnations). Performance-tool work
  with Goodiepal & Pals; Whistlegraph (2.6M TikTok followers) is the GitHub org
  the repo bootstrapped from.
- **AC conceived April 2021**; repo re-rooted Dec 2022; ~20,100 commits today
  across ~25 contributors (Jeffrey dominant; an "Oven" automation bot second).
- **Milestones:** KidLisp (Apr 2024) · **Aesthetic Inc. (Nov 18 2024)** ·
  kidlisp.com IDE + ATProto federation (Nov 2025) · **AC Native bare-metal OS
  (Mar 2026)** · the papermill (19 papers, 4 languages).
- **Recognition:** KADIST, SMK (Denmark), ZKM, Rhizome (collectors/exhibitors).

## Open items before submit
- [ ] Pull the exact `apply.restlessegg.com` Google-Form fields + any word caps.
- [ ] Decide: lead the whole app on **notepat** as the company, or **Aesthetic
      Inc. (instruments studio)** with notepat as flagship? (Outline assumes the
      latter.)
- [ ] Get external HN permalinks (No Paint 2020, notepat 2024) to substantiate.
- [ ] Confirm SOTCE subscription monthly price (Stripe dashboard) if cited.
- [ ] Confirm whether existing SAFE terms / the booted-by cap table create any
      conflict with a Restless Egg lead — review with the vault cap records.
- [ ] Decide on the ~5% dilution and the "Shared Earnings" path (founder call).
