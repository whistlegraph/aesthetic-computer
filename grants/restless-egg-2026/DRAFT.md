# Restless Egg — Batch 2 Application DRAFT v1
### Aesthetic, Inc. · founder Jeffrey Alan Scudder · due 2026-06-30

> **Form:** Google Form (sign-in required) linked from `apply.restlessegg.com`
> → `docs.google.com/forms/d/15cOcDuyYMEsgVO5FrrLTe7yqS9Cw6xy8edduoY8xEKg`.
> **The exact field labels/caps could not be scraped (login wall).** Open the
> form signed-in and map these answers to the real fields; trim to their caps.
> Answers below are organized as the standard accelerator fields + Restless
> Egg's stated proposal asks (their words: obsession/frontier-practice origin ·
> technical/experiential originality · real users/market demand · durable-
> business path). Lead framing = **Aesthetic Inc. as an instruments studio,
> notepat as flagship.**

---

## 0. Identity / contact (short fields)

- **Founder:** Jeffrey Alan Scudder
- **Company:** Aesthetic, Inc. (Delaware/MA C-corp, incorporated Nov 2024)
- **Email:** mail@aesthetic.computer
- **Location:** Los Angeles, CA (US; can attend Berlin/London remotely or in person)
- **Website:** https://aesthetic.computer · flagship: https://notepat.com ·
  company/equity: https://aesthetic.direct
- **Links:** CV jas.life · Bluesky/TikTok @aesthetic.computer · @whistlegraph

## 1. One-line: what are you building?

**Aesthetic Inc. is an instruments studio — we make new-media instruments you
*play*, not tools that make you efficient — starting with notepat, an expressive
software instrument that runs anywhere, down to the bare metal of a $50 laptop.**

## 2. What is it, in a paragraph?

Aesthetic Computer is a creative-computing platform with ~18,000 registered
accounts and 16,000+ programs people have made on it. Aesthetic Inc. is the
company that productizes instruments out of that platform. The first product is
**notepat**: a melodic instrument — QWERTY, touch, MIDI, analog-pressure
keyboard — driven by a custom 128-voice physically-modeled synthesizer we wrote
from scratch. It already lives at notepat.com, ships inside our iOS/Mac apps, and
boots as the *first process* on our own bare-metal operating system (sub-10ms
latency on hardware the consumer market threw away). Behind notepat is a pipeline:
a family of learn-by-playing games on a shared engine, and KidLisp, a tiny
language anyone can use to make and share generative art. The platform is the
funnel and the factory; the instruments are the products.

## 3. The obsession / frontier-practice origin (their ask)

I'm a painter (Yale MFA) who spent a decade making the case — in 65+ performances
across Europe and the US — that the computer is painting's next material. That
obsession produced *No Paint* (2016, a picture-making instrument with one rule:
you can only paint, never erase) and then Aesthetic Computer (2021–), where I
stopped renting my tools and built the whole stack myself, in public, down to a
custom kernel. notepat is the sharpest expression of it: a decade-long bet that
software should be something you *play*.

## 4. Technical / experiential originality (their ask — the moat)

Nobody ships an instrument that boots as PID 1. Our originality is owning the
whole stack from kernel to synth:
- **A bare-metal creative OS (AC Native):** boots from USB as the first process —
  no systemd, no shell — into a playable instrument in ~7 seconds, with
  instrument-grade latency a browser can't reach.
- **A physical-modeling synth engine** (3,870 lines of C): all 128 General MIDI
  voices via digital waveguides and modal synthesis — a serious instrument on a
  fifty-dollar machine, not a sample-pack toy.
- **Runs everywhere from one codebase:** the same notepat is a web page, an app,
  a DAW plugin (Max-for-Live alpha; VST3 in progress), and the default piece on
  the metal. The instrument goes wherever the computer goes.

This is "luxury technology beyond efficiency" in the literal sense: it exists to
make a human more expressive, not more productive.

## 5. Real users / market demand (their ask — be honest)

- **Built audience:** ~18,000 registered accounts, ~2,600 claimed handles,
  16,000+ programs created, 4,400 paintings, 18,000 chat messages. Two Hacker
  News front-page moments (No Paint 2020, notepat 2024).
- **Honest gap I'm not hiding:** this is *creative-output traction*, not yet
  *retained DAU or material revenue*. ~92% of accounts logged in once; ~1,457 are
  active per year; we have no analytics pipeline yet. Growth was front-loaded
  (2023 → 2025 signups fell as I went heads-down building the OS).
- **Why that's the opportunity, not the objection:** I've proven people will
  *make things* here. I have not yet turned the flagship instrument into a
  retained, paid product. **That conversion is exactly what this raise funds.**

## 6. Path to a durable business + the 6 months (their ask)

- **notepat as a paid instrument** — a pro tier / standalone app / DAW plugin
  (the VST3 + Max-for-Live work is half-built). Musicians pay for instruments.
- **The games as an edtech family** — numbnom (math), engnom/mexinom/dannom
  (language), on one shared engine + locale packs: a low-cost, fast-to-ship
  catalog with a real K-12 / language-learning market.
- **The platform as funnel** — fix retention (analytics + onboarding) so free
  creators convert to paying players.
- **6 months:** ship notepat as a real revenue product; instrument the funnel;
  stand up the first two paid SKUs; bring on one collaborator. Concrete: first
  product revenue, a retention number we can actually measure, and a roadmap the
  follow-on milestone money unlocks.

## 7. Why Restless Egg specifically?

Your thesis is my company's written mission. Aesthetic Inc. exists "to lift the
computational literacy of mass audiences through the invention, distribution and
**productization of new media instruments**." You fund *artist-founders* building
*luxury technology beyond efficiency* — that's not a pitch I'm retrofitting, it's
the thing I incorporated to do. And your **"Shared Earnings" structure** is the
honest fit: Aesthetic Computer can be a durable, profitable instruments business
that keeps an open creative commons alive underneath it — I don't have to pretend
it's a hypergrowth SaaS to make the numbers true.

## 8. The raise / structure / what the money does for the founder

- **Structure:** Aesthetic Inc. already exists as a C-corp with an equity vehicle
  (`aesthetic.direct`), and a small number of early supporters already hold SAFEs
  via our "booted-by" model. **Restless Egg would be the lead investor anchoring
  a round that's currently scattered and under-funded** — your $100k + ~$175–200k
  follow-on for ~5% is exactly the catalyst that vehicle was built for.
- **What it changes for me:** today my time is split across freelance gigs and
  grant-chasing just to keep building. This is **6–18 months of full-time founder
  runway** to turn the flagship instrument into a product instead of a piece —
  the single highest-leverage thing I can do, and the thing I can't do part-time.
- **Cap-table note (to confirm pre-signing):** I need to reconcile the existing
  booted-by/SAFE terms with a lead — flagged as a founder/legal to-do, not a
  blocker.

## 9. Team / risks (lead with candor)

- **Team:** solo founder today, with ~25 contributors having touched the open
  platform over the years. The raise funds the first hire(s). Founder
  concentration is a real risk; I'm naming it.
- **Top risk = retention, not reach.** The whole point of the raise is to convert
  a creative-output platform into a retained instrument product.
- **Commons vs. company (so it can't be misread):** the open AC platform stays
  open and free; the company productizes *instruments* out of it. The commons is
  the moat and the funnel — investors buy into it, not over it.

---

## Pre-submit checklist
- [ ] Open the Google Form signed-in; map these to the real fields + trim to caps.
- [ ] Decide & state the ~5% dilution; reconcile booted-by/SAFE cap table.
- [ ] Add external HN permalinks (No Paint 2020, notepat 2024).
- [ ] Confirm incorporation state (DE vs MA) before writing it as fact.
- [ ] Attach/link a 30–60s notepat demo (bare-metal boot + play) if the form allows.
- [ ] Sequence with SSRC (Jun 28) — submit SSRC first, Restless Egg by Jun 30.
