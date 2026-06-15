# Oracle Egg — BROILER Residency — Application Draft

> **STATUS (verified 2026-06-15): CYCLE CLOSED — no open call right now.**
> BROILER runs seasonal cohorts (NOT rolling). Pattern: open call ~Nov →
> mid-Dec deadline → spring residencies. Last call: Nov 2025 → Dec 11, 2025
> deadline → Spring 2026 cohort (performing now). The old "2026-05-15" in
> our tracker was a *performance* date, not an application deadline.
> **Next call expected ~Nov 2026; residencies likely early 2027.**
> Action: watch @oracle_egg + join the oracleegg.com newsletter; this draft
> stays warm for that call.
>
> **Length:** 3–7 days, full facility access (3,000 sq ft loft, 939 Maple
> Ave, DTLA Fashion District) · **Docs:** high-quality documentation ·
> **Public:** culminating ticketed performance · **Cost:** FREE to apply,
> no published stipend (in-kind support) · **Run by:** Melissa Achten &
> Eli Klausner, fiscally sponsored by Fulcrum Arts · **Submit:** form
> linked from https://www.oracleegg.com/broiler (live only during a call).
>
> *Exact form fields unknown until the next call opens. Length targets
> below are guesses; the project frame is a strong fit (ambitious sound +
> performance, priority to early-development work).*

---

## Project Title

**Library Orchestra — *notepat* on twelve flashed laptops**

(Working title; alternate: *"What a room of personal computers sounds like."*)

---

## One-paragraph Description (~120 words)

Twelve refurbished laptops, each booted directly into Aesthetic Computer
from USB on the first day of the residency, become a network instrument
for a single 30–45 minute room-scale piece. Each laptop runs **notepat**
— an 8,466-line polyphonic instrument the artist built from scratch over
two years, a sample-level audio engine running at 192 kHz with 32 voices
of polyphony — and the twelve are linked over a local WebSocket relay so
that what one player hears propagates to the others. The piece develops
the *PLORK* (planetary laptop orchestra) thesis: cheap commodity laptops,
flashed with a creative kernel, become an instrument library. The room
becomes the instrument the room itself can play.

---

## What I Want To Develop During The Residency

I have built the instrument (notepat). I have written the paper proposing
this exact form (`papers/arxiv-plork/plork.pdf`, 8 pages). What I have
*not* done is play twelve of these in one room. The residency is exactly
the right length and shape to:

1. **Day 1 — Flash Day.** Twelve refurbished ThinkPad 11e Yogas (already
   stripped of their consumer OS, $128 per seat per `papers/arxiv-os/`)
   boot into Aesthetic Computer. Performers — local LA musicians — each
   take a laptop. The room becomes the instrument library.
2. **Days 2–4 — Open rehearsal.** Develop a 30–45 minute score for
   twelve networked notepats, voice, room mics, and the PA. The piece
   uses AC's existing WebSocket relay (the same code that powers
   real-time multiplayer in `system/public/aesthetic.computer/disks/`)
   so all twelve laptops can phase, sync, cross-modulate, listen to each
   other, and respond.
3. **Day 5 (or 6 / 7) — Public ticketed performance.** The score in
   front of an audience. Documentation reel for online release.

The risk is real and acknowledged: I have never run twelve simultaneous
laptops on one local network running a shared synth state. Days 2–4 are
budgeted to fail openly, and the final-day performance is shaped by
whatever the room actually does, not what was planned.

---

## Why Oracle Egg

BROILER is for ambitious sound and performance work in early stages or
active development. This piece is in active development on every
component (notepat, the AC kernel, the WebSocket relay, the PLORK
argument) — what's missing is the room. Oracle Egg's 3,000 sf loft in
the Fashion District is exactly the kind of space the piece needs: enough
floor for twelve laptop stations and an audience, run by musicians who
understand the rehearsal-into-performance arc, with a culminating public
moment that turns a residency into something an audience comes to.

The residency is also the right cohort question: which LA musicians want
to play this? I would invite a mix from the **NELA Computer Club**
(biweekly demos at Plot.Place in Chinatown) and Oracle Egg's own active
circles. Twelve players, mixed practices, one room, one piece.

---

## Logistics

| Need | Who provides |
|---|---|
| Twelve refurbished ThinkPad 11e Yoga laptops | Artist (AC Library / shipped from LA studio) |
| Twelve USB drives, pre-flashed with AC Native OS | Artist |
| Twelve sets of headphones | Artist |
| Local WebSocket relay (laptop or RPi) | Artist |
| Eight to twelve cheap powered speakers + mixer + mic stands | Oracle Egg / artist (rider TBD) |
| Local network (ethernet or 5GHz wifi without internet exit) | Oracle Egg / artist |
| Performance space + door + ticketing | Oracle Egg |
| Performers (≈8 invited) | Artist (LA-based, mix of NELA Computer Club + invited) |

---

## Honorarium / Cost

The form will state Oracle Egg's policy. **My ask:** a per-day artist
fee for performers (eight performers × $200/day × 4 days =
$6,400 across the cohort), a $1,500 production budget for travel of
shipped equipment + speaker rental + mixer rental, and an artist fee for
the residency lead at Oracle Egg's standard rate. If Oracle Egg's
policy is artist-fee-only with no production budget, I will adapt the
piece to fewer performers (six instead of twelve) and use existing
equipment.

---

## Performance Date Window

September – December 2026. Strong preference for **early-to-mid
October 2026** (lines up with the fall semester at UCLA Social Software
where the AC cards are circulating, and with my own LA calendar). Will
adapt as Oracle Egg needs.

---

## Short Bio (~100 words)

Jeffrey Alan Scudder is an artist, educator, and technologist based in
Los Angeles. He builds instruments and tools for other artists, with a
live and active practice across performance painting, software writing,
and teaching. Yale MFA (2013), Ringling BFA (2011). Author in Residence
at UCLA Social Software with Casey Reas and Lauren Lee McCarthy. Creator
of Aesthetic Computer, Whistlegraph, and No Paint. His open-source tools
*No Paint* and *notepat* each reached the front page of Hacker News.
Work in the collections of KADIST (San Francisco) and SMK (Copenhagen).
Hosts biweekly NELA Computer Club demos at Plot.Place, Chinatown LA.

---

## Media Samples (to include)

1. **notepat performance reel** — 3 min, solo or duo on browser; pull
   from existing AC documentation.
2. **AC Native OS boot reel** — 60 sec showing a ThinkPad cold-booting
   into notepat from USB (kernel prototype landed Feb 2026).
3. **PLORK paper** — `papers/arxiv-plork/plork.pdf` (8 pp.) as written
   thesis.
4. **One previous live-performance still** (NELA Computer Club, 47th
   Venice Family Clinic Art Exhibition 2026, or a UCLA classroom).

---

## Source Materials Repurposed Here

- `papers/arxiv-plork/plork.tex` — the PLORK / planetary laptop
  orchestra argument (primary intellectual frame)
- `papers/arxiv-notepat/notepat.tex` — the instrument paper
- `papers/arxiv-os/os.tex` — AC Native OS '26, the kernel that boots
  notepat as PID 1
- `papers/arxiv-folk-songs/folk-songs.tex` — repertoire / score thinking
- `papers/arxiv-whistlegraph/whistlegraph.tex` — graphic-score thinking
- `system/public/aesthetic.computer/disks/notepat.mjs` — the instrument
  source
- `gigs/dis-order-pamphlet-8.5x11-folded/` — example of how AC ideas
  travel into print form for an audience handout

---

## Submission Checklist

- [ ] Open the actual Google Form, capture exact fields + word caps
- [ ] Trim each section to fit the form
- [ ] Decide ensemble framing (solo / "Jeffrey + AC Library Orchestra")
- [ ] Confirm equipment count (12 vs. 8 vs. 6 laptops based on Oracle
      Egg's space + budget)
- [ ] Pull the performance reel
- [ ] Confirm performance date preference window
- [ ] Submit before 2026-05-15, 23:59 PST
