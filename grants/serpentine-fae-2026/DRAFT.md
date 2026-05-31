# Serpentine — Future Art Ecosystems R&D Fellowship: Art × Convergence — Application Draft

> **Deadline:** 2026-06-07, midnight BST
> **Award:** £10,000 + travel & accommodation for three London weekends
> **Format:** Hybrid, six months (Sep 2026 – Mar 2027)
> **Submit:** Form linked from the [programme page](https://www.serpentinegalleries.org/whats-on/future-art-ecosystems-rd-fellowship-art-x-convergence/)
>
> *Draft v0. The form's exact fields and word counts are not yet
> captured; the sections below are written as the natural shape of the
> proposal and will be trimmed against the form.*

---

## Project Title

**A Voice of My Own — building a tract-fit jeffrey synth inside Aesthetic Computer**

(Working title; alternates: *Reclaiming the Throat*, *The Singing Tract*.)

---

## One-paragraph Description (~150 words)

I have spent two years building **Aesthetic Computer** — a mobile-first
runtime, operating system, and social network for creative computing —
as an alternative to the platforms most artists currently rent from. The
last unfinished room in AC is my own voice. I currently sing through a
hosted clone of myself (ElevenLabs `jeffrey-pvc`) inside a pop/ pipeline
where every sung word is sliced, re-pitched, and harmonised in code. I
want six months to walk that pipeline out of the cloud: fit a Pink
Trombone vocal-tract model to my own clone using a minimal training
corpus, ship the result as a small C voice that runs natively inside AC
Native OS, and write the songs the harness asks for. The R&D output is
documented process — a working binary, a paper, a reel — and the framing
question is exactly the *Art × Convergence* one: what does it mean for
an artist, in 2027, to own the throat they are sung through.

---

## What I am developing during the fellowship

I have already built the long parts. What's missing is the last hop from
hosted clone to artist-run instrument.

**Already in place:**

- **Aesthetic Computer** — a six-year-old runtime / social network /
  operating system, ~30 papers, daily public output, an active library
  of refurbished laptops booting directly into the AC kernel.
- **jeffrey-pvc** — an ElevenLabs voice clone tuned for identity
  stability (≥ 0.5 stability keeps it sounding like me), wired into AC
  through `/api/say`.
- **The pop/ vocal post-production pipeline** — every sung word is a
  slice in code, with pitch / elongation / harmonisation / aggression
  knobs, written into a `.edit.json` recipe per track.
- **Pink Trombone harness — early.** Open exploration: fit a Pink
  Trombone tract model to `jeffrey-pvc` on a 123 / abc minimal corpus,
  produce a tiny C-runnable jeffrey voice.

**What the six months funds:**

1. **Months 1–2 (Sep–Oct 2026).** Finish the pop/ vocal pipeline as a
   stable, documented artist-facing tool. Fix the open syllable-to-note
   alignment drift (multi-note melisma currently misses). Publish the
   recipe format.
2. **Months 2–4 (Oct 2026 – Jan 2027).** Pink Trombone fitting in
   earnest. Build the training corpus, write a perceptual-loss fitter,
   port the best parameter trajectory into a small C runtime that loads
   directly on AC Native OS.
3. **Months 4–6 (Jan – Mar 2027).** Make work *for* the new voice —
   short songs, sung essays, a piece that reads aloud one of the
   `papers.aesthetic.computer` papers in my own throat without any
   internet connection. Documentation: a paper, a public reel, the
   binary.

The risk is real. Fitting Pink Trombone to a clone of myself with
perceptual loss is research, not engineering — it may not converge
cleanly. The fallback is honest and useful: a much richer per-word
post-production pipeline plus a dossier of what failed in the fit, which
is itself a contribution to the field's shared knowledge of *how far you
can carry one voice with how little*.

---

## How this connects to "Art × Convergence"

The convergence brief asks how AI's capacity to pursue goals, model
environments, and act in the world reshapes cultural and societal
systems. Most artists meet that question from the outside: as users of
hosted creative-AI services, or as critics of them. I want to meet it
from inside: by building, end to end, an artist's own infrastructure for
the most personal of those services — voice.

That is not a defensive gesture. It is a *positive* one: there is
something specific that artists make when they own their throat, their
runtime, their kernel, their publication channel. AC is the long-form
argument for that position; the voice piece is the last room.

The fellowship would also let me bring this work into direct
conversation with people working the same question from different
starting points — including (without overstating) Holly Herndon's
ongoing voice work, where the question of cloning, ownership, and
artistic identity has already been pushed hard from the music side.

---

## Why this fellowship, specifically

- **FAE's published research has been the most serious public thinking
  about the infrastructure layer under art × tech.** AC is, end to end,
  infrastructure that artists actually use. A fellowship that funds the
  *practice of building infrastructure* rather than the *artworks built
  with it* is rare and exactly the right shape.
- **Documented process is the deliverable.** AC's papermill already
  publishes everything continuously: source, papers, dailies, recap
  shows, dossiers on every adjacent organisation. The fellowship's
  expectation that I document is not a stretch; it is what AC already
  does. The fellowship lets me document a specific, hard sub-thread
  with the time it deserves.
- **The London weekends are tractable.** Three trips over six months
  from Los Angeles is real, but achievable, and the cohort question —
  which other fellows, working what — is itself part of the appeal.

---

## My practice (~120 words)

Jeffrey Alan Scudder is an artist, educator, and technologist based in
Los Angeles. He builds instruments and tools for other artists, with a
live practice across performance painting, software writing, and
teaching. He is the author of **Aesthetic Computer**, a mobile-first
runtime and social network for creative computing, and **KidLisp**, a
minimal Lisp for generative art that ships inside it. Yale MFA (2013),
Ringling BFA (2011). Currently Artist in Residence at UCLA Social
Software with Casey Reas. His open-source tools *No Paint* and *notepat*
have each reached the front page of Hacker News. Work in the collections
of KADIST (San Francisco) and SMK (Copenhagen). Hosts biweekly NELA
Computer Club demos at Plot.Place, Chinatown Los Angeles.

---

## Media samples (to attach)

1. **pop/ vocal pipeline reel** — 90 sec, current jeffrey-pvc + per-word
   post in action, with the `.edit.json` recipe shown alongside.
2. **Pink Trombone harness sketch** — 60 sec, current fitting state on
   the 123 / abc corpus, however rough.
3. **AC Native OS demo** — 60 sec, a refurbished ThinkPad cold-booting
   from USB straight into the AC kernel (this is where the new voice
   will live).
4. **paper as PDF** — `papers/arxiv-notepat/notepat.pdf` (or
   `papers/arxiv-os/os.pdf`) as a written companion piece.

---

## Source materials from the AC repo

- `system/public/aesthetic.computer/disks/pop/` — the pop/ track
  authoring and vocal-routing code
- `papers/arxiv-os/os.tex` — AC Native OS '26 (where the voice binary
  will live)
- `papers/arxiv-notepat/notepat.tex` — the instrument paper (sister
  work)
- *(forthcoming)* `papers/arxiv-voice/` — the harness paper, draft
  during the fellowship

---

## Submission checklist

- [ ] Open the actual application form, capture the exact required
      fields + word caps
- [ ] Trim each section to fit the form
- [ ] Pull the four media samples
- [ ] Confirm London-weekend windows against jeffrey's Sep 2026 – Mar
      2027 calendar (NELA Computer Club, UCLA Social Software, Fia plans)
- [ ] Decide spine framing once and stick with it — voice *or*
      AestheticAnts, not both (see `feedback_proposals_single_spine.md`)
- [ ] Submit before 2026-06-07, midnight BST
