# CultureHub — LA Residency 2026 — Application Draft

> **Deadline:** 2026-05-20, 23:59 PT
> **Length:** 1–2 weeks at CultureHub LA, $2,000 stipend, tech support,
> public presentation
> **Apply:** https://www.culturehub.org/residency (form: https://forms.gle/TUNpYcWNV835N9iQ7)
>
> *This draft is the canonical narrative source. Trim and re-shape once
> we have the exact form fields + word caps. Length targets below are
> guesses pending the form.*

---

## Project Title

**The Score Is the Software — a piece composed on the Aesthetic
Computer instrument stack, performed live, and published as a score**

(Working title; alternates: *"One Material: instrument, performance,
score"*, *"A Piece and Its Document"*.)

---

## One-paragraph Description (~120 words)

During the residency I compose a single performable piece of music
entirely from the Aesthetic Computer instrument stack — instruments I
built from scratch: **notepat**, an 8,466-line polyphonic engine
running at 192 kHz with 32 voices, plus the *sinebells*, *chord*, and
*beat* instruments. Nothing is generated end-to-end; the piece is
composed bottom-up from these primitives the way one writes for
acoustic instruments. I perform it live for the CultureHub audience on
the final day. The piece is then published as a **score** — a document
that is at once a *Whistlegraph*-style graphic notation and the literal
AC/KidLisp source that produced it, typeset through the Aesthetic
Computer papermill as a single-sheet card the room can take, read, run,
and re-perform. The instrument, the performance, and the score are one
material.

---

## What I Want To Develop During The Residency

I have the instruments — they ship today, open-source, in any browser
and on AC Native OS. What I have *not* done is **compose and notate a
concert-length piece for them and produce a publishable score.** That
is the work, and the residency is exactly the room and time for it:

1. **Week 1 — Compose.** Write the piece on notepat / sinebells /
   chord / beat: fix its form, harmony, and length (~10–15 minutes).
   Composed by hand from the instrument primitives — the bottom-up
   method behind the AC music research, not Suno-style end-to-end
   generation.
2. **Week 1–2 — Rehearse + engrave.** Rehearse it to performance
   tightness in the CultureHub studio. In parallel, **engrave the
   score**: produce the *Whistlegraph* graphic-score rendering and the
   cleaned AC/KidLisp source listing, typeset through the papermill
   (`cards-convert.mjs` / the arXiv pipeline) into a printed sheet.
3. **Final day — Perform + publish.** Perform the piece live for the
   CultureHub audience, and leave a stack of the printed scores in the
   room for people to take.

The honest risk, stated plainly: I have never engraved an AC
performance as a readable static document before. Making an executable
piece legible *as a score* — so a stranger can both read it and re-run
it — is the unsolved part. Week 1 is budgeted to fail openly on the
notation while the composition itself stays playable.

The concrete musical plan — working title *REPL*, a ~12-minute
continuous piece whose Read / Eval / Print / Loop form mirrors the
source that produces it, with per-instrument roles, harmonic plan, and
the score-card spec — is sketched in `COMPOSITION.md` alongside this
draft.

---

## Why CultureHub

CultureHub supports artists experimenting with emerging technology and
gives the two things this piece needs that a browser tab cannot: a
**room with a clean signal and projection path**, and a **public
presentation** with technical support to make it sound and look right.
The piece is also a direct argument for CultureHub's own thesis — that
technology is a medium you compose *with*: here the instrument, the
performance, and the score are literally the same material. It extends
the Aesthetic Computer score research (the *Whistlegraph* graphic-score
work and the broader "score as self-teaching document" thread) into a
single concrete, performed, printed object.

---

## Logistics

| Need | Who provides |
|---|---|
| AC kernel + notepat / sinebells / chord / beat (shipping) | Artist |
| Composition + the engraved score (papermill print run) | Artist |
| Studio with clean audio + projection + a good PA | CultureHub LA |
| Technical support (signal path, projection, mics) | CultureHub |
| Public ticketed presentation + documentation | CultureHub |
| Optional: one invited LA co-performer (2nd notepat / voice) | Artist (invited) |

Solo is the default and fully sufficient. One invited LA co-performer
(a second notepat or voice) is a stretch option if the composition asks
for it — decided during Week 1, not promised up front.

---

## Honorarium / Cost

CultureHub's posted offer is a **$2,000 stipend** plus the studio,
technical support, marketing, documentation, and the public
presentation. That covers the residency. The only added costs are the
score print run (≈$100 for a take-away stack of the card) and, if a
co-performer is brought in, a single honorarium (≈$300). Total
out-of-pocket: ≈$100–$400 — in budget.

---

## Performance Date Window

Fall 2026 — preferred **late October or early November** (lines up with
the fall semester at UCLA Social Software where the AC cards are
circulating, and with my own LA calendar). Will adapt as CultureHub's
booking allows.

---

## Track Preference

**LA program** (1–2 weeks at CultureHub LA, $2,000 stipend). This piece
is composed and performed in one room — it does not depend on the
NY-hybrid telepresence track, so LA is the clean fit. (If asked, I am
open to the NY-hybrid track with the same piece, but it is not the
point of the work.)

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

1. **notepat performance reel** — 3 min, solo on the instrument; pull
   from existing AC documentation.
2. **Composed-passage clip** — 60–90 sec of a hand-composed AC
   instrument passage (sinebells / chord / beat) showing the bottom-up
   method, not generative output.
3. **Whistlegraph paper** — `papers/arxiv-whistlegraph/whistlegraph.pdf`
   (4 pp.) as the graphic-score thesis behind the score document.
4. **One previous live-performance still** — NELA Computer Club, 47th
   Venice Family Clinic Art Exhibition 2026, or a UCLA classroom.

---

## Source Materials Repurposed Here

- `papers/arxiv-notepat/notepat.tex` — the instrument paper
- `papers/arxiv-whistlegraph/whistlegraph.tex` — graphic score / score
  as document
- `papers/arxiv-folk-songs/folk-songs.tex` — repertoire / score thinking
- `papers/arxiv-plork/plork.tex` — instrument-as-ensemble thesis
- `pop/` — the bottom-up AC music composition lane (notepat / sinebells
  / chord / beat; never end-to-end generation)
- `papers/cards-convert.mjs` — the papermill engraving path for the
  printed score card
- `system/public/aesthetic.computer/disks/notepat.mjs` — the instrument
  source (itself a form of score)

---

## Submission Checklist

- [ ] Open the actual form (https://forms.gle/TUNpYcWNV835N9iQ7),
      capture exact fields + word caps
- [ ] Trim each section to fit the form
- [ ] Decide the score's form: Whistlegraph graphic score, the
      KidLisp/AC source listing, or **both on one card** (recommended)
- [ ] Decide solo vs. one invited LA co-performer
- [ ] Pull the notepat reel + a composed-passage clip
- [ ] Confirm performance date preference (late Oct / early Nov)
- [ ] Submit before 2026-05-20, 23:59 PT
