# The Score Is the Software

### A piece composed on the Aesthetic Computer instrument stack, performed live, and published as a score

**Jeffrey Alan Scudder — CultureHub LA Residency 2026**
*1–2 weeks, CultureHub LA · Fall 2026 (pref. late Oct / early Nov) · $2,000 stipend*

---

## Summary

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

## What I will develop during the residency

I have the instruments — they ship today, open-source, in any browser
and on AC Native OS. What I have *not* done is **compose and notate a
concert-length piece for them and produce a publishable score.** That
is the work, and the residency is exactly the room and time for it.

1. **Week 1 — Compose.** Write the piece on notepat / sinebells /
   chord / beat: fix its form, harmony, and length. Composed by hand
   from the instrument primitives — the bottom-up method behind the AC
   music research, not end-to-end generation.
2. **Week 1–2 — Rehearse + engrave.** Rehearse to performance
   tightness in the CultureHub studio. In parallel, engrave the score:
   the *Whistlegraph* graphic rendering and the cleaned AC/KidLisp
   source listing, typeset through the papermill into a printed sheet.
3. **Final day — Perform + publish.** Perform the piece live for the
   CultureHub audience, and leave a stack of the printed scores in the
   room for people to take.

The honest risk, stated plainly: I have never engraved an AC
performance as a readable static document before. Making an executable
piece legible *as a score* — so a stranger can both read it and re-run
it — is the unsolved part. Week 1 is budgeted to fail openly on the
notation while the composition itself stays playable.

---

## The piece — *REPL* (~12 minutes, continuous)

A program's life is **Read → Eval → Print → Loop** — which is also
statement → development → restatement → coda. Using the REPL as the
musical form is not a gimmick: it makes the piece legible as a score,
because the form is the same shape as the source that produces it.

**The cell:** a 4-note motif **D – F – A – G** (dorian color),
memorizable on first hearing — the "function" that everything else
calls.

- **BOOT** *(0:00–0:45)* — one sinebell on D swells from silence; the
  room tunes to it.
- **READ** *(0:45–3:30)* — the cell stated plainly, slow, notepat solo
  over the D drone; a single sustained Dm9 from chord. No beat. The
  listener is handed the material to hold.
- **EVAL** *(3:30–7:30)* — the computation. beat enters (sine-kick,
  half-time); chord moves `Dm – B♭ – C – Gm` with a bright feint to F
  major; notepat fragments and recurses the cell against sinebells.
  Climax ~6:45: a +2-semitone lift to E, full stack, the cell restated.
- **PRINT** *(7:30–10:30)* — texture clears to "output." The cell
  returns whole and harmonized, a Picardy turn toward D, lyrical notepat
  over a sinebell halo. The legible version — the one engraved on the
  card.
- **LOOP** *(10:30–12:00)* — the cell reduces to its first two notes
  (D – F), thinning; written so the tail can splice back to BOOT, played
  once as an ending or actually looped live.

**The take-away card.** Side A: a horizontal graphic score in the AC
menubar-piano-roll language — four colored lanes (notepat / sinebells /
chord / beat), the four phases labeled, the cell drawn as a recurring
glyph every time it is called, so the eye tracks the recursion
(*Whistlegraph* is the precedent). Side B: the runnable KidLisp/AC
source. Reading the card teaches the piece; running it plays it.

---

## Why CultureHub

CultureHub supports artists experimenting with emerging technology and
gives the two things this piece needs that a browser tab cannot: a room
with a clean signal and projection path, and a public presentation with
technical support to make it sound and look right. The piece is also a
direct argument for CultureHub's own thesis — that technology is a
medium you compose *with*: here the instrument, the performance, and
the score are literally the same material. It extends the Aesthetic
Computer score research — the *Whistlegraph* graphic-score work and the
broader "score as self-teaching document" thread — into one concrete,
performed, printed object that outlives the live night.

---

## Performance, logistics, budget

notepat is played live — the expressive line, the rubato in *Read*, the
climax phrasing in *Eval*. chord / beat / sinebells run on a KidLisp
macro clock but are triggered and modulated live at the section seams,
so the public night is a performance, not a playback. **Solo is the
default and fully sufficient;** one invited LA co-performer (a second
notepat or a voice doubling the cell in *Print*) is a Week-1 decision,
not a promise.

| Need | Who provides |
|---|---|
| AC kernel + notepat / sinebells / chord / beat (shipping) | Artist |
| Composition + the engraved score (papermill print run) | Artist |
| Studio with clean audio + projection + a good PA | CultureHub LA |
| Technical support (signal path, projection, mics) | CultureHub |
| Public presentation + documentation | CultureHub |

CultureHub's offer — a **$2,000 stipend** plus studio, technical
support, marketing, documentation, and the public presentation —
covers the residency. The only added costs are the score print run
(≈$100) and, if a co-performer is brought in, a single honorarium
(≈$300). Total out-of-pocket: ≈$100–$400 — in budget. **Track: the LA
program** — the piece is composed and performed in one room and does
not depend on the NY-hybrid telepresence track.

---

## Bio

Jeffrey Alan Scudder is an artist, educator, and technologist based in
Los Angeles. He builds instruments and tools for other artists, with a
live and active practice across performance painting, software writing,
and teaching. Yale MFA (2013), Ringling BFA (2011). Author in Residence
at UCLA Social Software with Casey Reas and Lauren Lee McCarthy. Creator
of Aesthetic Computer, Whistlegraph, and No Paint. His open-source
tools *No Paint* and *notepat* each reached the front page of Hacker
News. Work in the collections of KADIST (San Francisco) and SMK
(Copenhagen). Hosts biweekly NELA Computer Club demos at Plot.Place,
Chinatown LA.

---

## Work samples

1. **notepat performance reel** — 3 min, solo on the instrument.
2. **Composed-passage clip** — 60–90 sec of a hand-composed AC
   instrument passage, showing the bottom-up method.
3. **Whistlegraph paper** — the graphic-score thesis behind the score
   document (`papers/arxiv-whistlegraph/`).
4. **One previous live-performance still** — NELA Computer Club / 47th
   Venice Family Clinic Art Exhibition 2026 / a UCLA classroom.

---

*Apply: https://www.culturehub.org/residency · deadline 2026-05-20,
23:59 PT. This is the reading copy; the field-by-field submission text
and the full composition sketch live alongside it in `DRAFT.md` and
`COMPOSITION.md`.*
