# CultureHub — LA Residency 2026 — Research

> **Deadline:** 2026-05-20, 23:59 PT
> **Apply:** https://www.culturehub.org/residency
> **Length:** 1–2 weeks at CultureHub LA (separate NY hybrid track also open)
> **Stipend:** $2,000 (LA program); $2,000 + travel + housing (NY hybrid)
> **Includes:** studio with reconfigurable lighting/audio/video/projection,
> technical support, marketing, documentation, one public presentation
> **Eligibility:** must be based in Los Angeles or New York

## What CultureHub supports

Time-based work that engages emerging technologies — VR, AI, robotics,
creative coding, telepresence systems. CultureHub builds and maintains
two open-source platforms specifically for residents to use:

- **LiveLab** — telepresence platform for distributed live performance
- **CultureHub Broadcaster** — interactive livestreaming

A core CultureHub interest is **IRL/URL hybrid** performance — work
that engages an audience simultaneously in-person and online.

## Why AC fits

The instruments are already built, open-source, and shipping: **notepat**
(an 8,466-line polyphonic engine, 192 kHz, 32 voices) plus *sinebells*,
*chord*, and *beat* — a complete instrument stack made from scratch. The
unsolved, fundable work is *composing a concert-length piece for them and
producing a publishable score* — which is exactly what a room + time + a
public presentation buys.

The score-as-document idea is native to AC, not bolted on. The
*Whistlegraph* paper (`papers/arxiv-whistlegraph/`) already frames
drawing-plus-singing as a graphic score; the broader AC "score" research
treats a piece as a self-teaching document. An AC/KidLisp piece is
URL-addressable and executable — it is *already* a score that can be
read and re-run. The residency makes that literal: one composed piece,
performed live, then engraved through the AC papermill into a printed
sheet that is simultaneously graphic notation and runnable source.

## Suggested angle

**"The Score Is the Software" — compose one performable piece on the AC
instrument stack, perform it live, publish it as a score.**

A 1–2 week LA residency producing one ~10–15 minute piece:

- **Compose** the piece by hand from notepat / sinebells / chord / beat
  — bottom-up from the instrument primitives (the AC music-research
  method; never end-to-end generation).
- **Perform** it live for the CultureHub audience on the final day —
  solo by default, with one optional invited LA co-performer if the
  composition asks for it.
- **Publish** it as a score: a single-sheet card, engraved through the
  papermill (`cards-convert.mjs` / arXiv pipeline), that carries both a
  *Whistlegraph*-style graphic notation and the cleaned AC/KidLisp
  source. A take-away stack is left in the room.

This is a single spine — instrument → performance → score, one material
— not a telepresence/multiplayer buffet. It positions AC as a medium you
*compose with*, and produces a durable artifact (the printed score)
beyond the live night.

## Source materials in repo

- `papers/arxiv-notepat/notepat.tex` — the instrument paper
- `papers/arxiv-whistlegraph/whistlegraph.tex` — graphic score / score
  as document
- `papers/arxiv-folk-songs/folk-songs.tex` — repertoire / score thinking
- `papers/arxiv-plork/plork.tex` — instrument-as-ensemble thesis
- `pop/` — the bottom-up AC music composition lane (notepat / sinebells
  / chord / beat)
- `papers/cards-convert.mjs` — papermill engraving path for the score card
- `system/public/aesthetic.computer/disks/notepat.mjs` — the instrument
  source (itself a form of score)

## Open questions for the user

- **Score form:** Whistlegraph graphic score, the KidLisp/AC source
  listing, or both on one card? (Recommended: both — that *is* the point.)
- **Solo vs. one invited LA co-performer** — decide during Week 1 from
  what the composition needs; don't promise an ensemble up front.
- **LA vs. NY-hybrid track:** LA is the clean fit — the piece is composed
  and performed in one room and does not depend on telepresence. NY-hybrid
  stays an option but isn't the work.
- **Tech rider:** confirm the AC kernel + instruments run cleanly through
  CultureHub LA's audio + projection path before the public night.

## Sources

- [CultureHub Residency](https://www.culturehub.org/residency)
  (form: https://forms.gle/TUNpYcWNV835N9iQ7)
- [Past residency announcements (Q&A 2025)](https://www.culturehub.org/events/residency-q-a-2025)

## Next steps

1. Open the application form, capture exact required fields and word counts.
2. Lock the score form (recommend graphic + source on one card).
3. Pull a 3-min notepat reel + a 60–90 s composed-passage clip.
4. Confirm performance month preference within Fall 2026 (late Oct / early Nov).
