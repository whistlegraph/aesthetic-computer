# Simplified Brief — ArtsIT 2026 Full Papers

Date: 2026-05-20

Deadline: 2026-06-01 (12 days out)

Conference: Dec 2–4, 2026 — Bratislava, Slovakia (hybrid)

Publication: Springer LNICST (indexed Web of Science, Scopus, Compendex, DBLP)

Likely track: **sound/image relationships** (one of ArtsIT's 8 special tracks
— maps cleanly onto Whistlegraph's drawing + singing fusion).

## One-sentence version

Whistlegraph is a graphic-score format where a single brush-stroke gesture
records both a drawing and the voice that sang it, making notation, performance,
and circulation the same artifact.

## Spine — single subsystem

Whistlegraph. Not Aesthetic Computer as a whole, not KidLisp, not the OS.
One coherent artifact, one viral form, one set of arts-tech claims.

Drop in via the existing `arxiv-whistlegraph/whistlegraph.tex` as raw material;
this paper is a venue-specific recasting for the sound/image track, not a
re-derivation from zero.

## Keep

- **Brush + voice as one stream:** the gesture and the audio are timestamped
  together; you cannot perform the drawing without producing the score and
  vice versa.
- **The graphic score as a viral form:** whistlegraphs circulate as short
  loops; the score IS the share artifact, not a byproduct of it.
- **Singing as input modality:** voice-driven creative tools as an underused
  channel in HCI, sitting between speech UI and instrument UI.
- **Public corpus:** Whistlegraph has a measurable body of work (the
  whistlegraph-platter) that lets us point at actual scores instead of
  imagined ones.

## Cut or compress

- AC platform overview / cosmology — venue is arts-tech, not systems.
- KidLisp internals — wrong audience.
- Latency / OS material — different paper entirely.
- Dossier / governance / sustainability politics.
- Anything that requires explaining the AC runtime before the artifact.

## Proposed paper shape

1. **Introduction:** notation, performance, and recording usually live in
   three separate artifacts. Whistlegraph collapses them into one stroke.
2. **The artifact:** what a whistlegraph is, how it is captured, what it
   looks/sounds like. Show one before any theory.
3. **Relation to graphic-score traditions:** Cardew, Crumb, Cage — the
   short genealogy of scores-as-drawings, and where Whistlegraph fits.
4. **System (minimal):** the smallest description of the runtime needed to
   make the artifact legible — pen + audio capture + replay. No more.
5. **Circulation:** how whistlegraphs travel; the score as the social object.
6. **Discussion:** what this implies for arts-tech tools that treat output
   and notation as the same thing.
7. **Conclusion:** the move from "tool that produces art" to "form that is
   art and tool simultaneously."

## Abstract seed

A whistlegraph is a short artifact in which a single brush-stroke gesture
records both a drawing and the voice that sang it. Drawing and singing share
one timeline: the stroke cannot be made without producing the score, and the
score cannot be replayed without re-producing the drawing. This paper presents
Whistlegraph as a graphic-score format situated between the open-notation
tradition (Cardew, Crumb, Cage) and contemporary short-form video, and argues
that its viral readability comes from collapsing notation, performance, and
recording into a single shareable form. We describe the minimal capture
runtime, characterise the public corpus of whistlegraphs produced over the
last several years, and discuss what this collapse implies for arts-technology
tools that today still treat creative output and its notation as separate
artifacts.

## Tone

AC papers voice — direct, personal, plain (see `papers/VOICE.md`). Lowercase
the artifact in body prose where natural; capitalize "Whistlegraph" when
naming the form/system. Springer LNICST allows a single-spine arts paper to
read like the artifact itself: short paragraphs, one claim per section, no
platform-pitch register.

## What the platter contributes

- `arxiv-whistlegraph/whistlegraph.tex` — primary source material.
- `papers/whistlegraph-platter/` — corpus + references for the public body
  of work.
- `papers/jeffrey-platter/` — only for biographical sourcing if needed.
- `papers/VOICE.md` — register check before submission.

## Open questions before drafting

- **Anonymous review?** ArtsIT papers are reviewed; confirm whether full
  papers are double-blind. If yes, strip named-author framing from the source
  `whistlegraph.tex` before reuse.
- **Page limit?** Not stated on the landing page — pull from the Springer
  LNICST template once we have it (typically 12–14 pp full / 6 pp short).
- **Special-track submission flow:** does the sound/image track use the same
  EasyChair instance as main? Confirm at submission time.
- **Template:** Springer LNICST `.cls` + `splncs04.bst` — fetch official
  package, do not rewrite AC layout for this venue.
