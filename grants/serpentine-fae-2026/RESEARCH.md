# Serpentine — Future Art Ecosystems R&D Fellowship: Art × Convergence — Research

> **Deadline:** 2026-06-07, midnight BST
> **Apply at:** https://www.serpentinegalleries.org/whats-on/future-art-ecosystems-rd-fellowship-art-x-convergence/
> **Award:** £10,000 + travel & accommodation for three London weekends
> **Format:** Low-residency hybrid, six months (Sep 2026 – Mar 2027)
> **Time commitment:** ~3.5 hours/week online + three in-person weekend intensives in London
> **Run by:** Serpentine Arts Technologies (Future Art Ecosystems / FAE programme)
> **Selection panel (named):** Refik Anadol, Cao Fei, Holly Herndon, + others spanning art / tech / economics

## What the call is about

**Theme: Art × Convergence.** The brief frames "convergence" as the current
condition where AI systems can pursue goals, model environments, and act in
the world — and asks how that reshapes cultural and societal systems. The
fellowship is explicitly for **creative R&D rather than finished outcomes**;
documentation of process is the expected deliverable.

Eligible: artists, curators, technologists, and organisers working at the
intersection of art and advanced technologies. International applicants
welcome if they can attend the three London weekends.

## Why AC fits — one spine

Jury includes **Holly Herndon**, who has spent years working on voice as a
clonable artistic material (Spawn, Holly+). That maps cleanly to one
specific live thread inside AC:

**The artist as their own voice model.**

The pop/ research lane is actively converting jeffrey's voice from a hosted
service into artist-run infrastructure:

1. **jeffrey-pvc** — a high-fidelity ElevenLabs voice clone keyed to AC's
   `/api/say` route. Used now as the vocal layer in pop/ tracks.
2. **Per-word vocal post-production** — every sung word is sliced and
   re-pitched / elongated / harmonised against the score, with a
   gentle→firm→aggressive aggression knob (recipe lives in each track's
   `.edit.json`). This is where the *composition* happens, not in the model.
3. **Pink Trombone "jeffrey harness"** — the open exploration: fit a Pink
   Trombone vocal-tract model to jeffrey-pvc using the 123/abc minimal
   corpus, then ship a tiny C-runnable jeffrey voice that lives inside AC
   Native OS and removes the cloud dependency entirely.

This is exactly the convergence question, lived in one body: an artist
shaping their own vocal apparatus while AI clones reshape what a voice even
is. The R&D the fellowship would fund is the bridge from (1)+(2) to (3) —
six months is a realistic window to get the tract-fit harness from research
toy to a usable AC instrument.

This is also a paper-able outcome. The pop/ work already touches
`papers/arxiv-folk-songs/`, `papers/arxiv-notepat/`, and would generate a
new dossier on voice + tract synthesis at the end of the six months.

## Why this venue specifically

- **Future Art Ecosystems** as a programme has been the most serious public
  thinking about the *infrastructure* under art × tech (their published
  reports are the closest thing the field has to a shared bibliography on
  the question). AC is, end-to-end, infrastructure — the OS, the runtime,
  the piece API, the social layer, the papermill. A fellowship that funds
  *the practice of building infrastructure* rather than *artworks that use
  infrastructure* is the right shape for what AC actually is.
- **Herndon's presence on the panel** makes the vocal spine legible without
  having to translate; she's already publicly worked the same problem from
  a different starting point.
- **The London weekends** are tractable: three trips over six months from
  LA is a real commitment but not destabilising, and travel + accommodation
  are covered.

## What we would actually do across the six months

Rough shape — to be tightened against whatever the form asks:

- **Months 1–2 (Sep–Oct 2026).** Lock the pop/ vocal pipeline as a stable
  artist-facing tool: per-word edit format, alignment fixes (the open
  melisma drift in `project_vocal_melisma_alignment.md`), publish the
  recipe.
- **Months 2–4 (Oct 2026 – Jan 2027).** Pink Trombone harness — corpus,
  fitting loop, perceptual loss, C runtime. Goal: a "jeffrey voice"
  binary that runs on AC Native OS without internet.
- **Months 4–6 (Jan – Mar 2027).** First works *for* the harness rather
  than *demonstrating* the harness. Songs, monologues, automated readings
  of papers from the papermill. Documentation reel + a written piece for
  FAE.

## Things the panel will likely look at hard

- **Public surface.** The fellowship rewards documented process. AC
  already publishes everything (papers.aesthetic.computer, the platter,
  daily aesthetic-24 recaps). That's a major fit signal.
- **Originality of the question.** "Build my own voice" is unusual
  framing — most artists relate to AI voice as either tool or threat,
  not as a body part to be reclaimed via DSP. Lean into that.
- **Realism of the plan.** Pink Trombone fits to a clone are real
  research, not a guarantee. Be honest about the dead-end risk and
  describe the fallback (richer per-word post-prod, fully documented).

## Open questions for jeffrey

- Do we want to *lead* with the voice spine, or with **AestheticAnts** as
  an artist-run agentic infrastructure (also convergence-shaped)?
  Recommendation: voice. It's more singular and Herndon's presence makes
  it self-explaining.
- London weekends Sep 2026 – Mar 2027 — does that conflict with anything
  fixed in the calendar (NELA Computer Club rhythm, UCLA Social Software
  residency dates, Fia plans)?
- Funding stacking: £10k from Serpentine is fine to combine with other
  grants. Worth checking once we know what else lands.

## Sources

- [Programme page (call)](https://www.serpentinegalleries.org/whats-on/future-art-ecosystems-rd-fellowship-art-x-convergence/)
- [Future Art Ecosystems reports (background context on FAE's
  infrastructure-first posture)](https://futureartecosystems.org/)
- [Serpentine Arts Technologies](https://www.serpentinegalleries.org/arts-technologies/)

## Next steps

1. Open the application form, capture the exact field structure + word
   counts.
2. Trim `DRAFT.md` to fit the form.
3. Pull two media samples: a pop/ track demonstrating current jeffrey-pvc
   + per-word post pipeline, and a Pink Trombone harness sketch (even if
   rough).
4. Decide spine framing once and stick with it (don't buffet — see
   `feedback_proposals_single_spine.md`).
5. Submit before 2026-06-07, midnight BST.
