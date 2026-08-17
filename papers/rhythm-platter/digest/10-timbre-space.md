# 10 · Timbre space as a control structure

**Shelf:** timbre · **Sources:** `wessel-1979-timbre-space`

Secondary shelf, like [08](08-spatial-appendix.md): not rhythm, but the same
move the whole platter is built on — subjective judgments pressed into a
geometry, then the geometry used as a compositional lever. Wessel is the
canonical statement of that move for timbre, and it predates every rhythm
application here.

## The construction

1. Collect pairwise **dissimilarity judgments** over a tone set (Wessel: 24
   orchestral tones equalized for pitch, loudness, duration). No attribute
   naming required — "A is more like B than C" is the whole input.
2. Multidimensional scaling (KYST) embeds the tones as points so that distance
   reproduces judged dissimilarity.
3. The axes are then given a **psychoacoustic reading**: one dimension tracks
   brightness (spectral energy distribution), the other the "bite" of the
   attack. The interpretation comes *after* the geometry, from correlating
   coordinates with acoustic measures — the same discipline as reading
   evenness out of a necklace polygon rather than asserting it.

## Space → control

The paper's actual thesis is the platter's thesis: a perceptual geometry is
only worth having if you can *drive* it.

- **Synthesis handle.** Additive synthesis with line-segment envelope
  approximations (5–7 breakpoints per envelope suffice — Grey 1975 showed the
  approximations are nearly indiscriminable from full detail) makes each tone
  a small, editable data object; moving through the space becomes moving
  breakpoints. Low-dimensional control over high-dimensional synthesis data.
- **Timbral analogies.** A parallelogram model (Ehresman & Wessel 1978):
  A→B as C→D means D completes the parallelogram in the space. Listener
  rankings of candidate D's track distance from the ideal point — vector
  arithmetic in the space predicts musical judgment. This is "transposition"
  defined for timbre.
- **Stream segregation as acceptance test.** Alternate two timbres on a
  repeating pitch pattern: small timbral distance, one stream; large distance,
  the line splits in two (the Wessel illusion). Perceptual distance in the
  space has audible, binary consequences — a falsifiable check, like the
  platter's rule that a computed rhythm distance must predict something a
  listener can hear.

## Why it sits on this platter

The rhythm shelves geometrize *when*; Wessel geometrizes *what*. Any AC lane
that sequences instrument changes — notepat's GM palette, `pop` engines
swapping voices per phrase — is doing note-to-note timbre manipulation, which
is precisely the compositional situation the paper was written for. Chronotonic
distance ([05](05-distance-similarity.md)) and timbre-space distance are the
two levers a phrase-morphing tool would pull together.

Limitations to carry: the space is stimulus-set-relative (24 tones in, that
geometry out — add tones and the map can warp); dimensions are interpretations,
not measurements (Wessel is explicitly pessimistic about subjective *units*);
and a 1979 2-D solution is not a universal timbre coordinate system.

## Provenance

Platter copy (JSTOR scan of *Computer Music Journal* 3(2), pp. 45–52) shared
by Sage Jenson, 2026-08-15 — hand-placed in `sources/`, not fetched, since no
open-access URL currently resolves. Cite the journal, not the scan.

## Status

[10a](10a-timbre-space-in-ac.md) audits this shelf against AC, notepat, Menu
Band and TrackDrum, and builds the tools below in their acoustic-correlate
form — measured axes, no listeners, no MDS. Read it for what the fleet already
does with the paper, and for the limits the measurements carry.

## tools

- `timbre_space(tones, judgments) -> points[]` — MDS embed; judgments may come
  from a model proxy instead of listeners, but then say so.
- `timbre_distance(a, b)` — distance in the embedding; the morphing lever.
- `timbre_analogy(a, b, c) -> d` — parallelogram completion over the embedding.
- `stream_split_risk(seq) -> warnings[]` — flag alternations whose timbral
  distance predicts segregation, before a lane renders an unintended two-voice
  texture (or use it on purpose).
- Envelope objects stay breakpoint lists (5–7 segments), never dense curves —
  the data-reduction result is load-bearing for real-time control.
