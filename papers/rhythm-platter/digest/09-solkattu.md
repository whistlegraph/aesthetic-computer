# 09 · Solkattu: spoken rhythm as a representation

**Shelf:** solkattu · **Sources:** `levin-2011-indian-rhythms`

## What the source settles

South Indian rhythmic solfège (solkattu, spoken as konnakol) assigns a fixed
syllable string to each group size. The canonical ladder, as taught in the
source (on-screen overlays OCR'd to `sources/levin-2011-indian-rhythms.ocr.txt`,
speech in `.captions.txt`):

| k | Syllables |
|---|---|
| 1 | Ta |
| 2 | Ta Ka |
| 3 | Ta Ki Ta |
| 4 | Ta Ka Di Mi |
| 5 | Ta Din Gi Na Tom |
| 6 | Ta Ki Ta Ta Ki Ta |
| 7 | Ta Ka Di Mi Ta Ki Ta |
| 8 | Ta Ka Di Mi Ta Ka Jo No |

Two structural facts ride along:

- **Composites are literal.** 6 = 3+3 and 7 = 4+3 *in the syllables themselves*
  — the string for a larger group is built by concatenating smaller groups, so
  the decomposition is audible, not annotated.
- **Tom is a boundary marker.** The overlay glosses it as "syllable marking the
  end of a phrase" — the system carries an explicit phrase-delimiter, not just
  duration content.

## Two modes, two formalisms

The source demonstrates the same groupings driving two different clocks, and
the distinction matters for code:

1. **Concatenative (pulse-fixed).** Every syllable = one pulse (one 16th).
   Groups chain into additive meters: 3+5+7 = 15/16, 5+5+7 = 17/16, and
   5+3+3+5 = 16/16 — a palindromic carve-up of plain 4/4. This is exactly the
   platter's IOI world ([01](01-representation.md)): a phrase is an IOI
   sequence whose sum is `n`, with syllables naming the intervals.
2. **Proportional (beat-fixed).** Every *group* = one beat; density varies:
   Ta = quarter, Ta Ka = 8ths, Ta Ki Ta = 8th-note triplets, Ta Ka Di Mi =
   16ths, Ta Din Gi Na Tom = quintuplets, up to 32nds — a tuplet ladder that
   the source runs up and back down as an accelerating/decelerating exercise.
   This is *not* an IOI structure on a shared grid; it is a per-beat
   subdivision index.

A rhythm engine that conflates these two modes will be wrong in one of them.
The concatenative mode composes onto one grid; the proportional mode changes
the grid per beat.

## Why it matters here

The platter's geometry is silent about *how rhythms are carried by people* —
the perception shelf ([07](07-perception-groove.md)) already flags that gap.
Solkattu is a counterexample worth keeping in view: a fully oral, named,
compositional rhythm representation with no notation dependency, where evenness
and grouping are properties of a syllable string. It is also the natural
labeling layer for any AC tool that wants to *speak* a rhythm rather than
only flash it.

Source register: the video is a pedagogical demonstration by a guitarist, not
a Carnatic primary source — syllable spellings vary across traditions
(takadimi / ta ka dhi mi, etc.). Treat the table as *a* canonical form, cited
to this source, not *the* canonical form.

## tools

- `solkattu(k) -> string[]` — the syllable string for a group of `k` pulses
  (table above), for labeling and vocalized playback.
- `phrase_meter(groups: int[]) -> {n, ioi}` — concatenative mode: `[3,5,7]` →
  15 pulses with IOI-boundary accents; feeds the same onset machinery as
  [01](01-representation.md).
- `tuplet_ladder(k, beat_ms) -> events[]` — proportional mode: one beat carved
  into `k` equal syllables; the ladder exercise is `map(1..8)` ascending then
  descending.
- Phrase delimiters (`Tom`) map to the accent layer, never to onset timing.
