# 04 · Syncopation and complexity measures

**Shelf:** syncopation · **Sources:** `longuet-higgins-lee-1984`, `povel-essens-1985`, `keith-1991`, `toussaint-2005-offbeatness`

Off-beatness ([03](03-oddity-depth-interval.md)) is a cheap *static* proxy —
it counts onsets on group-generator pulses and ignores the meter's hierarchy
entirely. `pop/minitek/c/dubtek.c` labels it "a syncopation proxy," which is the
correct claim. These are the measures that actually model syncopation.

## Metric hierarchy

All of them presuppose a **metric weight per pulse**: a tree of subdivisions
where the downbeat is strongest, the half is next, then quarters, and so on
(Lerdahl & Jackendoff's grid). For `n = 16` the standard weights descend
`0 > 8 > {4,12} > {2,6,10,14} > odd`. Syncopation is then some measure of onsets
landing where the hierarchy says they should not.

The hierarchy is an **input**, not a derived fact — it encodes a metric
interpretation, and a different interpretation gives different numbers for the
same onsets. Any implementation must take the weights as a parameter with the
binary-subdivision default made explicit.

## LHL (Longuet-Higgins & Lee 1984)

The most-used measure. For each note followed by a rest, if the rest sits at a
**stronger** metric level than the note that precedes it, that pair contributes
the difference in levels. Sum over pairs. Higher = more syncopated.

Captures the core intuition directly: syncopation is a strong position left
*empty* by a note that arrived early.

## Povel & Essens (1985) — the C-score

Model-based rather than counting: induce the best-fitting **clock** (a periodic
pulse train) by scoring how much counterevidence each candidate clock accumulates
against the pattern's accent structure. The C-score is the counterevidence of the
best clock. High C = no clock fits well = complex.

Different in kind from LHL — it measures *how hard the pattern is to hold onto*,
not how far it departs from a fixed grid. Worth having both, since they disagree
in interesting places.

## Keith (1991)

Classifies each onset as a hesitation, anticipation, or syncopation relative to
the metric grid and sums a weighted count. Simpler than LHL and easy to compute;
Keith's book is also the source of the Pólya-counting machinery used for
enumerating necklaces in [01](01-representation.md).

## WNBD

Weighted note-to-beat distance: for each onset, its distance to the nearest beat,
weighted. Continuous rather than level-based, so it degrades gracefully with
microtiming and is the only measure here that survives leaving the grid.

## What to conclude

There is no single correct syncopation number. The measures agree on extremes and
diverge in the middle, which is precisely the region music lives in. **A rhythm
should carry a vector of measures, not a scalar** — and any claim the `bracelet`
track makes about "more syncopated" must name which measure it means.

## tools

- `metric_weights(n, subdivision = 2) -> float[]` — explicit, parameterised.
- `syncopation_lhl(rhythm, weights) -> float`.
- `syncopation_povel_essens(rhythm) -> {c_score, best_clock}`.
- `syncopation_keith(rhythm, weights) -> float`.
- `wnbd(rhythm, beats) -> float`.
- `syncopation_profile(rhythm) -> {lhl, keith, c_score, wnbd, offbeatness}` —
  the vector; the thing engines should print rather than a single number.
