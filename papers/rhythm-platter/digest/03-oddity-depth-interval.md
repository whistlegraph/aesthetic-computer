# 03 · Rhythmic oddity, deepness, interval content

**Shelf:** oddity-depth · **Sources:** `arom-1991`, `demaine-2009-distance-geometry`, `toussaint-2006-interlocking`, `toussaint-2005-offbeatness`, `pressing-1983`

## Rhythmic oddity (Arom)

A rhythm has the **rhythmic oddity property** when **no two of its onsets divide
the cycle into two halves of equal length** — i.e. no pair of onsets is exactly
`n/2` apart.

Only meaningful for even `n`. Arom identified it in Central African Aka
repertoire; Toussaint shows the Cuban claves are the canonical oddity timelines.
`pop/minitek/c/dubtek.c` already measures it and found the bembé E(7,12) on the
12-pulse grid is the sole failure among its five candidates.

Implementation is one pass over onset pairs, but the function must **return
undefined (not `false`) for odd `n`** — a rhythm on an odd cycle cannot have two
onsets `n/2` apart, so reporting `true` would be vacuous and misleading.

## Interval content

The **full interval vector** is the histogram of geodesic (shorter-way-round)
distances between all `C(k,2)` pairs of onsets. Every distance lies in
`[0, floor(n/2)]`. This single object underlies deepness, homometry, and the
hexachordal theorem.

Note this is *not* the IOI sequence — IOIs are adjacent gaps only. Both are
needed and they are easy to confuse.

## Deepness — two definitions, both required

Demaine et al. (2009) are precise about a distinction usually collapsed:

- **Winograd-deep**: every distance `1, 2, …, floor(n/2)` has a **unique
  multiplicity**. Equivalently (the Common Tone Theorem) the number of onsets
  shared with each of its own rotations is unique.
- **Erdős-deep**: for **every multiplicity `1, 2, …, k−1`** there is some
  distance occurring exactly that many times.

Every Winograd-deep rhythm is Erdős-deep; the converse fails. Erdős-deep is
strictly more general.

The Western diatonic major scale is Winograd-deep, and — via Pressing's
pitch/rhythm duality — **is the same object as the bembé bell**. That equivalence
is why scale theory imports wholesale into rhythm and why this platter can borrow
from pitch-class set theory without apology.

## Shelling — an arrangement lever

A **shelling** of an Erdős-deep rhythm is an ordering of its onsets such that
removing them one at a time leaves an Erdős-deep rhythm at every step.

This is the find of the section. It is a *principled way to thin a pattern* —
a breakdown that strips onsets one per bar while the rhythm keeps its structural
property the whole way down, and a build that reverses it. AC arrangements
currently thin by muting whatever seems least important. A shelling is the
sourced alternative.

## Homometry and the hexachordal theorem

Two rhythms are **homometric** (Z-related, in pitch-class terms) when they have
identical interval vectors but are **different necklaces**. They are formally
indistinguishable by interval content yet audibly distinct — a natural A/B pair
for any listening test that wants to isolate what interval content does *not*
capture.

The **hexachordal theorem**: when `k = n/2`, a rhythm and its complement have the
same interval vector. Toussaint (2006) works this through for interlocking
rhythms — see [06](06-complements-canons.md).

## Off-beatness

Sourced properly at last. Toussaint (2005, Bath) defines the off-beat positions
of an `n`-cycle as those **not reachable by any regular polygon inscribed in the
cycle** — equivalently, the **generators of the cyclic group C(n)**, the `p` with
`gcd(p, n) = 1`. For `n = 12` these are `{1, 5, 7, 11}`. Off-beatness is the
count of onsets on such positions; among ten West African 12/8 bell patterns the
bembé uniquely attains the maximum of 3.

Two implementation consequences:

1. It is a **static property of the meter**, not of a performance, and it is the
   one measure here that is **not rotation-invariant**. Rotating a necklace
   changes its off-beatness. That makes it the natural axis to vary when
   precessing a pattern — the property that actually moves.
2. It **degenerates for prime `n`**, where every position from `1` to `p−1` is a
   generator. The function must flag this rather than return a number that looks
   meaningful.

## tools

- `has_rhythmic_oddity(rhythm) -> bool | undefined` — undefined for odd `n`.
- `interval_vector(rhythm) -> int[]` — full pairwise geodesic histogram.
- `is_winograd_deep(rhythm) -> bool`, `is_erdos_deep(rhythm) -> bool`.
- `shelling(rhythm) -> onset_order | null` — **the thinning lever.**
- `homometric_pairs(n, k) -> [[a, b], …]` — distinct necklaces, equal vectors.
- `offbeatness(rhythm) -> {count, positions, degenerate: bool}`.
- `generators(n) -> int[]` — the off-beat positions of the meter.
