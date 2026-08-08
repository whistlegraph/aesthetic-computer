# 06 · Complements, interlocking, tiling canons

**Shelf:** canons · **Sources:** `toussaint-2006-interlocking`, `hall-klingsberg-2006`, `amiot-2016-fourier`

## Complement

The **complement** of a rhythm is the set of its silent pulses. Trivial to
compute and structurally rich: a rhythm and its complement together tile the
cycle exactly once, which is the simplest possible interlock.

## The hexachordal theorem

When `k = n/2`, a rhythm and its complement have **identical interval vectors**.
Two patterns that are the photographic negative of each other are indistinguishable
by interval content.

Musically this is the formal licence for the oldest trick in ensemble
percussion: give one player a pattern and the other the holes, and the pair
sounds like one interlocked line rather than two competing ones. Toussaint (2006)
works it through under the name *interlocking rhythms*.

## Tiling rhythmic canons

A stronger condition. A rhythm `A` and a set of translations `T` form a **tiling
canon** when every pulse of the cycle is struck **exactly once** by exactly one
translate: `A ⊕ T = Z_n`, a direct sum with no collisions and no gaps.

The same voice, entering at several offsets, covering the cycle perfectly — a
canon in the strict sense, in rhythm rather than pitch.

**Vuza canons** are the hard case: tilings where neither `A` nor `T` is periodic.
They exist only for certain `n` (the smallest is 72), which puts them outside the
16- and 12-pulse cycles the pop lanes use. Worth knowing about, not worth
implementing yet.

Hall & Klingsberg (2006) count rhythmic-oddity ("asymmetric") necklaces and
connect that count to tiling canons — the bridge between
[03](03-oddity-depth-interval.md) and this shelf.

Amiot's Fourier machinery gives the practical test: tiling is a condition on
vanishing DFT coefficients, so the `dft` backend from
[02](02-evenness.md) already computes most of what a checker needs.

## Why the `bracelet` lane needs this

The spatial mapping puts each onset at its own azimuth around the listener. Two
rings sounding **the same azimuth at the same instant** collapse into one image
and the geometry stops being audible.

Complementary and tiling rhythms are the sourced answer: choose the second ring
to be the complement (or a tiling translate) of the first, and **no two beads
ever strike the same point in space at the same time** by construction. The
interlock is not mixed by ear — it is guaranteed combinatorially.

That makes this shelf load-bearing for the track rather than ornamental.

## tools

- `complement(rhythm) -> rhythm`.
- `interlocks(a, b) -> bool` — no shared onsets.
- `same_interval_vector(a, b) -> bool` — hexachordal check for `k = n/2`.
- `tiling_partners(rhythm, n) -> translations[]` — offsets forming a tiling.
- `is_tiling_canon(rhythm, translations) -> bool`.
- `is_periodic(rhythm) -> bool` — the precondition Vuza cases negate.
