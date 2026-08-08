# 05 · Rhythmic distance and similarity

**Shelf:** distance · **Sources:** `toussaint-2004-similarity`, `demaine-2009-distance-geometry`, `toussaint-2003-ternary`

This is the shelf that buys a lever AC does not have: a **metric on rhythms**,
and therefore a **path from one timeline to another**.

## The measures

**Hamming.** Number of positions where two binary strings differ. `O(n)` and
wrong for rhythm: it is blind to *how far* a displaced onset moved. An onset one
pulse late scores the same as one seven pulses late.

**Swap.** The minimum number of adjacent swaps converting one rhythm into the
other. For rhythms with equal `k`, this equals the sum of absolute differences
between the two sorted onset lists — so it is `O(k)` once sorted, not a search.
Directly musical: it counts *how far the onsets had to move*.

**Directed swap.** The generalisation for unequal `k`, where onsets must be
many-to-one matched. Needed the moment two rhythms of different density are
compared.

**Chronotonic.** Draw the rhythm as a step function over the `n` atomic pulses,
where each pulse's height is the length of the IOI it falls inside. Son clave
(3,3,4,2,4) becomes the 16-vector `(3,3,3, 3,3,3, 4,4,4,4, 2,2, 4,4,4,4)`. The
distance is the **area between two such curves** — Toussaint uses the Kolmogorov
variational distance, `K = ∫|f₁ − f₂|`, an L1 area.

The chronotonic curve encodes *duration context* at every pulse, not just onset
positions, which is why it separates rhythms the positional measures collapse.

**Interval-difference.** L1 or L2 between full interval vectors. Blind to
homometric pairs by construction ([03](03-oddity-depth-interval.md)).

## The verdict

Toussaint (2004) compares five measures against five criteria and concludes:

> the best overall rhythmic dissimilarity measure is the chronotonic distance,
> followed by the swap distance in close second place.

So `pop/lib` **defaults to chronotonic, offers swap**, and provides Hamming only
for completeness with a docstring saying not to use it. Choosing Hamming because
it is easiest to write is the failure mode this note exists to prevent.

## Cyclic distance

Every measure above is defined on a *fixed rotation*. Rhythms are cyclic, so the
musically correct distance between two **necklaces** is the minimum over all `n`
rotations of one against the other. That minimising rotation is itself useful
output — it is the phase alignment at which the two patterns are most alike, and
therefore a defensible place to cut between them.

Implement `dist(a, b, {cyclic: true})` returning `{distance, best_rotation}`.

## Phylogeny

Toussaint (2003) builds phylogenetic trees over these distances for African
ternary timelines, recovering plausible family structure — which is the evidence
that the metric tracks something real rather than being an arbitrary norm.

## Morphing — the compositional lever

A metric space admits paths. Given son and rumba (which differ by a single
one-pulse displacement) the swap-distance geodesic is a one-step move; given son
and bembé on different cycle lengths it is a longer chain through intermediate
necklaces.

**A morph is an arrangement device AC has never had.** Instead of cutting between
two timelines at a section boundary, walk the shortest path between them one
onset-move per bar, so the groove *becomes* the other groove while you listen.
`pop/minitek` precesses a fixed necklace; this changes which necklace it is,
continuously, along a measured path. That is the `bracelet` lane's second
movement.

## tools

- `dist_hamming(a, b)` — present, documented as inappropriate.
- `dist_swap(a, b)`, `dist_directed_swap(a, b)`.
- `dist_chronotonic(a, b)` — **the default.** Via the step-function L1 area.
- `dist_interval(a, b)`.
- `chronotonic_vector(rhythm) -> int[n]` — reusable for plotting.
- `dist(a, b, {measure, cyclic}) -> {distance, best_rotation}`.
- `morph_path(a, b, {measure}) -> rhythm[]` — the geodesic, one move per step.
- `phylo_tree(rhythms, {measure}) -> tree` — for platter figures, not for tracks.
