# 08 · Spatial appendix — the bracelet thesis

**Shelf:** spatial · **Sources:** `blauert-1997`, `mills-1958`, `wallach-1949`, `milne-2017-balance`

A secondary shelf, indexed here rather than assumed. The
[`pop/bracelet/`](../../../pop/bracelet/) lane's whole claim is about **hearing a
rhythm as a place**, so the limits of spatial hearing are load-bearing evidence,
not background.

## The thesis

Rotation and reflection equivalence ([01](01-representation.md)) are asserted on
paper and never heard. In mono, a rhythm and its retrograde are plainly different
objects, yet the classification calls them one bracelet.

Put the cycle in physical space around the listener — **step `i` of `n` at
azimuth `2πi/n`, step 0 dead ahead** — and both equivalences become physical
operations:

- **Rotation** = spinning the sound field, which is also exactly minitek's
  precession lever, now moving in space rather than only in time.
- **Reflection** = mirroring across the median plane: a left/right flip.

E(k,n) stops being a number and becomes **an actual regular k-gon of sound
sources around the head**. A lopsided rhythm is heard as a lopsided room.

The experiment: does the equivalence class survive contact with real spatial
hearing, and where does it break?

## The limits that decide it

**Minimum audible angle (Mills 1958).** About **1° frontally**, degrading to
**10° or worse** toward the sides. At `n = 16` the beads are 22.5° apart, clear
of the lateral figure by a factor of two. **At `n = 32` the spacing is 11.25° —
above the 10° number but not safely above it, since Mills' figure is a floor
that worsens with eccentricity.** So n=32 is marginal, not clean, and n=64 is
hopeless laterally.

This is the ceiling on the mapping law, and the implementation must report
which of the three states it is in rather than silently rendering a ring nobody
can resolve. `minAudibleAngleCheck()` returns `resolvable | marginal |
unresolvable` for exactly this reason.

**The cone of confusion (Blauert).** Front/back and elevation are poorly
resolved compared to left/right; interaural cues are ambiguous along cones of
constant ITD. This is why the mapping puts the cycle in the **horizontal plane
with step 0 ahead**: bracelet reflection is then a *left/right* mirror, which is
the **best-resolved axis available**. Reflecting about the frontal plane instead
would land the whole test inside the ambiguous axis and prove nothing. The choice
of reflection axis is the difference between a real experiment and a null one.

**Precedence effect (Wallach et al. 1949).** Onsets fusing within roughly 1–5 ms
are localised to the first arrival. At 120 BPM a 16th-note step is 125 ms, far
clear of it — but a fast lane or a large `n` can cross the threshold, at which
point two beads stop being two places and become one. Compute it, don't assume
it.

**Balance (Milne).** From [02](02-evenness.md): the balance vector is the centre
of mass of the onsets as unit vectors. Under this mapping it is **literally the
centroid of the sound field**. A perfectly balanced necklace has no net
direction; an unbalanced one pulls to one side of the room. Balance is the single
measure that becomes directly perceptible under spatialisation, which makes it
the lane's primary axis — more than evenness.

## What AC already has

`pop/nullabye/c/ac_hrtf.h` — allocation-free procedural binaural core: fractional
ITD (Woodworth-scale, ~650 µs max), far-ear head shadow, elevation-dependent
pinna notches, compressed inverse-distance. Compiles unchanged to WASM. Not a
measured HRIR dataset, and the lane must say so.

`pop/nullabye/c/spatial-sineabye.c` — listener physics plus a first-person 3D MP4
rasteriser of the sound field. A ring of emitters around the head **is** Toussaint's
circular notation drawn in first person, so the explanatory video is close to free.

## Open question the track answers

Elevation and distance are unassigned. Candidates, in order of preference:

1. **Elevation = off-beatness.** Generator pulses (`gcd(p,n)=1`) lift above the
   plane; polygon-vertex pulses stay at ear level. The one measure that is *not*
   rotation-invariant ([03](03-oddity-depth-interval.md)) becomes the one visible
   axis that moves as the necklace precesses. This is the strongest pairing.
2. **Distance = gap class.** Onsets ending a long gap sit further out.
3. Leave both fixed — the purest test of the azimuth claim alone, and the right
   control condition regardless.

Whichever ships, the control render (fixed elevation, fixed distance) must ship
alongside it, or the claim is unfalsifiable.

## tools

- `necklace_to_positions(rhythm, {radius, elevation_map, plane}) -> {azimuth, elevation, distance}[]`.
- `min_audible_angle_check(n) -> {spacing_deg, resolvable, lateral_warning}`.
- `precedence_check(n, bpm) -> {step_ms, fuses: bool}`.
- `balance_direction(rhythm) -> {angle_rad, magnitude}` — the sound field's
  centroid; the lane's primary steering signal.
- `reflect_spatial(positions, axis = "median")` — the A/B operation.
