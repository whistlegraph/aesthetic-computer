# bracelet — spatial necklaces

**A rhythm you hear as a place.** ~1:49, 138 BPM, A minor, binaural.
**Headphones required** — on speakers it collapses to mono and the track is gone.

```bash
node pop/bracelet/c/render.mjs           # → out/bracelet.mp3
node pop/bracelet/c/render.mjs --play    # and open it
```

## The thesis

In Toussaint's taxonomy two rhythms are the same **necklace** if one is a
rotation of the other, and the same **bracelet** if one is a rotation *or
reflection*. Both equivalences are asserted on paper and never heard: in mono, a
rhythm and its retrograde are plainly different objects.

Put the cycle in physical space around the listener — step *i* of *n* at azimuth
2πi/n, step 0 dead ahead, in the horizontal plane — and both become physical
operations:

- **rotation** → the sound field spins. Precession in time and rotation in space
  become the *same operation*.
- **reflection** → a left/right mirror across the median plane.

E(k,n) stops being a number and becomes an actual regular *k*-gon of sound
sources around your head. A lopsided rhythm is heard as a lopsided room.

The horizontal plane is not a stylistic choice. Left/right is the best-resolved
axis humans have; front/back and elevation sit inside the cone of confusion.
Reflecting about the frontal plane instead would put the whole experiment in the
ambiguous axis and prove nothing.

## What the platter turned up, and what it cost

Building this against [`papers/rhythm-platter/`](../../papers/rhythm-platter/)
produced one finding that rewrote the form:

**Most canonical timelines are achiral.** son, bossa, shiko, tresillo, cinquillo
and bembé are each their own reflection up to rotation — mirroring them only
*turns* the ring. Only **rumba, soukous and gahu** are chiral.

And chirality alone is not enough. The ring must also carry its balance centroid
**off the mirror axis**, or the flip changes the pattern without moving the
field. soukous sits at 180°, dead on the axis. So the mirror section uses
**gahu**: chiral, centroid at +17.8° — nearly frontal, where the minimum audible
angle is ~1° — and the strongest pull of any candidate (0.710). The flip swings
it to −17.8°, a 35° sweep across the best-resolved part of the field.

Picking soukous, the obvious choice, would have been a null test.

## Form

| section | bars | ring | what it is for |
|---|---|---|---|
| still | 8 | bossa | state the polygon: maximally even, perfectly placed, not turning |
| turn | 12 | bossa | rotation = the room spins; centroid walks 0° → 90° → 180° |
| mirror | 8 | **gahu** | the bracelet A/B, two bars on / two bars flipped |
| weave | 12 | soukous | second ring = the complement, so no two beads share an azimuth |
| morph | 13 | clump → bossa | the swap geodesic, one onset-move per bar |
| land | 8 | bossa | rotation stops on the balanced set; the centroid resolves |

The **morph** starts from a deliberate non-timeline: five onsets crammed into the
first half, gaps (2,2,2,2,8) — a badly lopsided pentagon heard as a clump in the
front-right with the whole left side silent. Twelve moves later it is the regular
pentagon. You hear the polygon become regular. The canonical claves are too close
together to morph between (son → bossa is a single swap), which is itself
Toussaint's point about how tightly that family clusters.

## Build

Rhythm from [`pop/lib/c/ac_necklace.h`](../lib/c/ac_necklace.h), binaural from
[`pop/nullabye/c/ac_hrtf.h`](../nullabye/c/ac_hrtf.h). The engine prints its own
claims — spacing, precedence margin, per-ring chirality, evenness, off-beatness,
and the centroid bar by bar — to stderr at render time.

**Elevation carries off-beatness**: generator pulses (gcd(p,16)=1) lift +22.5°
above the plane. That is the one measure rotation changes, so it is the axis
that visibly moves as the ring precesses.

**The centre bus is never spatialised.** Kick and sub live below ~150 Hz where
localisation is weak, so placing them in the ring would be a claim the ear cannot
check. They are the fixed floor everything else turns against.

**Ring voices open with a broadband transient.** HRTF cues live in spectral
notches and interaural timing; a pure tone gives the ear almost nothing to
localise. The chiff is what makes a bead a place.

## Verification

Per-strike measurement of the `still` section, ILD only (ITD is the stronger cue
and does not appear in RMS):

| step | azimuth | R−L |
|---|---|---|
| 0 | ahead | −0.4 dB |
| 3 | +67.5° | +4.60 dB |
| 6 | +135° | +2.55 dB |
| 10 | −135° | −2.34 dB |
| 13 | −67.5° | −4.09 dB |

Symmetric and correctly signed. Note that the *whole-bar* average is near mono by
construction — a complete ring sweeps every azimuth, so it cancels. Measuring the
bar average is the wrong test and will make a working spatialiser look broken.

## Mastering

Deliberately **not** the minitek club chain. The left/right differences are the
content, so: no stereo widening, no mid-side EQ, channel-linked compression only,
and −14 LUFS rather than −9 to keep transient cues intact. See
[`c/render.mjs`](c/render.mjs).

## Limitations

`ac_hrtf.h` is a procedural binaural model — ITD, head shadow, elevation-dependent
pinna combs — not a personalised measured HRIR set. Localisation is real but
generic, front/back confusions are expected, and results vary by listener and by
headphone. The lane's claim is that the equivalence classes become *audible
operations*, not that the imaging is accurate.

Status: first cut rendered 2026-08-07.
