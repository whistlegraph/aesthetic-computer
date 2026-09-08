# Composition Model — *Note(s)pat(ial) Native*

> How the fixed 1:41 of *Special Sign* becomes an 8–12 minute live
> spatial performance on six salvaged laptops. Companion to the runtime
> (`fedac/native/pieces/notespatial.mjs`, score data
> `special-sign.nsscore`). A model to rehearse against, not a playback
> plan — the conductor's holds decide each night's true length.

**Source:** *Special Sign* release master, 101.4s, 13 lanes, six
movements, spatial-rotation ribbon (peaks at 16.9s, 27.8s, 47.7s,
70.6s, 97.4s — one per movement door, roughly).
**Target:** 8–12 minutes. **Forces:** six machines in a ring, one
conductor (seventh machine or one of the six), audience inside.

---

## The core reading

The expansion mechanic is **dwelling, not stretching**: each movement
is a holdable region that loops until released. What keeps a held
movement alive is that *every pass re-seats the sound* — the movement
subtitles in the score data are already spatial verbs, and the live
version performs them literally with machines instead of a stereo
field:

| Movement | Source | Sub (verbatim) | Spatial choreography | Passes | Dwell |
|---|---|---|---|---|---|
| I · Assembly | 18.9s · lvl .38 | bodies enter high to low | **Enter.** Pass 1: the whole score sounds from ONE machine — the piece begins as a point. Each pass spreads lanes outward (1 → 3 → 6 machines alive). | 3–4 | ~60–90s |
| II · Signal | 20.6s · lvl .58 | question and answer | **Antiphony.** Lanes split into question/answer halves seated across the ring; the question walks one seat clockwise per pass. | 3–4 | ~60–90s |
| III · Super-Spin | 16.0s · lvl 1.0 | eight listener-relative turns | **The showpiece.** Assignments orbit the ring — one seat per bar, doubling each pass until the field blurs. Longest dwell; the audience sits inside the turn. | 4–6 | ~90–120s |
| IV · Constellation | 19.8s · lvl .82 | counterpoint and sine garden | **Scatter.** Maximal dispersion: one lane per machine, extras floating; rate bends down (×0.7–0.85) so the garden opens. Densest material (echo 121, pads 70/71) carries slowness well. | 3–4 | ~75–100s |
| V · Home Sign | 20.0s · lvl .62 | G7 resolves to C major | **Converge.** Reverse of Assembly: each pass pulls lanes toward one home machine; the resolution lands with every voice in one body. | 2–3 | ~45–70s |
| VI · Run-Down | 6.0s · lvl .28 | the physical field reaches rest | **Rest.** Once, plain, from the home machine alone. Then room silence. | 1 | ~10s |

Passes alone total ≈ 5.5–8 minutes. The remaining time is **fermatas at
the movement doors**: the conductor may hold a door — the last sonority
loops thin as a drone — while the machines re-seat for the next
choreography. Five doors × 10–30s of held breath lands the night inside
8–12 minutes without ever slowing the material into sludge.

## Per-pass variation (why repetition reads as composition)

1. **Seating** — the choreography above; no two passes share an
   assignment map.
2. **Level** — first pass of each movement enters at ~0.7 gain, opens to
   full; the score's own movement `level` arc (.38 → 1.0 → .28) stays
   the macro shape of the night.
3. **Rate** — bounded per movement: I rubato (0.85–1), III may
   accelerate through its final pass (→ ~1.15), IV slow (0.7–0.85),
   VI dying (~0.8). Rate rebases continuously — no clicks.
4. **The rotation ribbon** — the composed spin envelope from the
   recording drives default orbit speed; the conductor overrides it
   live.

## Roles of the lane families in the room

- **bass + boom** — the floor; prefers a single seat (subwoofer-ish
  laptop), moves rarely.
- **pad-l / pad-r, hat-l / hat-r, nose-l / nose-r** — composed stereo
  pairs → **opposite seats**; their l/r identity becomes ring geometry.
- **melody + jeffrey vowels** — the sign itself; the voice the
  conductor moves by hand, the one the audience tracks.
- **echo + gong** — the resonant halo; happiest dispersed.
- **air** — weather; wherever nothing else is.

## Runtime hooks (already live)

`/pieces/notespatial-state.txt` (PUT over LAN, polled ~1Hz):
`lanes` (assignment), `rate`, `seek`, `hold`. On-device: space toggles
hold, enter jumps to the next movement door. Solo rehearsal = one box
playing all lanes, conducting itself.

## Honest risks

1. **~1s state-poll skew** is the current sync floor — fine for
   assignments and holds, too coarse for beat-locked antiphony. If II's
   Q&A needs tighter handoff, the UDP lane is the upgrade path.
2. **Dwell fatigue** — a held movement must earn its passes; if a
   choreography reads static by pass 3, cut the dwell, not the tempo.
3. **Six-box audio latency spread** is unmeasured until the LA fleet
   stages; the model assumes seat-level, not sample-level, coherence.
