# Chamber study 05: inside the ring — an audit of the eleven chapters, September 23

The reading is `papers/chamber-platter/digest/05-hearing-inside-the-ring.md`; this
file applies it to `fedac/native/tools/compose-notespatial-native.mjs` the day
before the CultureHub premiere. Three facts drove the audit:

1. **A laptop does not pass the bass.** The ring's speakers roll off under
   about 200 Hz. The score's bass roots run 87 to 147 Hz, the Sneak's creep 98
   and 131 Hz, its last low C 65 Hz, the kick's body 78 Hz. On a laptop these
   were being heard, if at all, through the triangle's faint third harmonic.
2. **Nobody is at the center.** Every listener is 1 to 5 m from any given
   laptop, so a sound crossfaded between two seats arrives from the nearer one
   first (11.7 ms across a 4 m difference, well past the 1 to 5 ms fusion
   window for a pluck) and 14 dB louder. The equal-power orbit the runtime
   renders is, for the audience, a jump whose moment depends on the seat.
3. **A sine without a transient cannot be placed** (Hartmann 1983: chance for
   a 500 Hz sine in a room). The echoes and the travelling theme were single
   sines below 1.1 kHz, so the two gestures that are *about* place carried the
   fewest cues for it.

## What changed in the composer

| Rule | Change | Where |
|---|---|---|
| R1 | `bassNote()`: triangle root plus sine octave (0.5) and twelfth (0.28), same attack, shorter tails. Used for the phrase bass, the Waltz oom and tag, the Fanfare bass and stinger, the final chords, the Sneak's creep and its last low C. | all chapters with bass |
| R1 | `kick()`: 30 ms click at 2.5 kHz, the 150 Hz thump, the 78 Hz body, and the body's second harmonic at 156 Hz. | VIII |
| R2 | `soft()` echoes gain a 2 ms octave at 0.22 so the answer has energy above 1.5 kHz. | II, VI, VII, VIII, X |
| R2 | The travelling theme gains the same octave (0.25). | VIII |
| R2 | The Sneak's creep: every step carries a 30 ms tick at 0.1 and the bar tick rises from 0.12 to 0.2 and 30 to 40 ms. | V |
| R3 | The theme and the answer **hop** instead of gliding: five pinned lanes each; a note is written whole to the seat nearest its orbit angle at onset (period 12.5 s clockwise, 15 s counter). `hopOrigin` is the Lift's downbeat, so the theme leaves the hands at seat 1 and walks 1, 2, 3, 4, 5, one seat every 2.5 s. `--glide` rebuilds the old orbiting lanes. | VIII |
| R4 | While the theme travels, the walk hops only to the two seats across from it (144° or more). | VIII |
| R5 | The Chase's runners hold each seat two eighths once an eighth is under 200 ms (168 and 184 and 200 BPM); the notes are unchanged. | IV |

Verified with `notespatial-native-check.mjs`: 10,185 of 10,185 events voice
(a first pass dropped 105 clicks shorter than the 25 ms frame, hence the 30 ms
floor in R2), 32 lanes, peak polyphony 26 of 32 in the Lift (was 20), routing
map unchanged in shape. Length still 12:54. `spatial-rehearsal.test.mjs`
passes; the routing library was not touched.

## What did not change, and why

- **Pads, top line, hats still glide.** Pads are wash (600 ms attack) and
  carry no place; hats are noise, which Féron shows follows rotation best; the
  top line is soft and slow (24 s). Gliding these costs nothing.
- **The eight-turn spin and the blast** still turn the whole field through
  `fieldShift`. The spin is the one blur used as cadence (digest 02, R7); for
  an off-center listener it will read as level flutter around the ring, which
  is the hum it is meant to be.
- **The Sneak's low C (65 Hz) stays at midi 36** with its residue stack rather
  than moving up an octave: the seat-4 laptop will give its 130 and 195 Hz
  partials, and the pitch will still read as the low C.
- **The chromatic pile-up in the Chase** still hops per eighth at 200 BPM: it
  is a pile-up, texture is the point.

## What is left to the tech and the performer

1. **Center level (R8).** Set the small speaker from the far side of the ring,
   not from the hands: a listener 1 m from a ring laptop and 3 m from the
   center needs the center about 9.5 dB hotter for the bell and the pluck to
   match. Set it during the Overture's first bell against seat 1's first pluck,
   standing at seat 3 or 4.
2. **Walk (R8, Cardiff).** The Waltz's A sections, the Sneak's tiptoe and the
   Lullaby's tune are center-only for more than a phrase. Walk slowly toward
   the rear pair (3 and 4) in each; return to the middle for the Lift's
   downbeat so the theme audibly *leaves* the hands for seat 1.
3. **Drift (R6).** Six free-running clocks at 20 to 50 ppm are 15 to 39 ms
   apart by the Vanish. Plucked tuttis stay one event under about 40 ms and one
   point under 5 ms. Measure at the tech with `beeps`; if the spread passes
   20 ms by the Lift, cue chapters VIII and X from their own files.
4. **Measure a seat.** Play pink noise from one ring laptop and listen for
   where it gives out; if it holds to 150 Hz the residue stacks are
   reinforcement, if it gives out at 250 Hz they are the whole bass.
5. **Program note (R7).** One sentence inviting listeners to turn their heads:
   front-back confusions nearly disappear when they do.
