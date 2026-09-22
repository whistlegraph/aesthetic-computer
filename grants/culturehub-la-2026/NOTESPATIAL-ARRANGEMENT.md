# Arrangement — *Note(s)pat(ial) Native*

The 13-minute form for six laptops, September 24. One score, one cue,
no conductor moves mid-piece. Built from the material that rehearsed on
September 18: the walking sine, the soft 3-against-2 swing, the octave
climb, the D-major theme.

**Score:** `fedac/native/scores/notespatial-native.nsscore`, written by
`fedac/native/tools/compose-notespatial-native.mjs`.
**Duration:** 13:08. **Tempo:** 112 BPM from 1:44 to 12:01; free before and after.
**Room:** six laptops in a ring around the audience, seat 1 at the front,
numbers increasing clockwise. Seat 4 is directly behind the listener.

---

## The form

| | Section | Starts | Length | What happens | Where it is in the room |
|---|---|---|---|---|---|
| I | **Appear** | 0:00 | 1:44 | One sine at the front breathes five long tones. Then it walks: one note per laptop, clockwise, ten laps, each lap faster (1.5 s per seat down to 0.5 s). Lands back at the front. | A point, then a circle drawn faster and faster |
| II | **Ring** | 1:44 | 1:25 | The pulse arrives: soft taps front and back, alone for 17 s. The three-note swing enters, each note hopping to the next laptop. Bass joins at the back. | Front–back axis, with a walk turning around it |
| III | **Echoes** | 3:09 | 2:00 | Every walked note is answered: first from two seats on, then from across the room, then from the next seat. Pads enter mid-section and revolve once a minute. | Every hop has its reply elsewhere; the harmony drifts |
| IV | **Climb** | 5:09 | 2:17 | Four registers, 34 s each, an octave apart. From the third register a high line glides counter-clockwise against the clockwise walk. From the fourth, hats orbit every 5 s. | Two directions at once, then a fast ring on top |
| V | **Lift** | 7:27 | 2:17 | Up a whole step to D. Kick at the front, snare at the back, the theme orbits every 12.5 s over the walk, which drops an octave under it. Second statement adds the counter-orbiting answer and hats. Bars 16–24 of it lift the phrase tails an octave: the peak. | Everything, everywhere, spinning |
| VI | **Return** | 9:44 | 2:17 | Back to C. Kick gone at once. Then, every 34 s, something leaves: an echo level, the bass, the pads, the taps. The walk narrows to the three front seats, then to seat 1 alone, and slows to one note a cycle. | The ring closes toward the front |
| VII | **Vanish** | 12:01 | 1:07 | One slow lap the other way, descending, one laptop at a time. Three tones at the front with lengthening silences. A low C rises out of nothing over 3.5 s and is gone. | A circle drawn backwards, then a point |

Level arc: .3 → .55 → .75 → .9 → 1 → .6 → .3. Peak polyphony 19 of the
runtime's 32 voices, in the Lift.

## How the room is written

Two kinds of lane, and every lane is one or the other:

- **Pinned** (one laptop): the walk and its echoes (six lanes each, one
  per seat), taps front and back, bass back, kick front, snare back. A
  note lives entirely inside one machine, so a hop is a hop and clock
  skew between laptops cannot smear it.
- **Orbiting** (continuous, equal-power handoff between neighbors): pads
  (60 s, three voices 120° apart), top line (24 s, counter-clockwise),
  theme (12.5 s), answer (15 s, counter-clockwise), hats (5 s).

Slow things orbit; struck things sit.

## Rehearsal

Hosts in seat order, 1 to 6 clockwise from the front.

```sh
node fedac/native/tools/compose-notespatial-native.mjs      # rebuild the score
node fedac/native/tools/notespatial-native-check.mjs        # routing map + dry run
node fedac/native/tools/notespatial-native-render.mjs       # hear it from the center: mp4, ~20 s to make
node fedac/native/tools/notespatial-native-render.mjs --section 5 --fast   # one section, parametric head, seconds
node fedac/native/tools/spatial-rehearsal.mjs deploy --score notespatial-native H1 H2 H3 H4 H5 H6
node fedac/native/tools/spatial-rehearsal.mjs cue H1 H2 H3 H4 H5 H6
node fedac/native/tools/spatial-rehearsal.mjs stop H1 H2 H3 H4 H5 H6
```

The renderer places the six feeds around a listener at the center with
measured KEMAR head-related responses (or a parametric head with
`--fast`); `--from 8:30 --to 9:10` renders a slice, `--audio-only` skips
the picture. Renders land beside this file and are not committed.

Each section is also written as its own score, rebased to zero, for
rehearsing one part or for cueing the piece in parts if the full run
drifts: `--score notespatial-native-4-climb`, and so on (`1-appear`,
`2-ring`, `3-echoes`, `4-climb`, `5-lift`, `6-return`, `7-vanish`).

Knobs, all in the composer: section lengths are cycle counts in the `S`
table (one cycle is 1.07 s; 16 cycles is one chord round); the walk's
lap count is `LAPS`; per-section levels are the `lvl` ramps.

## Listen for, at the tech

1. **1:44** — the first tap lands on the grid the walk set up. If the
   walk's last note and the tap feel unrelated, the front laptop is late.
2. **Any hop.** A walked note should come from one machine. Two machines
   sounding it means a seat is mis-numbered.
3. **7:27** — the kick. If it thuds from anywhere but the front, the
   host order is wrong.
4. **8:35–9:00** — the peak. Level check for the house.
5. **12:01 → 12:25** — the backwards lap: 1, 6, 5, 4, 3, 2, 1.

## Risks

- **Drift over 13 minutes.** Six audio clocks are aligned once at the
  cue and never corrected. The hops are immune, the orbiting lanes and
  the kick–snare pair are not. Measure at the tech; if the pair flams by
  the Lift, cue the piece in sections.
- **Battery.** Seat 5 died at 2% on September 18. Thirteen minutes on
  mains, not batteries.
- **Front and back carry the weight.** Kick and taps front, bass, snare
  and taps back. Confirm those two feeds are full-range on the Kalio.
