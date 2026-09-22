# Arrangement — *Note(s)pat(ial) Native*

Thirteen minutes for five laptops in a ring and one held at the center,
September 24. One score, one cue, no conducting mid-piece.

**Score:** `fedac/native/scores/notespatial-native.nsscore`, written by
`fedac/native/tools/compose-notespatial-native.mjs`.
**Duration:** 13:12. **Tempo:** free until 1:23, then 96 rising to 124 by
the Lift, easing to 88 by the Return's end, free again from 12:12.
**Room:** seats 1 to 5 in a pentagon around the audience, seat 1 at the
front, numbers clockwise. Seat C is the sixth laptop in the performer's
hands at the center on a small speaker. It is the piece's voice: the
piece starts there, spreads to the ring, and comes back.

The background reading is in `chamber-studies/` here and distilled for
future `pop/` work in `papers/chamber-platter/`.

---

## The form

| | Section | Starts | Length | Tempo | What happens | Where it is |
|---|---|---|---|---|---|---|
| I | **Appear** | 0:00 | 1:23 | free | The held laptop breathes five long tones, then states the theme's first phrase twice as bells. The ring learns it: the same eight notes hop seat to seat, five passes, each faster, the held laptop keeping the last note under each pass. A scale lap into the first downbeat. | Center, then a circle drawn faster and faster |
| II | **Ring** | 1:23 | 1:17 | 96 → 102 | Taps front and back set the pulse. The walk (three chord tones per two beats, swung 60:40) plays from the front three seats, then in halves: question from the front, answer from the back. Bass at seat 4. The held laptop sings the theme; in the last phrase the ring echoes it once. | Front–back axis; the melody in the hands |
| III | **Echoes** | 2:41 | 2:21 | 104 → 112 | Eight phrases. Theme phrases alternate with instrumental ones where the walk laps the whole ring. Echoes of the theme grow from one to three, seated two, three and one seats on from wherever the last echo landed. Pads revolve once a minute. The last phrase does not cadence: the held laptop holds the dominant, G, into the Climb. | Every phrase answered from around the ring |
| IV | **Climb** | 5:02 | 2:26 | 112 → 124 | Nine phrases in four registers, an octave apart, two phrases each. From the third register a high line glides counter-clockwise against the clockwise walk; from the fourth, hats orbit every five seconds. The last bar is the break: a rising D-major lap, then a beat and a half of silence. | Two directions at once, then a ring on top, then nothing |
| V | **Lift** | 7:29 | 2:09 | 120 → 124 → 118 | Up a whole step to D. Kick at the front, snare at seat 3, bass pattern at seat 4. The theme leaves the hands and orbits the ring every 12.5 s; the walk drops an octave under it. The second statement adds the counter-orbiting answer and the hats; its bars 16 to 24 lift the phrase tails an octave, the peak. Bar 63: every laptop strikes the D chord and holds two bars. General pause. | Everything, everywhere, spinning; then one chord from all six |
| VI | **Return** | 9:38 | 2:34 | 112 → 88 | Home in C, the theme back in the hands. Each phrase something leaves: the kick at once, then an echo level, the bass, the pads, the taps. The walk narrows to the front three seats, then to seat 1, then stops. The tempo eases every phrase. The last phrase does not cadence. | The ring closes to the front, then to the center |
| VII | **Vanish** | 12:12 | 0:59 | free | The ring hands back the theme's last five notes, one laptop at a time, backwards around the room. Three bells from the hands with lengthening silences. A C4 rises out of nothing over 3.5 s and is gone. | A circle drawn backwards, then a point |

Measured on the binaural render, mean level per section rises from
−28 dB (Appear) through −25, −22, −20 to −18 (Lift), and falls to −26 and
−30. Peak polyphony is 18 of the runtime's 32 voices.

## Phrase grammar

Every grid section is built from eight-bar phrases:

- Harmony two bars per chord: C, Am, F, then G in bar 7 resolving to C in
  bar 8. The theme is four two-bar phrases over it.
- Bar 8 is the cadence breath: the walk plays only the downbeat, the taps
  only the downbeat, the bass a whole note. Then a fill: a five-note scale
  lap around the ring in the last two beats, landing on the next downbeat.
- Dynamics swell 6% a bar into bar 7 and relax in bar 8.
- Tempo moves only at phrase starts, by 2 to 4 BPM.
- Joins: half cadence and held dominant (III to IV), break and silence (IV
  to V), tutti chord and general pause (V to VI), unresolved last phrase
  into free time (VI to VII).

## Instruments

Built from the runtime's linear attack and decay:

- **Bell** (the held voice): fundamental with 4 ms attack and a tail over
  80% of the note, plus the octave at 28% for 55% of the length and the
  twelfth at 10% for 30%.
- **Pluck** (the ring walk): 3 ms attack, tail over 78%, plus the octave at
  22% for half the length.
- **Echoes**: single sines, 20 ms attack. **Pads**: 600 ms attack.
- **Taps**: two sine thumps plus a 30 ms noise click. **Kick**: 150 Hz then
  78 Hz. **Snare**: noise plus a 185 Hz triangle.

## How the room is written

- **Pinned** lanes (one laptop): the held voice, the walk and echoes (one
  lane per ring seat), taps at seats 1, 3 and 4, bass at 4, kick at 1,
  snare at 3. A hop stays a hop under clock skew.
- **Orbiting** lanes (equal-power handoff): pads 60 s, top line 24 s
  counter-clockwise, theme 12.5 s, answer 15 s counter-clockwise, hats 5 s.

No orbit is faster than 5 s, so motion never reads as modulation. The only
period under 3 s is the hats, which are noise.

## What the screens show

Each laptop's screen looks into the space: its own notes appear far off at
the horizon and fly at the front of the screen, arriving exactly when they
sound; the screen glows while the note is in the room, then the glyph
fades. Melodic notes carry their names. The simulation video shows the same
thing from above as a radial timeline, with each laptop box carrying a
miniature of its own screen.

## Rehearsal

Hosts in seat order: five ring laptops 1 to 5 clockwise from the front,
then the held laptop last.

```sh
node fedac/native/tools/compose-notespatial-native.mjs      # rebuild the score
node fedac/native/tools/notespatial-native-check.mjs        # routing map, dry run, paint smoke test
node fedac/native/tools/notespatial-native-render.mjs       # the mp4 from the center, about a minute to make
node fedac/native/tools/notespatial-native-render.mjs --section 5 --fast   # one section in seconds
node fedac/native/tools/spatial-rehearsal.mjs deploy --score notespatial-native H1 H2 H3 H4 H5 HELD
node fedac/native/tools/spatial-rehearsal.mjs cue H1 H2 H3 H4 H5 HELD
node fedac/native/tools/spatial-rehearsal.mjs stop H1 H2 H3 H4 H5 HELD
```

Each section is also its own score, rebased to zero, for rehearsing a part
or cueing in parts if the full run drifts: `--score notespatial-native-5-lift`
and so on. Renders land beside this file and are not committed.

Knobs, all in the composer: each section is a list of `phrase()` calls
with `setTempo()` between them; add or remove a phrase to change length
(one phrase is 15 to 20 s depending on tempo); `lvl` is the phrase level;
`register`, `echoes`, `walkWhere` and the instrument switches shape it.

## Listen for, at the tech

1. **0:00** — the first bell from the held laptop alone. Level of the small
   speaker against the ring is set here.
2. **1:23** — the first tap lands on the downbeat the scale lap set up.
3. **Any hop.** A walked note comes from one machine. Two machines sounding
   it means a seat is mis-numbered.
4. **5:02** — the dominant G held in the hands resolves as the Climb begins.
5. **7:28** — the break: a beat and a half of nothing, then the kick from
   the front.
6. **9:32** — the tutti D chord from all six, then the pause.
7. **12:12 → 12:35** — the backwards lap: 1, 5, 4, 3, 2, then the hands.

## Risks

- **Drift over 13 minutes.** Six audio clocks are aligned once at the cue
  and never corrected. The hops are immune; the orbiting lanes, the
  kick–snare pair and the tutti chord are not. Measure at the tech; if the
  pair flams by the Lift, cue in sections.
- **The small speaker.** The held laptop carries the melody. Its level
  against five house-fed laptops is the one balance to set by ear.
- **Battery.** Seat 5 died at 2% on September 18. Thirteen minutes on
  mains, not batteries, and the held laptop charged.
