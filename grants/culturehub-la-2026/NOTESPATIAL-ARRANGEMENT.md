# Arrangement — *Note(s)pat(ial) Native*

A suite in eleven chapters for five laptops in a ring and one held at the
center. September 24. One score, one cue.

**Score:** `fedac/native/scores/notespatial-native.nsscore`, written by
`fedac/native/tools/compose-notespatial-native.mjs`. **Duration:** 12:54.
**Room:** seats 1 to 5 in a pentagon around the audience, seat 1 at the
front, numbers clockwise. Seat C is the sixth laptop in the performer's
hands at the center on a small speaker. It is the piece's voice.

Not one build. Each chapter is its own little ode with its own key,
meter, tempo, tune and spatial gesture, cut on the downbeat the way a
cartoon score is, with stingers and melodic rests. Percussion carries the
beat through every chapter and transition. What ties them: the theme
(a diatonic tune in four two-bar phrases), the held voice as narrator,
and the room.

The reading behind it is in `chamber-studies/` here and, distilled for
future `pop/` work, in `papers/chamber-platter/`. The September 23 pass
(`chamber-studies/05-inside-the-ring.md`) rewrote the low end, the echoes
and the travelling theme for an audience inside a ring of *laptop*
speakers; the form and timings are unchanged.

---

## The chapters

| | Chapter | Starts | Length | Key, meter, tempo | What happens | Where it is |
|---|---|---|---|---|---|---|
| I | **Overture** | 0:00 | 1:00 | free melody, pulse 55 to 136 | Low and high taps from the first bell. The held laptop breathes four long bells and states the theme's first phrase. The ring learns it: four passes, each faster, with the percussion following. A scale lap into the Walk. | Center, then a circle drawn faster |
| II | **The Walk** | 1:00 | 0:56 | C, 4/4, 100 to 104 | Taps front and back. The walk (three chord tones per two beats, swung) from the front three seats, then in halves. A counter-melody enters in the second phrase; syncopated root/fifth bass and light backbeats join the theme in the third. | Front–back axis, then an opposing line |
| III | **Waltz** | 1:56 | 1:18 | A minor, 3/4, 132 easing to 112 | Oom at the back (seat 4), pah left (seat 2), pah right (seat 5). Taps mark all three beats. A tune in the hands, twice. Then the ring hops the tune around the room while the hands hold a counterline. Four answering notes cross each two-bar phrase against the three-step dance, with two hat accents per bar. The tune returns with a third below and chromatic bass pickups. A tag that eases; every laptop lands on A. | A sway; then melody circling under a held line |
| IV | **Chase** | 3:15 | 0:47 | F, 4/4, 152 to 200 | A scale runs up and down the ring one seat per eighth; two seats behind, its inversion an octave down gives chase. Three tempo steps, a chromatic pile-up, a cluster, a crash. Percussion holds the gap. The hands say "again" and it runs once more, faster. | Two runners circling |
| V | **Sneak** | 4:03 | 0:58 | C minor, 4/4, 84 | An offbeat bass creeps one seat a bar around the audience with a tick. The hands tiptoe down a whole-tone stair, then scurry up it. Bar 13: every laptop strikes the chord. Two beats of percussion alone. The sneak resumes at double speed and ends on a low C. | One seat at a time, behind you |
| VI | **Lullaby** | 5:01 | 1:14 | F, 6/8, 72 | Arpeggios rock around the room one seat per eighth. The tune in the hands twice, then an octave down, echoed a bar later by the ring. Pads. The whole field turns once over the chapter. The cradle thins over soft thumps and brushed eighths. | A slow cradle; the room revolves |
| VII | **The Climb** | 6:15 | 1:05 | C, 4/4, 112 to 124 | Four registers an octave apart, one phrase each. A second melody moves against the walk in 3+3+2 eighths. Syncopated bass, light backbeats, high chimes and occasional tom turns gather across the four phrases. Hats in the last phrase. The last bar: a rising D-major lap into a beat and a half of percussion alone. | Two directions, then percussion alone |
| VIII | **The Lift** | 7:21 | 1:53 | D, 4/4, 120 to 124 to 118 | The blast: the room kicked one seat over and springing back. Kick front, snare seat 3, bass seat 4. The theme leaves the hands, lands at seat 1 and hops clockwise one seat every 2.5 s (a lap in 12.5 s); the walk keeps to the two seats across from it. The answer hops the other way (15 s a lap), hats join. Phrases 4 and 5: the eight-turn spin, every laptop trading channels until the field fuses. Bar 55: the tutti D chord held two bars. Percussion bridges the melodic pause. | Everything, spinning; then one chord |
| IX | **Fanfare** | 9:14 | 0:38 | G, 4/4, 132 | Front pair (1, 2) calls in triangle brass; back pair (3, 4) answers. Tutti dotted chords from every seat while the hands play the theme in G. Call and answer once more, an octave up. A stinger; taps carry into the Return. | Pairs across the room |
| X | **Return** | 9:52 | 2:00 | C, 4/4, 104 to 88 | The theme back in the hands over the Walk's counter-melody and syncopated bass; the bass groove leaves after one phrase, the counter-melody after two. The room takes one slow tour. Phrases 2 and 4 mirror the ring about the front axis. Something leaves every phrase: bass, pads, the larger taps; the walk narrows to the front three seats, then seat 1, then stops. The last phrase does not cadence. | The ring closes to the front, then the center |
| XI | **Vanish** | 11:53 | 1:01 | free melody, pulse 60 | The ring hands back the theme's last five notes, one laptop at a time, backwards. Quiet taps keep time between three bells from the hands. A C4 rises out of nothing and is gone. The pulse ends with one tap from behind: the hang-up. | A circle drawn backwards, a point, one knock |

Percussion follows each chapter's tempo: three beats in the Waltz, two
thumps with six brushed eighths in the Lullaby, a quarter-note pulse
elsewhere. Existing kit attacks take precedence. The melodic pauses
retain the pulse; the Return sheds its larger tap pattern but keeps quiet
time through the Vanish's final knock. `--kick` adds a restrained kick on
the main steps throughout; the Lift keeps its written kit.

The mallets-and-kick score has 17,502 events and peaks at 28 simultaneous
voices against the runtime's 32-voice limit.

## Instruments

Stacks of sine partials under the runtime's linear attack and decay:

- **Bell** (the held voice): fundamental, 4 ms attack, tail over 80% of
  the note; octave at 28% for 55% of the length; twelfth at 10% for 30%.
  A detuned copy ramps in over 0.45 s so the voice warbles; the depth is
  a ladder by chapter: 4, 6, 0, 0, 8, 16, 22, 0, 9, 0 cents.
- **Pluck** (the ring): 3 ms attack, tail over 78%, octave at 22% for half
  the length. **Staccato**: 3 ms attack, 60% tail. **Brass**: triangle
  body with a sine octave, 12 ms attack, 35% tail.
- **Echoes**: a sine with a 2 ms octave on top at 22%, 20 ms attack, so the
  answer has energy above 1.5 kHz and can be placed. **Pads**: 600 ms attack.
- **Bass, oom, creep, low C**: triangle root plus a sine octave at 50% and
  twelfth at 28%. A laptop passes little under 200 Hz; the root is heard as
  the residue pitch of its harmonics.
- **Taps**: two sine thumps plus a 30 ms click. **Kick**: a 30 ms click,
  150 Hz, 78 Hz and its second harmonic at 156 Hz. **Snare**: noise plus a
  185 Hz triangle. **Crash**: noise from every seat.

## How the room is written

- **Pinned** lanes: the held voice, the walk and echoes (one lane per ring
  seat), taps at 1, 3 and 4, bass at 4, kick at 1, snare at 3. A hop stays
  a hop under clock skew.
- **Hopping** lanes: the theme (five pinned lanes, one per seat; each note
  written whole to the seat nearest its 12.5 s clockwise orbit at onset)
  and the answer (15 s, counter-clockwise). One laptop per note: no
  crossfade to flam under clock skew, and no phantom between seats, which
  only a listener at the exact center would hear. `--glide` rebuilds the
  September 22 orbiting version.
- **Orbiting** lanes: pads 60 s, top line 24 s counter-clockwise, hats 5 s.
  Wash and noise only; nothing but noise orbits faster than 5 s.
- **Runners** hold a seat for 200 ms or more: the Chase takes two eighths
  per seat from 168 BPM up.
- **The field turns** (`fieldShift`): the whole room rotates, pinned lanes
  included, so laptops trade channels. Used four times: one tour through
  the Lullaby, the blast at the Lift's downbeat (0.4 of a turn, springing
  back at 0.92 Hz, damping 0.58, 4.8 s), the eight-turn quintic spin
  through the Lift's peak, and a slow lap opening the Return. Zero
  elsewhere.
- **Mirror**: the Return flips the ring about the front axis for a phrase
  at a time; the front-three / back-two map is chiral, so the flip reads.

## What the screens show

Each laptop looks into the space: its own notes appear at the horizon and
fly at the front of the screen, arriving exactly when they sound, glowing
in the machine's own color while the note is in the room. The video shows
the room from a raised seat: streams along the floor into each laptop, the
held laptop's stream from above, every laptop's actual screen on its
display, every incoming melodic note named with its frequency, chords
grouped, tempo and note rate at the right.

## Rehearsal

Hosts in seat order: the five ring laptops 1 to 5 clockwise from the
front, then the held laptop last.

```sh
node fedac/native/tools/compose-notespatial-native.mjs      # rebuild the score
node fedac/native/tools/notespatial-native-check.mjs        # routing map, dry run, paint smoke test
node fedac/native/tools/notespatial-native-perf.mjs --seat 3 # frame cost: draw calls per frame by chapter (--all for every seat)
node fedac/native/tools/notespatial-native-render.mjs       # the video from the center, ~3 min to make
node fedac/native/tools/notespatial-native-render.mjs --section 3 --fast   # one chapter in seconds
node fedac/native/tools/spatial-rehearsal.mjs deploy --score notespatial-native H1 H2 H3 H4 H5 HELD
node fedac/native/tools/spatial-rehearsal.mjs cue H1 H2 H3 H4 H5 HELD
node fedac/native/tools/spatial-rehearsal.mjs stop H1 H2 H3 H4 H5 HELD
```

Every chapter is also its own score, rebased to zero, for rehearsing a
part or cueing chapter by chapter if the full run drifts:
`--score notespatial-native-3-waltz`, `-4-chase`, `-8-the-lift`, and so
on. Renders land beside this file and are not committed.

Knobs, all in the composer: each chapter is a function; the eight-bar
chapters are lists of `phrase()` calls with `setTempo()` between them,
the others write their tune as `[midi, beats]` pairs. Add or cut a
phrase to change length. `warbleCents` before each chapter sets the
wiggle. `turnsPlan` holds the field turns.

**Other orchestras.** `--voicing mallets | native | gm` rebuilds the same
score with each instrument family made of something else (marimba family,
the engine's whistle, harp and piano, or GM programs), `--set
held=whistle,bass=sawBass` swaps one family at a time, and `--fx studio`
adds room, drive, wobble and glitch ribbons per chapter, applied on each
laptop by the playback piece. Files carry the tag
(`notespatial-native-mallets-8-the-lift`). The menu, the effects plan and
what to listen for per voicing: [VOICINGS.md](VOICINGS.md).

## Mallets, kick and SUB preview

```sh
node fedac/native/tools/compose-notespatial-native.mjs --voicing mallets --kick
node fedac/native/tools/notespatial-native-check.mjs fedac/native/scores/notespatial-native-mallets-kick.nsscore
node fedac/native/tools/notespatial-native-render.mjs fedac/native/scores/notespatial-native-mallets-kick.nsscore --sub --out grants/culturehub-la-2026/notespatial-native-mallets-kick-sub.mp4
```

The video shows the instrument families, a kick hit indicator, and a SUB
cabinet whose cone follows its filtered signal. SUB uses the receiver's
bass/kick selection one octave down, 25 Hz high-pass, two 80 Hz low-pass
sections, and 25% level. Its compressor is approximated. `--sub-level`,
`--sub-cutoff` and `--sub-az` adjust the model; the assumed position is at
the front, outside the laptop ring. It stays fixed when the field turns.
Room acoustics, device response and receiver latency are not simulated.
`--solo sub --audio-only` exports the SUB alone for inspection.

For live rehearsal, start the receiver with
`SUB_SCORE=fedac/native/scores/notespatial-native-mallets-kick.nsscore node fedac/native/tools/sub-receiver/server.mjs`
and deploy that same score to the six laptops. This render does not deploy
to the room or publish an OS update.

## Listen for, at the tech

1. **0:00** — the held voice over percussion from the first beat. Set the small
   speaker against seat 1's first pluck *from seat 3 or 4*, not from the
   hands: a listener 1 m from a ring laptop and 3 m from the center needs
   the center about 9.5 dB hotter for the two to match.
2. **1:00** — the Walk's fuller taps land on the downbeat the scale lap set up.
3. **1:56** — oom from seat 4, pah from 2 and 5. If the sway is wrong, the
   host order is wrong.
4. **3:15** — the chase: one note per laptop, clockwise (two notes per
   laptop from the second tempo step). A note from two machines at once
   means a seat is mis-numbered.
4a. **4:03** — the sneak: each creep step should be *placeable* behind
   you; if a step reads as a hum from nowhere, that laptop's speaker gives
   out above 200 Hz and the tick needs another 6 dB.
5. **7:21** — the blast: the kick from the front, and the room lurches one
   seat and swings back. Then the theme leaves the hands for seat 1 and
   walks 1, 2, 3, 4, 5 while the walk answers from across the ring.
6. **9:12** — the tutti D chord from all six, then percussion alone.
7. **11:53 → 12:20** — the backwards lap 1, 5, 4, 3, 2, then the hands,
   then one tap from seat 4.

## Risks

- **Drift over 13 minutes.** Six audio clocks aligned once at the cue and
  never corrected. Hops are immune; the pads, hats, kick and snare, the
  spin and the tutti chords are not. Crystals differ by 20 to 50 ppm, so
  the seats are 15 to 39 ms apart by the Vanish: a plucked tutti stays one
  event under about 40 ms and one *point* under 5 ms. Measure with `beeps`
  at the tech; if the spread passes 20 ms by the Lift, cue VIII and X from
  their own files.
- **Nobody is at the center.** Every listener is 1 to 5 m from any laptop,
  so no two seats hear the same mix and the held voice is quietest at the
  back. During center-only passages longer than a phrase (Waltz A sections,
  Sneak tiptoe, Lullaby tune) walk slowly toward seats 3 and 4; be back in
  the middle for the Lift's downbeat so the theme audibly leaves the hands.
- **The laptops' bass.** Play pink noise from one seat at the tech and
  listen for where it gives out. If it holds to 150 Hz the residue stacks
  are reinforcement; if it gives out at 250 Hz they are the whole bass.
- **The small speaker.** The held laptop carries the melody in nine of
  eleven chapters. Its level is the one balance to set by ear.
- **Battery.** Seat 5 died at 2% on September 18. Mains for the ring, the
  held laptop charged.
- **Frame rate.** The screens looked slow at the September 22 run. The
  cause was the hatch inside the flying frames: the Lift asked the software
  raster for up to 2,600 line fills in one frame, 8.7 million over the
  piece on a ring seat. The hatch is now budgeted to ten lines per frame
  and drawn only on frames big enough to read; the peak is 275 draw calls
  a frame, the screen glow no longer scans every event in the score each
  frame, and the status file is written 4 times a second instead of 10.
  `notespatial-native-perf.mjs` reports the per-chapter cost; deploy ships
  the piece, so this needs no reflash, but it is in the next OTA too.
