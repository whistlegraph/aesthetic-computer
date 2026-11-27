# tombomb

A dance track, 2:57, in F minor, in which every sound is a synthesized
tom, struck or rubbed, and the tempo is a dirt bike on a motocross track.
Drum and bass and rock, pushed toward hard techno, written the way a fugue is,
with a riff in power chords, a slow top line, donks, a wobble bass, reversed
swells and a cathedral. No samples.

```bash
node pop/tombomb/bin/tombomb.mjs                  # → out/tombomb.{wav,mp3,json}
TOMBOMB_RIDE=1 node pop/tombomb/bin/tombomb.mjs   # print the ride bar by bar and stop
```

Writes to `out/` (gitignored): `tombomb.wav` (24-bit, 48 kHz, stereo),
`tombomb.mp3` (320 k) and `tombomb.json` (every stroke, scrape and sung note;
the ride bar by bar; shifts, jumps, spins, tunes; levels and loudness). Needs
`ffmpeg`. The render takes about two minutes.

## The tom

One function makes every struck drum. A stroke is the head's modes at 1, 1.59,
2.14, 2.30 and 2.65 times its note, the higher ones quieter and shorter. The
head starts sharp and settles as it relaxes, more so the harder it is struck. A
click of low-passed noise is the stick; the rimshot adds four shell partials
and 45 ms of head buzz; a hand stops the drum when its note ends.
9,273 strokes are played on 1,508 distinct drums.

| Kind | Drum | Plays | Range |
|---|---|---|---|
| kick | 18" floor tom, slack head: it starts 3.6 times sharp and falls in 20 ms | the kick, tuned to the chord | F1–C2 |
| floor | 16" floor tom | the bass, the landings | F1–G3 |
| rack | 12" rack tom | tenor, fills, runs, the riser | E2–G5 |
| high | 8" concert tom and octobans | alto, lead, open hats, the flips | C4–E7 |
| tick | 6" tom damped with a hand, 30 ms | hats, the freewheel | C7–Bb7 |
| ring | undamped concert toms, soft mallets | the chorale, the start of each sung note, the last chord | F1–A5 |
| power | 13" tom tuned to one clear note, its overtones damped | the power chords | Ab1–C#4 |
| rim | rimshot on a 10" tom | the snare, the gear changes | Bb2–F#4 |

## The rubbed head

Scratches, scrapes and brakes are a finger on one of the toms.
[Menu Band's drum-skin scratch](../../slab/menuband/Sources/MenuBand/MenuBandPercussion.swift)
gave the layout (skin, snare wires, rim, hat and metal edge by distance from
the centre) and the control: it arrives 120 times a second and is held, then
smoothed with a 2.5 ms attack and a 6 ms release. The sound is three things.

- **The hand.** Each stroke has a bell-shaped (minimum-jerk) speed. In a
  scratch the finger stays down and stands still for 10–40 ms at each turn.
  Length, top speed, pressure and path differ from stroke to stroke: most
  strokes are light and now and then one is leaned on. Speed and pressure
  tremble at 8–12 Hz, and a gesture starts up to 4 ms early or late.
- **The contact.** The fingertip is a 1.5 g mass on a spring dragged by the
  hand. Friction falls as it slips faster (Stribeck), so it sticks, stretches,
  lets go and catches again; the head's surface is uneven in 0.4 mm cells,
  which makes the catches irregular. Every bump it passes is a small impact, a
  Poisson train at tip speed times 9,000 bumps a metre, softened less the
  faster it goes. A fingernail (strokes at the edge) has less grip and fewer,
  sharper impacts with nothing under 500 Hz.
- **The drum.** That force drives the head the toms have (modes at 1, 1.59,
  2.14, 2.30, 2.65 times its note), each mode as strongly as its shape allows
  at the finger's place. The pressing finger damps the head and tightens it by
  up to 3.5 %, so it barely rings while rubbed and rings on at its own pitch
  when the finger lifts. On the rim and the metal edge, hoop and shell modes
  answer instead. The drums rubbed are rack and floor toms, G3 to F4, tuned to
  the chord's root or fifth.

158 gestures, 242 strokes, placed as features and not as a layer:

- **intro, breakdown:** slow drags across every seam, out and back. These are sent to the cathedral and sit 4 dB forward.
- **exposition:** a fingernail at the edge after the snares of every other bar; a scratch every fourth bar.
- **drops:** back-and-forth scratches in sixteenths and triplets, every other bar.
- **sequence:** a quick rub at the rim before beats three and one, every other bar.
- **into each drop:** one scrape from the centre out to the edge, accelerating.
- **in the air:** a scrape circling the head. **Coasting:** the tyre. **At the end:** the brakes.

In the drops the scrapes are close and dry: a little of the small hall and
three reflections 6 to 14 ms behind them, no cathedral.

Measured on the gestures themselves:

| Check | Result |
|---|---|
| Brightness against finger speed, all 9,928 frames | centroid 2,043 Hz in the slowest third, 3,236 in the middle, 4,748 in the fastest; correlation 0.53 |
| The same inside single strokes (190 long enough to tell) | median correlation 0.45; 45 % of strokes over 0.5 |
| Level where the finger stands still at a turn (84 turns) | median 10.6 dB under the stroke before it; 45 % of turns at least 12 dB under |
| Stick-slip catches | 2,982; gaps of 9.17 ms (10th percentile) to 29.52 ms (90th), median 12 ms; standard deviation 1.04 times the mean |

Inside a single stroke brightness follows speed only loosely, because each
slip is itself a short bright event whatever the hand is doing.

Level: the scrape bus sits under the snare everywhere, and under the lead
wherever there is one to speak of.

| Section | Against the snare | Against the lead |
|---|---|---|
| i · intro | -3.7 dB | no lead |
| ii · exposition | -9 dB | -1.9 dB |
| iii · drop | -15.6 dB | -12.3 dB |
| iv · sequence | -15.7 dB | -10.5 dB |
| v · breakdown | -7.4 dB | +9.2 dB |
| vi · second drop | -12.7 dB | -7.4 dB |
| vii · finale | -14.9 dB | -10.4 dB |

(The breakdown has almost no lead to compare with.)

## The ride

The beat is the engine's stroke. A 180 kg bike and rider go round a track
composed bar by bar; speed is integrated from power, rolling resistance, air
drag and gravity, and BPM is speed through the gearbox (14.5 BPM per m/s in
fourth, six gears 9 % apart). The rider shifts down when the engine labours
under 160 and up over 183, so a climb is a slow sag and then a step. The boost
is a third more power; held for 31 bars it overheats the engine, which then
runs at 0.7 until it has cooled. It overheated in bar 65 and had cooled by
bar 79: that is the breakdown. Within the beat the time leans with the
crank, 2 ms on the flat and up to 7 under load. Over the rock gardens every
stroke off the beat is knocked up to 5 ms early or late and loose rack toms and
rims rattle, about five a bar; the bar lines do not move.

| Section | Grade | Ground | Track | Gears | BPM |
|---|---|---|---|---|---|
| i · intro | 0 % | dirt | the start straight | 5 | 163–174 |
| ii · exposition | 3.5 %, -3 % | dirt | a climb, a crest, down the back | 5 4 3 | 163–180 |
| iii · drop | 0 %, -1 % | dirt, rocks | boost held; a rock garden in bars 37–40; a jump landing on bar 41 and a bigger one landing on bar 49 | 6 5 | 171–195 |
| iv · sequence | 0 % | mud, dirt | lands in mud, still on the boost: down through every gear | 6 5 4 3 2 1 | 154–180 |
| v · breakdown | 3 %, -10 % | dirt | engine hot up a long climb; over the top in bar 74 and down, coasting in bars 79–80 | 4 3 2 1 5 6 | 161–181 |
| vi · second drop | 0 % | dirt, rocks | flat out; a rock garden in bars 97–100; jumps landing on bars 89, 97 and 105 | 6 5 | 161–190 |
| vii · finale | -2 %, 0 % | dirt | downhill to the line, a jump landing on bar 113, throttle shut for the last four bars | 5 6 | 117–181 |

28 gear changes, each a clunk (a rimshot and a floor tom), at bars
19.5 (5→4), 23.75 (4→3), 28.25 (3→4), 30 (4→5), 32.75 (5→6), 41 (6→5), 42.75 (5→6), 49.5 (6→5), 50.25 (5→4), 51.25 (4→3), 52.25 (3→2), 54 (2→1), 62.25 (1→2), 63 (2→3), 64 (3→4), 66.5 (4→3), 68.5 (3→2), 70.75 (2→1), 75.75 (1→2), 76.5 (2→3), 77.25 (3→4), 78 (4→5), 79 (5→6), 99.25 (6→5), 103.5 (5→6), 105 (6→5), 109.75 (5→6), 113 (6→5).

Jumps: the wheel leaves the ground, so there is no pulse (kick, bass and snare
stop) and the engine races 8 %; the landing is the biggest floor tom, and it
costs speed, more after a long flight and more on the flat.

| Lands on bar | Sixteenths | Airtime | Height | Ramp | BPM before → air → after | Speed lost | Turns |
|---|---|---|---|---|---|---|---|
| 41 | 4 | 0.33 s | 0.13 m | 7.5° | 168 → 181 → 157 | 5 % | 1 |
| 49 | 8 | 0.59 s | 0.43 m | 12° | 188 → 203 → 165 | 11 % | 3 |
| 89 | 4 | 0.3 s | 0.11 m | 6° | 187 → 201 → 169 | 8 % | 1 |
| 97 | 6 | 0.46 s | 0.25 m | 9.5° | 183 → 198 → 163 | 10 % | 2 |
| 105 | 8 | 0.65 s | 0.51 m | 14.5° | 172 → 185 → 156 | 8 % | 3 |
| 113 | 8 | 0.64 s | 0.51 m | 14.3° | 173 → 186 → 156 | 8 % | 3 |

## Round the head

Sources are heard by two ears 17.5 cm apart (arrival time and head shadow, from
`gundrum-and-bass-2.mjs`), and one behind the head is duller and quieter. Kick
and bass stay in the centre.

- **Flips:** in each jump a roll of high toms and a scrape circle the head one to three times (the table above) and stop dead on the landing.
- **Freewheel:** the ratchet (a 6" tom at 2.6 clicks a metre) circles while coasting and in the air.
- **Breakdown:** the chorale's four toms and the hats stand a quarter-circle apart and turn twice round in sixteen bars.
- **Stretto (second drop, bars 1–8):** tenor, alto and lead stand a third of a circle apart and step a third of a turn each bar.

## Space

Three rooms, and distance.

| Room | Tail | Arrives | Damped from | Used |
|---|---|---|---|---|
| hall | 2.2 s | at once | 5.2 kHz | a little of most voices, everywhere |
| nave | 5 s | 60 ms late | 2 kHz | the drops: whatever is sent to the cathedral there, held down hard under every kick and backbeat |
| deep end | 12 s by design, 13.4 s measured after the last chord | 110 ms late, so the dry hit is clear in front | 1.1 kHz | the intro, the breakdown (with the coasting bars 79–80), the last chord and the brakes |

The cathedral is one send, crossed over in 0.4 s between the nave and the deep
end by where the music is. Sent to it: the whole top line; the chorale and the
drags in the breakdown; the intro's drags; the last strokes of every fill; the
crashes (three high toms left to ring: there are no cymbals); the snare roll
before the second drop; each landing and each flip's toms; the gear changes, a
little; everything far away; the last chord and the brakes. Kick, bass, donks
and the wobble never.

**Near and far.** A stroke can be given a distance: it is quieter, loses its
top to the air, arrives 2.9 ms later for every metre and is mostly room. Far:
the intro's subject (20 m) against the close kick; the alto's answer in the
exposition (14 m, coming close at bar 25) against the tenor's subject; a floor
tom 30 m off every fourth bar of the intro and on the third beat of every other
bar of the breakdown; the swell into each tune's first note (18 m); the hit in
each gap (25 m). 169 far strokes in all.

**Gaps.** Four times everything stops and one hit (the chord in power chords,
a kick, and the far floor tom) is left to ring: bar 24, 1 beat (0.35 s); bar 56, 2 beats (0.75 s); bar 100, 1 beat (0.35 s); bar 116, 1 beat (0.33 s).
At these tempos a beat is a third of a second, so the gaps are short.

## Rock

- **The riff.** Power chords (root, fifth, octave on three single-pitched
  toms), four bars over the ground, leaning on Ab and Eb:
  `F F Ab F Eb F Ab | Db Db F Db Eb Db Eb | Bb Bb Db Bb Ab Bb Db | C C G C Bb C`,
  each bar `x..x..x.x.x.x.x.`. Their bus is driven (a tanh), which on one clean
  pitch per drum adds harmonics and not hash. In the mud and under the brakes
  it goes to half time: the root held, pushed, its fifth below, the root.
- **The bass** drives in straight eighths under the riff, with a little of the
  same drive.
- **The backbeat** is fat (a rack tom struck with the rimshot) and flammed (a
  grace stroke 25 ms early). In the mud (iv) and for the last four bars the
  kit is in half time: kick on one, backbeat on three.
- **Fills** go down the kit into every section, and two strokes go down it
  into each of the 28 gear changes.
- **Crashes** are three high toms left to ring in the cathedral, at the head
  of each drop.

## Donks and wobbles

A **donk** is the bass as a small hard drum: its pitch starts 2.4 times high
and is home in 11 ms, its body is a sine pushed by a sine an octave up (hollow,
like a square wave), and it is gone in a tenth of a second. Donks play on the
"and" of every beat, answering the kick, a four-note figure per chord from
F minor's root, b3, 4, 5 and b7: `F Ab F C | F Ab F Eb | Bb F Bb Ab | C Bb C C`.

A **wobble** is one long bass note (a saw and a square at its pitch) through a
low-pass that opens from 110 Hz to 2 kHz and closes in time, over a sine that
does not move. Its rate is the gearbox: a wobble every half bar in first gear,
then half-bar triplets, quarters, quarter triplets, eighths, and eighth
triplets in sixth. It is locked to the ride's steps, so it slows with the bike.
In the mud the bike goes down through all six gears, and the wobble with it.

## Reversos

A reversed stroke is a tom played backwards so that it swells and stops dead on
its beat. A reversed tail is the chord struck on four ringing toms, put through
a room of its own, and only the room kept, backwards: 2.6 s of reverberation
gathering into a downbeat. They are on a bus of their own.

- **reversed tails** into sections ii, iii, v (two laid on each other, so the breakdown's is the biggest), vi and vii, and into the last chord; none into iv, which is entered from a jump
- **reversed strokes** into a backbeat now and then (every fourth bar of the drops, bars 12 and 16 of the exposition), into the first note of every top-line tune (18 m off), into every landing (the ground coming up, as long as the flight), and into some of the breakdown's far floor toms
- **reversed fills** (the roll down the kit backwards, so it climbs): the first half of the intro's last bar, of the sequence's last bar and of bar 16 of the second drop, and far off at the end of the breakdown's twelfth bar

| Section | Power chords | Donks | Wobbles (steps per wobble) | Reversed strokes | Reversed tails | Far strokes |
|---|---|---|---|---|---|---|
| i · intro | 0 | 0 | 0 | 9 | 1 | 91 |
| ii · exposition | 1 | 0 | 0 | 5 | 1 | 41 |
| iii · drop | 108 | 30 | 0 | 9 | 0 | 4 |
| iv · sequence | 63 | 0 | 16 (8, 5.33, 4, 2.67, 2, 1.33) | 15 | 2 | 4 |
| v · breakdown | 0 | 0 | 0 | 13 | 1 | 19 |
| vi · second drop | 108 | 47 | 0 | 18 | 1 | 6 |
| vii · finale | 43 | 8 | 3 (2) | 5 | 1 | 4 |

Levels in the sections where they play, against the kick and against the snare (dB):

| Section | Riff | Donks | Wobble | Reversos |
|---|---|---|---|---|
| i · intro | — | — | — | -9.6 / -3.5 |
| ii · exposition | — | — | — | -8.2 / -1.8 |
| iii · drop | -5.4 / -2.1 | -4.7 / -1.5 | — | -6.7 / -3.4 |
| iv · sequence | -5.6 / -2.8 | — | -2.3 / +0.5 | -4.9 / -2.1 |
| v · breakdown | — | — | — | -14.1 / -10.3 |
| vi · second drop | -6.2 / -1 | -5.4 / -0.3 | — | -7.9 / -2.8 |
| vii · finale | -6.8 / -1.7 | -5.6 / -0.5 | -3.6 / +1.5 | -8.9 / -3.7 |

## The writing

Pitches are rungs of the chord that is sounding, so a figure fits wherever it
is put.

```
subject        x.x.xxx.x.x.xxx.    rungs 0 2 1 2 3 2 1 0 1 2
countersubject x...x.x.x...x.x.    rungs 3 2 4 3 2 1
ground         Fm · Db · Bbm · C
fifths         Fm · Bbm · Eb · Ab · Db · Bbm · C · C
```

| Section | From | Bars | BPM | Gears | What happens |
|---|---|---|---|---|---|
| i · intro | 0:00 | 16 | 163–174 | 5 | Kick on every beat, hats, slow drags; bass and the subject 20 m off from bar 9; snare from bar 13; a far floor tom every fourth bar |
| ii · exposition | 0:22 | 16 | 163–180 | 5 4 3 | Two-step with flams. Subject in the tenor, the alto answering from 14 m, then close; countersubject in the lead. The chorale, twice. Bar 8 ends on a hit left to ring |
| iii · drop | 0:45 | 16 | 171–195 | 6 5 | The riff over a chopped two-step with a fat backbeat. Bass in eighths, then donks. Subject in tenor and, twice as fast, lead. The subject four times as slow on top, then mirrored |
| iv · sequence | 1:06 | 16 | 154–180 | 6 5 4 3 2 1 | Half-time rock in the mud, round the fifths: the riff slow, a wobble bass, lead arpeggios. The falling sevenths, twice. Bar 8 stops for half a bar |
| v · breakdown | 1:30 | 16 | 161–181 | 4 3 2 1 5 6 | No kick: rolled toms turning in the deep cathedral, a floor tom 30 m off. The hook, alone, twice. A riser down the hill |
| vi · second drop | 1:52 | 24 | 161–190 | 6 5 | Stretto over the donks; then the riff with the subject twice as slow in the bass; then the riff, eighths and donks by turns. The hook three times |
| vii · finale | 2:24 | 16 | 117–181 | 5 6 | The sequence again; then the riff, the subject in tenor and lead, the hook over the chorale; half time and a wobble as the bike slows; F major |

## The top line

A slow voice over the fast drums: a head bowed with two fingers, the friction
model held steady so it sustains. Its noise is heard only slowly: smoothed to
about 30 Hz it leans on the tone's loudness and lets the pitch wander a few
cents, and what is left of the noise itself is a breath 34 dB under the tone.
The head's second and third modes sound with the first, and each note is
started by a soft stroke on a ringing tom. It is the loudest thing when it
plays and all of it goes to the cathedral. It rides the bike's tempo like
everything else.

| Tune | Bars | Notes |
|---|---|---|
| chorale (Bachian), 8 bars over the ground | 17–32, and under the hook 113–120 | C · Db–C · F · F–E–G · G–F · F–Db · Db–F · F–E |
| its second voice | 25–32 | Ab · Db · C · C · Ab · Bb · C–G |
| subject four times as slow, then mirrored | 33–48 | F C Ab Db F Db Bb G C E, then F Ab Db Ab F Bb Db G E C |
| falling sevenths (Bachian), 8 bars over the fifths | 49–64, 105–112 | Ab · · G · · F · · E · E–G, with C Db C Bb C above the second time |
| hook (pop), 8 bars over the ground | 65–80, 81–104, 113–120 | C Bb Ab · F Eb Db · Db C Bb · C Db C, then C Bb Ab · F Eb Db · Db C Bb · G Ab G |

The chorale dallies on suspensions: F tied over the bar hangs a fourth above
the dominant's bass before it falls to E (4–3), and G is carried into F minor
as a ninth before it falls to F (9–8). The falling line holds each third until
it has become the next chord's seventh. Of 215 notes that begin on a half
bar, 206 are consonant with the chord's root (`pop/lib/counterpoint.mjs`); the
other 9 are those suspensions.

## Where the noise was

Measured, not heard. Each source alone as it sits in the mix, over the three
drops: energy above 4 kHz, above 8 kHz and in 2–6 kHz (dB, one scale for all
rows), and spectral flatness between 150 Hz and 16 kHz (0 a pure tone, 1 white
noise). Before → after.

| Source | Over 4 kHz | Over 8 kHz | 2–6 kHz | Flatness |
|---|---|---|---|---|
| song | -8.6 → -37.6 | -10.9 → -41.6 | -9.4 → -28.3 | 0.028 → 0 |
| snare | -11.1 → -20.8 | -12 → -23 | -15.2 → -17 | 0.015 → 0.003 |
| hats | -17.9 → -16.3 | -26.7 → -28 | -9.9 → -9.7 | 0.022 → 0.016 |
| lead | -22.5 → -23 | -33 → -34 | -12.4 → -12.4 | 0.001 → 0.001 |
| cathedral | -18.9 → -41.4 | -21.4 → -45.3 | -18.6 → -33 | 0.007 → 0 |
| fx | -24.3 → -33.7 | -25.4 → -37 | -26.6 → -27.6 | 0.028 → 0.004 |
| scrape | -30.3 → -35.8 | -34.6 → -40.7 | -28 → -33.3 | 0.009 → 0.016 |
| alto | -34 → -35.7 | -37.3 → -39.9 | -25.1 → -25.2 | 0 → 0 |
| hall | -38.1 → -43.3 | -42.8 → -52.5 | -28.4 → -29 | 0.004 → 0.001 |
| tenor | -39.6 → -42.4 | -42.6 → -45.8 | -33.8 → -34.1 | 0 → 0 |
| kick | -40 → -34.2 | -43.2 → -36.9 | -39 → -34.9 | 0 → 0 |
| bass | -39.3 → -39.2 | -42.6 → -42.6 | -37.9 → -37.8 | 0 → 0 |
| **the mix** | -2.1 → -11.6 | -4.2 → -18.6 | -1 → -4.3 | 0.016 → 0.001 |

The mix's share of energy above 4 kHz went from -20.6 dB to -29 dB of
its total. The noise was mostly the top line: its friction noise was shaking
the tone at audio rate, on every note, at the loudest bus level, and feeding
the cathedral. After it came the snare's head buzz. The drive was not a
source: the mix had the same share above 4 kHz before and after the soft knee
(-20.6 and -20.5 dB), and the limiter was taking off 0.02 dB on average.

What changed: the top line's grip noise now moves its loudness and pitch only
slowly (about 30 Hz) and its breath is 34 dB under the tone; the snare's buzz
is half as long, rolled off above 6 kHz and under the shell; sticks are a
little softer on rack and high toms; hats are 8 or 9 to the bar in the drops,
not 11 or 13; the lead's double-speed subject comes once a bar; scratches come
every other bar and the rim rubs twice every other bar; the cathedral is
damped from 1.5 kHz and held down harder under the kick; the drive is 1.45,
not 1.7 (44 % of the drops' samples were in the knee, now 34 %). What did not
move: the hats' and the lead's energy in 2–6 kHz, which is pitched. Above
8 kHz the mix lost the most, -14 dB; if it reads dark, the sticks
(`stick`, `bright` in `KIND`) are the place to give it back.

### After the rock pass

The same measure after adding the riff, the donks, the wobble and the
reversos, against the cleaned-up mix before them (drops, dB).

| | Over 4 kHz | Over 8 kHz | 2–6 kHz | Flatness |
|---|---|---|---|---|
| the mix, cleaned up | -11.6 | -18.6 | -4.3 | 0.001 |
| the mix now | -11.9 | -20.1 | -4.4 | 0.001 |
| riff (driven) | -47.6 | -50.3 | -45.4 | 0 |
| donks | -58 | -62.6 | -31.5 | 0 |
| wobble | -52.1 | -56.5 | -39.4 | 0.002 |
| reversos | -56.1 | -60.3 | -43 | 0 |
| nave / deep end | -39.8 / -59.1 | -44.4 / -70.3 | -29.7 / -48.4 | 0 / 0 |

The mix's share above 4 kHz is -29.4 dB of its total (it was -29 after
the cleanup, -20.6 before it). The new voices are the least noisy things in the mix.

## Measured

Nothing was played. The WAV is -9.6 LUFS integrated at -1.5 dBTP (the mp3
is cut 1 dB lower), with no sample that is not a finite number. The drops sit
at about −10.1 dB RMS, the mud at -11.1, the intro at -14.5 and the breakdown at -13.2 The
sides are 6.8 dB under the middle. The limiter takes off 1.94 dB at most:
the level comes from the soft knee before it (`TOMBOMB_DRIVE`, default 1.45;
`TOMBOMB_LUFS`, default −9.5; `TOMBOMB_NAVE`, the cathedral's return, default 1.4).

Borrowed: the hall, the ear, the limiter and the loudness loop from
[`../gundrum/bin/gundrum-and-bass-2.mjs`](../gundrum/bin/gundrum-and-bass-2.mjs);
`bjorklund` from [`../lib/necklace.mjs`](../lib/necklace.mjs) places the hats;
`isConsonant` from [`../lib/counterpoint.mjs`](../lib/counterpoint.mjs) checks the top line.
