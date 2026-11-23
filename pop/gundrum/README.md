# gundrum

A four-voice fugue in D minor at 120 BPM, 1:15, in which every sound is a gun
cut short and played as a drum.

## Kit

| Voice | Sample | Played |
|---|---|---|
| soprano | `pistol-9mm-dry-fire-01` (Walther PPQ, dry fire) | D4–Bb5, −7…+13 st, left |
| alto | `pistol-9mm-shot-01` (Walther PPQ, 3 shots) | G3–D5, −12…+7 st, right |
| tenor | `ak47-shot-01` (4 shots) | A2–D4, −12…+5 st, half left |
| bass | `shotgun-pump-shot-01` (Benelli Nova, 2 shots) | C2–Bb3, −16…+6 st, centre |
| pulse | `pistol-heavy-shot-03` (1911 .45), low-passed at 900 Hz | every beat, D (A under the pedals), centre |
| offbeat | `rifle-dry-fire-01` | the off eighths |
| backbeat | `smg-9mm-shot-01` | two and four, from bar 7 |
| ornaments | magazines, slide and pump racks, bolt locks, casings, one SMG burst, a .30-06 tail on the last chord | phrase ends |

Each sample is cut 2 ms before the first sample over 12 % of its peak, resampled
for pitch (pitch and length move together), held 45–300 ms and faded, after
`gun` in [`../wattajetta/bin/last-minute.py`](../wattajetta/bin/last-minute.py).
The multi-shot WAVs are cut into takes that play in rotation.

## Form

The subject is two bars: a tresillo leaping tonic to dominant, a run of
sixteenths, eighths back down.

```
x..x..x.x.x.x.xx xxxxx.x.x.x.x...
D  A  F E G F ED EFGABb A G E F
```

| Bars | What happens |
|---|---|
| 1–2 | Pulse; magazines in, slide racked |
| 3–10 | Exposition: alto subject (D4), soprano tonal answer (A4) over the alto's countersubject, tenor subject (D3), bass answer (A2) |
| 11–14 | Episode: the head in sequence, down a step a bar, soprano answering alto at the half bar, over a bass falling in fifths D G C F Bb E A D |
| 15–16 | Subject in F in the alto, countersubject now below it in the tenor |
| 17–18 | Subject inverted in the bass, countersubject inverted in the soprano |
| 19–22 | Mensuration canon 4 : 2 : 1 — bass in augmentation, alto as written (subject, then answer), soprano in diminution four times; tenor adds the inversion from bar 21 |
| 23–26 | Break, no pulse: alto plays the subject in retrograde alone, then against the tenor playing it forwards; roll, pump rack and burst into 27 |
| 27–30 | Stretto at three beats, at the fifth: bass and tenor, then alto and soprano |
| 31–34 | Stretto in two pairs, each answered in inversion at the octave after four beats |
| 35–37 | All four on the subject's first bar in octaves, the run, a dominant, a last chord with a major third |

## Where the rules come from

- [`papers/composition-platter/bach/digest/05-fugue.md`](../../papers/composition-platter/bach/digest/05-fugue.md)
  and [`06-contrivance.md`](../../papers/composition-platter/bach/digest/06-contrivance.md)
  define the terms and end in `tools` blocks that were not yet built.
  `tonalAnswer`, `transform`, `strettoDistances` and `invertibleAt` in
  `bin/gundrum.mjs` are those tools, for this one subject.
- [`pop/lib/counterpoint.mjs`](../lib/counterpoint.mjs) supplies the consonance
  test for every check, and writes the countersubject's pitches:
  `generateFirstSpecies` over the answer's eight beats, verified by
  `checkFirstSpecies`.
- [`pop/lib/necklace.mjs`](../lib/necklace.mjs), after
  [rhythm-platter digest 06](../../papers/rhythm-platter/digest/06-complements-canons.md):
  the countersubject's rhythm is the complement of the subject's, thinned so no
  two sixteenths touch. `interlocks` confirms the two never strike together.
- Digest 02 (the bass leaps down a fifth or up a fourth) sets the episode's
  bass; digest 03 (139 of 177 minor chorales end major) sets the last chord.
- The tonal-answer rule and the choice of inversion axis are standard practice;
  the digest states only that tonic and dominant swap.

Checks run on the pitch each line sounds on the quarter-note beats. The stretto
search found one clean rectus delay (three beats, at the fifth) and one clean
mirrored one (four beats, at the octave); the piece uses those. Voices that are
not partners in a stretto pair are not all consonant with each other, and the
run prints every pair's score.

## Run

```bash
node pop/gundrum/bin/gundrum.mjs
```

Writes to `out/` (gitignored): `gundrum.wav` (24-bit, 48 kHz, stereo),
`gundrum.mp3` (320 k), `gundrum-timeline.png`, and `gundrum.json` (every event,
the checks, per-voice levels, attack timing, loudness). Needs `ffmpeg`, and
`pop/.venv` for the timeline. `GUNDRUM_DRIVE=1.4` sets the level into the
limiter (default 1.9).

Nothing is played. The run measures instead: the WAV is −12.9 LUFS integrated
at −1.1 dBTP (the mp3 is cut 0.5 dB lower to stay under −1 dBTP), and all 431
detected attacks are within 2 ms of the grid.

## gundrum and bass

A second, separate piece on the same subject: a drum-and-bass suite at 172 BPM
in five movements, 2:18. The drums are guns; the bass is a synthesized sub (a
sine with a little second and third harmonic and a quiet reese an octave up)
that carries a contrapuntal line of its own.

```bash
node pop/gundrum/bin/gundrum-and-bass.mjs
```

Writes `out/gundrum-and-bass.wav`, `.mp3`, `.json` and `-timeline.png`.
`gundrum.mjs` and its outputs are untouched; the subject, answer and
countersubject are restated in the new script.

| Movement | Bars | Device | Cross-rhythm |
|---|---|---|---|
| I · Invention | 18 of 4/4 | Two parts, pistol and sub: imitation at the octave, countersubject above then below, inversion, two sequences, stretto at three beats | The subject's 3+3+2 head against the two-step; hats on E(5,16) and E(7,16) |
| II · Courante | 8 of 6/4 | A first-species pair in voice exchange after four bars, running eighths above | 3 against 2 in every bar; the drums regroup 6/4 as 3/2 in bars 4 and 8 |
| III · Canon per prolationem | 16 of 4/4 | The subject's seventeen pitches as an even row at 1, 3, 4 and 5 notes to the bar | 3 against 4 against 5 |
| IV · Passacaglia | 30 of 4/4 | A five-bar ground D C Bb G A, six variations: theme, half notes, tresillo and its holes, divisions, the ground in the tenor, all together | A click every third sixteenth over the five-bar ground; it comes round every 240 steps |
| V · Stretto | 20 of 4/4 | Stretto over the subject augmented in the sub, again with inversion, the head in close imitation, then subject, answer and crab; major close | Augmentation 2 : 1; hats on E(11,16), ghosts on E(7,16) |

The kit changes: the soprano is a `.30-06` rifle (`sniper-shot-01`) pitched
up, so it has a body; the dry fires become the hats; the snare is the 12 gauge
high-passed, with the 9 mm SMG for ghost notes and rolls. Pitched voices range
over two to three octaves, kick and snare are retuned hit by hit, ghost notes
take their pitch from the subject, and rolls sweep in pitch and loudness. Each
bus is set to a level measured where it plays, so no voice sits under the rest.

Added from the repo: `bjorklund`, `metricWeights` and `syncopationLhl` from
`pop/lib/necklace.mjs` (rhythm-platter digests 02 and 04) place and weight the
hats and ghosts and score the kick patterns; `generateFirstSpecies` writes the
courante's pair and both counters over the passacaglia ground. The forms
themselves (invention, courante with hemiola, prolation canon, passacaglia on a
falling ground, divisions) are standard practice; the Bach digests name only
canon, augmentation, inversion, stretto and invertible counterpoint.

Measured: the WAV is -9.4 LUFS integrated at -1.1 dBTP; the mp3 is cut 1 dB
lower to stay under -1 dBTP. The limiter averages 0.6 dB of reduction and never
exceeds 3.6 dB.

## gundrum and bass 2

The same score with four layers added, rendered beside version 1 for A/B.

```bash
node pop/gundrum/bin/gundrum-and-bass-2.mjs
```

Writes `out/gundrum-and-bass-2.wav`, `.mp3`, `.json` and `-timeline.png`.
`gundrum-and-bass.mjs` is untouched; version 2 is a copy, because its mix and
master differ throughout, not just in added parts. The render takes about
four minutes.

- **High sines.** One pure sine per line, notes that touch glide 25 ms into
  each other, a little vibrato after 0.25 s. Each line is moved by octaves to
  sit near A5, three to four octaves over the sub (A4–Bb6 overall). It follows
  the countersubject in I, plays the courante's counter in half bars in II,
  adds the row at two notes to the bar in III (bars 9–16), carries the far
  counter in passacaglia variations 5 and 6, and doubles the subject and
  countersubject over V's last drop. Soft sine triads (A4–D6) hold over the
  ground from variation 3, the dominant made major, climb through V's build
  and hold D major at the end.
- **Microtiming**, after rhythm-platter digest 07: the score stays the grid; a
  separate table moves each hit inside its beat bin. Kick, snare, the ghost
  doubled on the snare, sub, bells and booms are not moved. Ghosts sit 7 ±2.5 ms
  late, hats 3 ±1.5 ms, the pitched guns ±2.5 ms. In I (56 %) and V (55 %) the
  off-sixteenths swing: +10.5 and +8.7 ms on hats and ghosts, half that on the
  guns' lines. Velocity swells over each four-bar phrase (two bars in the
  courante) and wanders slowly per voice, within ±1.9 dB. A take is picked at
  random, never the same one twice running.
- **FEM bells** from `pop/bell`, cached in `out/.bell-cache/`: a bronze church
  bell at D2 (A1 on V's dominant build) as the gong on every movement downbeat
  and every drop; bronze handbells doubling the subject's head at cadences and
  drops, folded into D4–D5; glass F#4, A4 and D5 on the last chord.
- **Space.** Each gun bus has an azimuth and a distance. Distance lowers the
  direct sound (−3.3 dB per tripling), cuts its top, brings six early
  reflections closer behind it and shortens the pre-delay into the room. The
  .30-06 is far left (4.5 m), the pistol close right, the AK half left; the
  three dry fires stand right, left and half right; kick and sub stay mono
  and dry. Six explosions (grenade-explosion-01 and -02, the distant-shot-01
  tail, a 90→30 Hz sub drop) land 8–20 m off or cross the field over 2.6–3 s,
  into a 5.5 s hall, and duck about 11 dB under each kick.

Bells, sines, booms and the hall join after the drive, so nothing but the guns
and the sub reaches the soft knee. Drive is 1.8 (`GUNDRUM_DRIVE`) and the make-
up gain is trimmed toward −10 LUFS (`GUNDRUM_LUFS`). Measured: −10.1 LUFS
integrated at −1.4 dBTP; 6.5 % of samples in the knee against version 1's 14.1 %.
The limiter works harder than in version 1 (1.1 dB mean, 7.4 dB maximum).

## Samples

All CC0 1.0, from `pop/wattajetta/assets/guns/`; authors and sources are in
[`SOURCES.json`](../wattajetta/assets/guns/SOURCES.json) there. `rpg-launch-02`
and `ricochet-02` are not used. The suite adds `sniper-shot-01`,
`pistol-9mm-dry-fire-02` and `grenade-explosion-01`.
