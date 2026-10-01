# afternoon-study

## Nighttime Study — Ringing Voice

The latest 9:48 revision turns measured vowels from all three takes into
pitched bell instruments. Vocal-bell chords ring for up to 38 seconds;
upper formants decay faster, leaving the fundamental hanging underneath.
The tuned lead and chorale remain, with longer 10–20-second room decays,
the original dance pulse, and additional space for the ending to ring out.

```sh
pop/.venv/bin/python pop/afternoon-study/bin/render-night-ringing.py
```

Master: `out/nighttime-ringing/nighttime-study-ringing.{wav,mp3}`. Its
`bells.wav` stem is synthesized from the original voice's spectral envelopes.
This reuses the chorale's cached vocal analysis; render the chorale first on
a fresh machine. Prior versions remain separate.

## Nighttime Study — Aesthetivox Chorale

Latest revision: the vocal suite's 9:32 form and 104 BPM groove, with 36
recorded phrases, voice from the opening, stronger Aesthetivox tuning, and
three separately written choir parts. The lead locks to the source score,
retaining 4% of within-note pitch movement and the original unvoiced sounds.
Bass, tenor and upper voices move through explicit chord voicings; selected
upper notes suspend across a chord change before resolving. Phrase endings
extend into held source vowels where the next entrance leaves room.

The choir grows after the opening and opens further in the slow movement.
Piano, bells and sine harmony sit lower. This is a pop chorale arrangement,
not strict four-part counterpoint. Full takes supply the voice; no tiny
repeated note-bank lead is used in this revision.

```sh
pop/.venv/bin/python pop/afternoon-study/bin/render-night-chorale.py
```

Master: `out/nighttime-chorale/nighttime-study-chorale.{wav,mp3}`. Separate
lead and choir stems, source hashes/ranges, tuning parameters and harmonic
voicings are preserved with the render.

## Nighttime Study — Vocal Suite

Latest September 30 revision: 9:32 at 104 BPM. The foreground now uses 18
continuous excerpts from the three original `stems/voice.wav` microphone
recordings, with their pitch, timing and breaths preserved. No WORLD or
phase-vocoder processing touches this lead. Source ranges and hashes are
recorded per phrase. Piano, bells and sine harmony duck beneath the voice.

The form is overture (0:00), exposition (1:14), development (2:46), slow
movement (4:55), transformed return (5:51), and coda (8:18). Two newly written
instrumental themes move between piano and bells, fragment and answer in
canon, stretch into longer notes, then return. This is a loose classical
narrative, not strict sonata form. Sustained sine harmony overlaps bar lines;
bells decay for 14–24 seconds. The dance groove enters during the overture.

```sh
pop/.venv/bin/python pop/afternoon-study/bin/render-night-suite.py
```

Master: `out/nighttime-suite/nighttime-study-suite.{wav,mp3}`; the isolated
original vocal is `out/nighttime-suite/stems/voice.wav`. Earlier versions
remain separate. Platter sources are listed below.

## Nighttime Study — Dance

September 30 revision after the first nighttime mix felt morose: 9:05 at
108 BPM, warm major sixth/ninth harmony, four-to-the-floor kick, swung brush
ticks, an answering bass line, shorter rolling vocal notes, and two new lead
melodies. The three original takes remain the only vocal sources. Reverbs are
shorter; low sustained hums give way to occasional upper-register breaths.
The breaks retain a pulse. The first ambient version remains available.

```sh
pop/.venv/bin/python pop/afternoon-study/bin/render-night-dance.py
```

Master: `out/nighttime-dance/nighttime-study-dance.{wav,mp3}`. Stems and
receipts are beside it. Uses the same platter sources listed below, retaining
gradual pattern growth, interlocking onsets and a reduced-density return.

## Nighttime Study

The nighttime companion is a new 9:05 composition at 72 BPM using the same
three September 26 voice banks. Lemon sings new, gradually growing melodies;
Saffron supplies overlapping rolling tones and one half-speed answer; Indigo
holds the harmony. Sparse Salamander piano and low marimba surround the voice.

```sh
pop/.venv/bin/python pop/afternoon-study/bin/render-night.py
```

Master: `out/nighttime/nighttime-study.{wav,mp3}`. Separate stems, vocal note
events, harmonic map, source references, measurements and hashes are beside it.

The arrangement applies the chamber platter's
[tonal-form rules](../../papers/chamber-platter/digest/01-tonal-forms.md)
(unequal phrases, growing/mirrored melodies, stillness, opening recalled),
[process rules](../../papers/chamber-platter/digest/03-process-and-envelopes.md)
(gradual note substitution and one half-speed answer), and the rhythm platter's
[interlocking patterns](../../papers/rhythm-platter/digest/06-complements-canons.md).
These are selective compositional applications, not a claim to implement every
rule or reproduce the cited works. The
[Loner arrangement critique](../../papers/arxiv-loner-arrangement-critique/loner-arrangement-critique.md)
informs the centered foreground voice and distinct role for each return.

The two-voice clearing starts at 3:07; the late bloom at 6:00 gives way to a
quiet afterglow at 6:27. The opening returns at 7:33 with half the attacks.
Mastering uses static gain with an oversampled safety ceiling, preserving the
ambient dynamics. The original afternoon mix remains separate.

## Afternoon Study

One dance track from the three Menu Band takes @jeffrey recorded on
September 26, 2026 — Lemon Kittens, Indigo Shadows and Saffron Swallows. It
grows out of `pop/tape-sketches/`, which split those takes into Aesthetivox
note banks (one pitch-corrected WAV per sung note). This lane plays those
banks as three singers, writes new lines for them, and puts the AC OS grand
piano under them. Artist: Aesthetic Dot Computer. 118 BPM, 76 bars, 2:35.

## Who plays what

| Source | What it gives the track |
|---|---|
| Lemon Kittens (voice) | Sings the **verse** and **chorus** melodies written for the record (see `melodies` in the receipts). A `Singer` picks, for every written note, the bank sample whose pitch is nearest (WORLD-shifted by at most five semitones, plus octaves), stretches it to the written length and sings it legato. Harmony samples double at a third, an octave-down double sits under the chorus, an octave-up double joins the second chorus. |
| Saffron Swallows (voice) | Sings the **whole-note counter line** (E–E–F–D, then G–E–F–D), the **chord-tone arpeggio** on sixteenths in the choruses with the octave trading every beat, the un-chopped slow arch over the breakdown, and a reversed C4 that swells into each drop. |
| Indigo Shadows (voice + hits) | Its long hums are the **hymn** pad (three voices, WORLD-shifted ≤5 st into the chords); its short repeated notes are the off-beat **pulse** in the choruses. Its loud recorded hits (peak above .3) tuck quietly under the marimba on the clave — the quiet taps and ticks were recording floor and are gone. |
| Grand piano | The **Salamander Grand** (CC0, Alexander Holm) from `fedac/native/samples/piano`, the same 26 anchors AC OS plays, voiced like `pop/maytrax/c/pianotrax.c`: nearest anchor, resampled, damper release, keyboard pan, a hazy-lazy hand (16 ms late, jittered, velocity wobbled). It sits forward (−17 LUFS bus, barely ducked). The right hand lives an octave up: a solo intro (rolled chords, the verse tune sung up high, a falling run), verse comping, chorus block chords doubling the hook with grace notes and a +24 sparkle on the holds, sixteenth sparkle up top, a pentatonic run at every phrase end, a trill on the high tone, broken chords two octaves up in the breakdown, and a marimba glissando into each drop. |
| Mallet kit | All percussion is synthesized and soft: a modal **marimba** (the `rosewood`/`bass`/`staccato`/`roll` recipes from `pop/marimba/synths/marimba.mjs`, ported) playing chord tones on the son clave, a **woodblock** on the 3:4 hemiola, **bubble pops** (a sine falling an octave and a half in forty milliseconds) on the off-eighths, **brush swishes** on two and four, and a round **soft kick** with no click (on one and three in verses, four to the floor in choruses). Bass is authored and rests: root on one, one answer on the and-of-two, a pickup before the next bar. |

## The build, from femrag++

Each chorus is set up the way `pop/maytrax/bin/render-femrag-plusplus.mjs`
sets up a drop: a four-bar woodblock-and-bubble roll (quarters → eighths →
sixteenths → thirty-seconds) under a noise riser that sweeps 500 → 7500 Hz
with two detuned sines climbing two octaves, one brush swish and marimba hit
on the "and" of four, then the last half-beat goes **silent** — the intake of
breath — and the drop lands on a short driven sub stinger (A1 → A2), a brush
wash, a marimba glissando, and the reversed vocal swell arriving where it
ends.

## Percussion chart

```
slot    1e&a 2e&a 3e&a 4e&a 5e&a 6e&a 7e&a 8e&a
kick    x... .... x... .... x... .... x... ....     soft kick (four to the floor in choruses)
brush   .... x... .... x... .... x... .... x...     brush swish
marimba x... ..x. .... x... .... x... x... ....     son clave, chord tones an octave down
block   x..x ..x. .x.. x..x ..x. .x.. x..x ..x.     3:4 hemiola (3-bar cycle)
bubble  ..x. ..x. ..x. ..x. ..x. ..x. ..x. ..x.     off-eighths (a six-pop pattern in choruses)
```

`out/chart.svg` draws the same three bars. Nothing here is random.

## Separation

The vocal buses (`lead`, `lead-oct`, `long`) stay centred with presence at
4 kHz and a short slap. Instruments are notched at 3.6 kHz and duck under
the lead's 20 ms energy (arp and hymn most, the piano only lightly). The
master aims at −12 LUFS with a light glue compressor, so the mix keeps its
air. Every sung note
has 15 ms attack and up to 250 ms release fades. `out/stems/vocals.wav` and
`out/stems/instrumental.wav` are the two halves.

## Form

intro 4 · verse 8 · build 4 · chorus 16 · hymn 8 · verse 8 · build 4 ·
chorus 16 · outro 8.

## Render

```sh
pop/.venv/bin/python pop/afternoon-study/bin/render.py            # → out/afternoon-study.{wav,mp3}
pop/.venv/bin/python pop/afternoon-study/bin/render.py --lufs -12  # quieter master
```

Sources are read from the private study on the Shelf
(`~/Documents/Shelf/Menu Band pop sketches 2026-09-26`, override with
`--study`). `out/` holds the master, per-bus stems, the WORLD cache, the
chart and `receipts.json` (melodies, every vocal sample used with its shift
and stretch, kit credits, piano bank, breaths, bus trims, master loudness,
SHA-256). Media stays out of git per `pop/ASSETS.md`.
