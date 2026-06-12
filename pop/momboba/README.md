# momboba

**Mombobasleep** — our first 10-minute extended **sleep mix**, a remix of
[`pop/marimba/marimbaba`](../marimba/). The calm nocturne twin of the lullaby,
the way [`pop/sleephellsine`](../sleephellsine/) is the calm twin of
[`pop/hellsine`](../hellsine/).

The difference: sleephellsine *abandoned* hellsine's instruments for pure sine.
Mombobasleep keeps the **actual marimba synth** (`../marimba/synths/marimba.mjs`)
— that's what makes it a marimbaba *remix* and not a sleephellsine clone.
"Mom" = the lullaby rocked dozy, dumb-happy, and very, very slow.

## The formula

marimbaba's 80-second lullaby is dissolved into a slow, **generative** 10-minute
drift. There is no fixed arrangement — every render is built from the same parts
on a deterministic random walk (seeded, so re-renders are bit-identical):

- **Harmony** stays home in marimbaba's F-major world — its four diatonic
  chords **F / Dm / Bb / C** — wandering between them every ~16–22 s. C wants to
  resolve home to F; the weighted walk keeps it gently circling, never landing
  hard enough to wake you. Voice-led by the pad's long ring.
- **Melody** drops marimbaba's own phrase cells (hush / twinkle / wow / ba-ba /
  sleep) sparsely over the chords. Bright cells (twinkle, way-up, wow) are
  sipped early and fade out; the descending sighs (hush, sleep) take over. The
  singer sinks an octave in the back third and the gaps between phrases widen —
  the piece itself falls asleep.
- **Four marimba voices**, all rung out to long decays (`decayMul` 1.3–2.6):
  `bass` (the rocking chair, deep + soft), `vibraphone_off` (the dream-haze pad,
  held thirds, 14 ms haas-widened), `rosewood` (the singer), `kalimba`
  (twinkles on phrase tails).
- A felted 5-voice **drone bed** underneath (stereo chorus + a fixed-filter
  sub) — every layer is pitched and musical: **no field recordings, no noise
  beds, no filter sweeps, no texture layers**. The old rain sample +
  brownian/air hiss went first (too repetitive, too noisy for sleep); the
  phaser sweep, the sub's LFO-swept "wub", and the felting noise-fizz
  followed (2026-06-11). Movement comes only from the chorus and each drone
  voice's own woozy vibrato/drift.

## Audio target

- **Length:** 10:00 final master (engine renders 10:10 and truncates to 600 s).
- **Master:** gentle sleep chain — highpass 22 Hz, +3 dB low shelf @ 85 Hz,
  −3.5 dB shelf above 3.2 kHz, one soft 2:1 glue compressor (no leveller), a
  measured LINEAR gain to **−16 LUFS / −1 dBTP**, with a 2 s fade-in and 5 s
  fade-out guarding the loop seam — nothing ever cuts abruptly.

## Files

- `bin/render-momabobasheep.mjs` — the generative renderer + inline sleep
  master. Reuses `mixEventMarimba` from the marimba lane.
- `bin/gen-illy.mjs` — cover generator. A felted naptime: one needle-felted
  wool doll of jeffrey alone, asleep in a nest of felt bedding, his little
  felt laptop dozing beside him.
- `momabobasheep.illy.txt` — cover prompt (lowercase fragments, papers voice).
- `out/momabobasheep.struct.json` — the chord walk (for any future visualizer).

## Run

```bash
# render + sleep-master in one go → out/momabobasheep.mp3
node pop/momboba/bin/render-momabobasheep.mjs

# write the master elsewhere
node pop/momboba/bin/render-momabobasheep.mjs --out ~/momabobasheep.mp3

# a different overnight — same parts, new random walk
node pop/momboba/bin/render-momabobasheep.mjs --seed nightfall

# cover (cached unless --force); embeds into out/momabobasheep.mp3
node pop/momboba/bin/gen-illy.mjs
node pop/momboba/bin/gen-illy.mjs --force
node pop/momboba/bin/gen-illy.mjs --embed-only
```

```bash
# scorodeon — watch the score fly by a fixed center line (vertical, story-ready)
node pop/momboba/bin/scorodeon-data.mjs   # events.json → scorodeon.json
node pop/bin/scorodeon.mjs pop/momboba/out/momabobasheep.scorodeon.json \
  pop/momboba/out/momabobasheep.mp3 --desktop          # --size WxH --zoom sec
```

Listen with QuickTime, not Apple Music:
`open -a "QuickTime Player" pop/momboba/out/momabobasheep.mp3`
