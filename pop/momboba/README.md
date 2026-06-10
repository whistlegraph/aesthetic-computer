# moronboba

**Moronbobasleep** — our first 10-minute extended **sleep mix**, a remix of
[`pop/marimba/marimbaba`](../marimba/). The calm nocturne twin of the lullaby,
the way [`pop/sleephellsine`](../sleephellsine/) is the calm twin of
[`pop/hellsine`](../hellsine/).

The difference: sleephellsine *abandoned* hellsine's instruments for pure sine.
Moronbobasleep keeps the **actual marimba synth** (`../marimba/synths/marimba.mjs`)
— that's what makes it a marimbaba *remix* and not a sleephellsine clone.
"Moron" = the lullaby gone dozy, dumb-happy, and very, very slow.

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
- A soft **brownian bed** underneath — two independent stereo random walks,
  slow 90 s sneak-in, dry — the room tone under the marimba.

## Audio target

- **Length:** 10:00 final master (engine renders 10:10; the master fades out
  over 14 s and truncates to 600 s).
- **Master:** same sleep chain as sleephellsine — highpass 28 Hz, −1.5 dB shelf
  above 4 kHz, `loudnorm` to **−28 LUFS** (TP −6, LRA 14), soft limiter at 0.35,
  **60 s fade-in** from silence, 14 s fade-out.

## Files

- `bin/render-moronbobasleep.mjs` — the generative renderer + inline sleep
  master. Reuses `mixEventMarimba` from the marimba lane.
- `bin/gen-illy.mjs` — cover generator. Inherits marimbaba's cover identity
  verbatim (jeffrey + Bill Gates at the Model M), recomposed as a sleepy
  nocturne — the two have drowsed off mid-task.
- `moronbobasleep.illy.txt` — cover prompt (lowercase fragments, papers voice).
- `out/moronbobasleep.struct.json` — the chord walk (for any future visualizer).

## Run

```bash
# render + sleep-master in one go → out/moronbobasleep.mp3
node pop/moronboba/bin/render-moronbobasleep.mjs

# write the master elsewhere
node pop/moronboba/bin/render-moronbobasleep.mjs --out ~/moronbobasleep.mp3

# a different overnight — same parts, new random walk
node pop/moronboba/bin/render-moronbobasleep.mjs --seed nightfall

# cover (cached unless --force); embeds into out/moronbobasleep.mp3
node pop/moronboba/bin/gen-illy.mjs
node pop/moronboba/bin/gen-illy.mjs --force
node pop/moronboba/bin/gen-illy.mjs --embed-only
```

Listen with QuickTime, not Apple Music:
`open -a "QuickTime Player" pop/moronboba/out/moronbobasleep.mp3`
