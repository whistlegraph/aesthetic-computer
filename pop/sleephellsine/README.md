# sleephellsine

A 15-minute **sleep mix** forked from [`pop/hellsine`](../hellsine/).
Where hellsine is the 2:42 hardcore single — D minor, 182 BPM, sampled
SFX, vocals, drums — sleephellsine is the calm winter twin: pure sine
voices walking the circle of fifths under a slow brownian rumble.

## The formula

The harmonic root walks the **circle of fifths** (C → G → D → A → E →
B → F# → C# → G# → D# → A# → F → C), one chord every 5 seconds. One
full rotation takes 60 seconds; the piece is 15 rotations.

**Two upper sine voices** each play a chord tone of the current chord
(the M3 and the P5). They oscillate sinusoidally around the chord's
midpoint in **contrary motion** — one cycle per chord, ±1.5 semitones
— so the voices cross **twice per chord** at the M3/P5 midpoint.

**At every crossing**, a long deep **root bell** fires in the C2 octave
(800 ms slow attack, 9 s exponential decay). The bell is the chord's
root, two octaves below the uppers; the long tails layer up into a soft
harmonic cushion underneath the moving uppers.

The **chord centre is voice-led** between adjacent chords with a 520 ms
one-pole glide, so the upper octave drifts smoothly between chords
instead of jumping. The mathematical formula always produces a chord
interval between the two voices.

## Engine

- Single C engine: `c/sleephellsine.c` (libc + libm + libpthread).
- Pure sine throughout — no samples, no drums, no vocals.
- AC "crosssies" detune (±2¢ pair per voice) keeps the chorus alive.
- "Wavery" ±6¢ random-walk wobble per voice for organic life on the
  held chord tones.
- Soft **brownian noise bed** underneath — independent stereo walks,
  slow 90 s sneak-in, dry (no reverb send).
- Schroeder reverb (4 comb + 2 allpass per channel, FB = 0.78, ~6 s
  tail) on the upper voices + root bells.

## Audio target

- **Length:** 15:00 final master (engine renders 15:10, bake fades out
  for 14 s and truncates).
- **Master:** -28 LUFS sleep target, TP -6 dBTP, soft limiter at 0.35,
  60 s fade-in from silence, 14 s fade-out, soft -1.5 dB shelf at 4 kHz
  to tame any bell-cluster brightness.

## Files

- `c/sleephellsine.c` — the engine.
- `c/build.sh` — builds `c/sleephellsine`.
- `bin/bake.mjs` — builds (if stale) → renders → loudnorm/limiter → wav
  + 320 mp3. Single command for a fresh master.
- `bin/gen-score.mjs` — generates the graphic score (PNG + PDF) and
  `out/sleephellsine-events.json` (crossing times, chord roots, chord
  names — for downstream tools).
- `bin/gen-illy.mjs` — cover generator (pixsies series identity).
- `sleephellsine.illy.txt` — cover prompt.

## Run

```bash
# render + master in one go (builds the C engine if needed)
node pop/sleephellsine/bin/bake.mjs

# regenerate the graphic score + events JSON
node pop/sleephellsine/bin/gen-score.mjs

# generate cover (cached unless --force)
node pop/sleephellsine/bin/gen-illy.mjs

# embed cover into the rendered mp3 after the fact
node pop/sleephellsine/bin/gen-illy.mjs --embed-only
```

Master output:
`~/Documents/Shelf/sleephellsine/sleephellsine-MASTER.wav`
and `sleephellsine.mp3`.
