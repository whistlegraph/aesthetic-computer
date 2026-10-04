# Notepat for Strudel

[Open the demo](https://pat.aesthetic.computer), then press Play.
All four tracks enter on the first beat: tonic melody, a fifth above it,
a chord, and bass. Random rests only occur after that opening.
The same address evaluates a new seed every ten minutes: key, notes, and rests
change when you open it or press Update. During playback, Perlin noise and
different signal periods evolve timbre, filters, stereo position, and levels.
The key does not abruptly change at a clock boundary during an existing run.
The source includes annotated, independently commentable tracks and a solo lab
for Will. Replace `Date.now()` with a fixed timestamp to keep a variation.
To use the instrument in another pattern:

```js
await import('https://pat.aesthetic.computer/s')
note("c3 eb3 g3 bb3").s('ac_glass').n("<0.2 0.8>")
```

Marimbaba arrangements:

- [Marimbaba](https://pat.aesthetic.computer/marimbaba): the written 24-bar
  melody at 56 BPM in 3/4, with a reduced marimba/bass/chord arrangement.
- [Marimbaba Orbit](https://pat.aesthetic.computer/marimbaba-orbit): the same
  melody at 84 BPM, with a feedback-FM double and moving stereo/delay.

Append `/source` to either URL for annotated import-based source, or `/paste`
for the self-contained version. These adapt `pop/marimba/marimbaba.np`, not
the released master and its later renderer additions. `build-tracks.mjs`
translates every score token and checks that the five sections total 72 beats.
Attribution: **@jeffrey / aesthetic.computer**.

[Stone Wattajetta](https://pat.aesthetic.computer/stone) is a compact instrumental
reinterpretation at 132 BPM: twelve active parts using ten AC voices. The
editable score is 49 lines / 2,528 UTF-8 bytes, down from 88 / 4,714, while the
number of distinct sounds doubles from five to ten. Strudel's `scale`,
`palindrome`, `struct`, and `euclidRot` replace the JavaScript score generator.
The E/G/A/D roots retain their 18-bar cycle; a rephrased 17-note stone melody
crosses a seven-note Orbit counterline, marimba answers, Bloom chords, and
Notepat fifth stabs. Sub/kick/bells enter immediately; all parts enter in bar one.
Shakers grow through 5/7/9/11 hits over 72 bars and rotate five steps per bar;
independent signals evolve brightness, ring, stereo, and chord level.

Annotated [source](https://pat.aesthetic.computer/stone/source) and self-contained
[paste](https://pat.aesthetic.computer/stone/paste) remain available for Will.
The paste embeds the synth library and is consequently larger than the editable
import-based score. `stone-score.strudel` preserves the earlier cursor-based
adaptation locally. Neither arrangement reproduces the current 144-bar club
master, accelerando, FEM bodies, vocals, or found effects.
Pattern reference: [Strudel time modifiers](https://strudel.cc/learn/time-modifiers/).

| Sound | Recipe |
| --- | --- |
| `ac_stone` | Designed inharmonic bar, modes 1:2.756:5.404; `.n()` changes brightness/ring; an approximation, not FEM |
| `ac_water` | Rising sine chirp, 0.55× to target pitch; `.n()` changes droplet length |
| `ac_kick` | Falling sine, roughly 126→43 Hz; `.n()` changes pitch attack and decay |
| `ac_marimba` | Rosewood modes 1:4:9.2, independently decaying; `.n()` controls brightness and ring |
| `ac_orbit` | Delayed feedback FM, four interlocking pulses, moving stereo and modulation depth |
| `ac_glass` | Metallic FM, triangle fifth, inharmonic partial, decaying brightness |
| `ac_swarm` | Detuned saws, triangle fifth, sine sub, drifting pitch |
| `ac_bloom` | Triangle/sine layers, gentle FM, moving stereo position and filter |
| `ac_triangle_fifth` | Triangle plus a 1.5× fifth, levels 0.42 + 0.12 |
| `ac_triangle` | Single triangle |
| `ac_sine` | Single sine |
| `ac_major` | Sine chord, semitones 0, 4, 7 |
| `ac_minor` | Sine chord, semitones 0, 3, 7 |
| `ac_sus2` | Sine chord, semitones 0, 2, 7 |

The original six recipes come from `fedac/native/pieces/notepat.mjs`: the touchscreen
triangle Shift alternate and keyboard Control/Alt chords. Web Audio uses
band-limited oscillators; native mixer saturation, echo, and speaker coloration
are not included. Chords are level-scaled for headroom. Glass, swarm, bloom, and
orbit are new extensions of those sounds. On these four, `.n(0)` through `.n(1)`
changes FM depth, detuning, and brightness; it can be patterned.

FM, feedback, detuning, envelopes, LFOs, and seeded composition are established
techniques. This package's identity is the particular Notepat-derived recipes,
their tuning/layer balance, and their evolving arrangement, not a novel
synthesis algorithm. Filters, reverb, delay, and pattern signals in the example
come from Strudel. Custom voices do not implement every built-in synth control;
use `.n()` for their internal modulation rather than Strudel's `.fm()`.
The marimba is a modal reduction of `pop/marimba/synths/marimba.mjs`; it omits
that renderer's mallet convolution and resonator-tube simulation.

Use `note()` or `freq()` for pitch and Strudel's `attack`, `decay`, `sustain`,
`release`, `gain`, `pan`, filter, delay, and reverb controls. The original voices
default to a 20 ms attack, full sustain, and 100 ms release; the new voices
have their own envelopes. Water and kick use `.n()` for their percussive decay;
they accept attack/release but ignore sustain/decay. The kick has a fixed
43 Hz destination regardless of the pattern's note. No external code dependencies
are downloaded by the module; it registers with the running Strudel audio API.

For a copy-and-paste version, paste all of [pat.aesthetic.computer/paste](https://pat.aesthetic.computer/paste)
(`notepat-paste.strudel`) into Strudel.
It contains the same voices and example, with no hosted import. Regenerate it
and the two Strudel links with `node ac-strudel/build.mjs`.

`example.strudel` is the complete example, also served at `/source`.
`node ac-strudel/build.mjs && node ac-strudel/deploy.mjs` publishes the dedicated
`ac-strudel-notepat` Cloudflare Worker on AC's account. The Worker bundles the
source, permits cross-origin imports, and redirects `/` to Strudel's example
URL. It stores no user data and makes no upstream requests. The original
`assets.aesthetic.computer/strudel/` files remain a compatibility mirror.

API reference: https://strudel.cc/technical-manual/sounds/

Validation: `node ac-strudel/verify.mjs` opens the actual Strudel editor in
headless Chrome with a local module response; `LIVE=1 node ac-strudel/verify.mjs`
checks the published URL. Both exercise the import and paste versions. Offline
audio checks cover non-silent output, triangle/fifth spectral levels, bounded
peaks, silent tails, timbre changes at both macro extremes, and exactly-once
cleanup after normal and early stops, including every modulation oscillator.
The scheduler check also verifies simultaneous first-beat onsets from all
four demo tracks, and treats Strudel's logged `getTrigger` errors as failures.
`node ac-strudel/verify-tracks.mjs` checks both Marimbaba arrangements and their
standalone versions against all 50 pitched events in the original score;
`LIVE=1` checks the published synth. All active tracks must trigger together.
`node ac-strudel/verify-stone.mjs` checks import/paste playback, immediate
entrances from all twelve parts, ten distinct voices, valid pitches across
72 bars, uninterrupted kick/gallop, changing shaker density, and timbre motion.
