# imab floor — draft 1 build log

`bin/floor.mjs` → `out/imab-floor-demo1.wav` + `.mp3` · 72 bars at 124
(2:22 with tail) · one grid, palindrome: bars 0–4 and 68–72 are the
same room (kick alone).

## bar map

| bars  | act | what plays |
|-------|-----|------------|
| 0–4   | kick alone | kit kick, four on the floor at 0.9 the whole record; one door tick at 0 |
| 4–8   | + closed hats | offbeat eighths |
| 8–12  | + open / shaker / sub | open-hat exhale (beat 4.5, every 2 bars), shaker 16ths (seed — ticks through everything until 66), offbeat C1 tanh sub |
| 12–16 | + bass / sines | marimba bass roots soft (0.35), sine choir 3 voices |
| 16–24 | **PASS 1** | sacredvox at T(16)+0.1 (dry, ×6.00 = the demo6 cap), bass full, vibraphone chords, wub on (0.14), reverse-bell into the door |
| 24–32 | lift | vox out; snare answers (repitched, beat 2.5 odd bars), sines 4v, shaker gathering into 32 |
| 30–40 | holyvox | half-time tissue enters at bar 30 (2-bar fade-in), haloed (vocal_bus reverb −14/1.6), its tail lands exactly at the drop door |
| 32–40 | **KICKLESS BREAK** | no kick/sub/bass/hats; sines 5v swelling (no pump — no kicks), shaker alone ticking; snare roll 36–40 (beats→halves→quarters), reverse-kick at T(40)−0.4, reverse-bell, biggest click rush (n=12, 1.6s) |
| 40–48 | **DROP · PASS 2** | everything: sacredvox, sines 6v, vibes, open hats every offbeat, wub 0.17, side-fold inhale T(40)−1.75→T(40) |
| 48–56 | peak | vox out; xylo chant echo — GT lines 1–2 up an octave in C (C5 G5 C5 C5 C5 · C5 C6 C5 C5 C5, kit one-shots) at 48/50/52/54; wub swells free |
| 56–64 | **PASS 3** farewell | sacredvox once more over the peel: vibes/xylo/snares gone at 56, sub+open+bass out at 60, sines fade 60–64, wub down to 0.10 and out |
| 64–68 | peel | closed offbeats thinning, shaker fading out by 66 |
| 68–72 | kick alone | plus one last hit at T(72), ringing into the fade |

Doors get click rushes (8, 16, 24, 32, 40, 48, 56, 64, 72 — softest
and widest last); kick turn-eighths lean in before 16, 24, 48, 56.
BREATH rests floor the decoratives in the last half-bar before each
door (0.34/0.52 alternating) — kick, sub, bass and both voices are
never gated. Kick-pump sidechain (45%, 90 ms) on sines/vibes/wub,
half-depth on bass.

## borrowed

- `loner/bin/v4pid/assemble.py` — the stage: `place()` ITD (≤0.6 ms) +
  ILD azimuth, depth via shelf+level; BREATH phrase-rest gates.
- `loner/bin/v4pid/gen-wub.py` — vocal-keyed wub: tanh sub on the
  roots (C2, +5 on F bars), wob alternating 4.07/6.10 Hz by bar,
  per-beat self-duck, then a 15 ms/230 ms follower on the sacred stem
  pulls it 2.8 dB down under voiced frames.
- `loner/bin/clubber360/gen-floor360.py` — acts, click-rush doors,
  turn figures, the roll into the drop, shaker-as-seed.
- `loner/c/cut-wax.sh` — the material chain (bass mono <120, width +
  slow drift, wow, tanh material, FM ceiling, drop inhale) reproduced
  inline so no temp files leave the lane; then the law: MEASURE
  (ebur128) → one static dB → true-peak limit 0.82. Never a second
  loudnorm.
- `bin/imabclub.mjs` / `bin/holyvox.mjs` — sub voice, sine choir
  voicings, pump constants, the holy halo call.

## measured (final print, wav = mp3)

- **I −11.7 LUFS** (target −11.5, static gain 2.96 dB after measure;
  the 0.82 limiter keeps ~0.2 LU) · **LRA 13.3 LU** · **TP −1.6 dBFS**
- act RMS arc: intro acts −19.7 → bass act −11.4 → body −8.7…−9.2 →
  break −13.0 → PASS 3 −10.6 → peel −19.8 (palindrome holds)
- premaster peak 4.53 at 37.0 s (a vocal consonant in PASS 1, bar 19);
  normalize is body-referenced (p99.9), the wax tanh folds the
  outlier at 1.20 — the voice itself stays untouched.
- sacred vocal gain hit the demo6 law's own cap (×6.00 — identical
  numbers to the confirmed demo6 render). Zero pitch work, no halo,
  placed whole at 16 / 40 / 56.

## open questions for @jeffrey

1. **Three passes or two?** PASS 3 (bar 56) sings the hook over the
   peel as a farewell. If the record should empty out wordless, drop
   it and let the peel start at 56.
2. **The bar-12 step.** The marimba bass dominates RMS — even at 0.35
   its arrival is +8 dB. Stretch the assembly (bass at 12, chords at
   14) or keep the two-stage door (harmony at 12, voice at 16)?
3. **Snare answers** (24–32, 48–56) are the closest thing to a
   backbeat here — clubber360 used rims instead. Keep, thin, or cut?
4. **Wub audibility**: 0.17 under the drop with the follower pulling
   it under the voice — worth a solo listen; it may want +2 dB or a
   second harmonic.
5. **LRA 13.3** is wide for a juke print (the palindrome ends quiet).
   If juke shuffle play makes the intro/outro read as dead air, the
   kick-alone acts could come up ~3 dB at the cost of the palindrome's
   hush.
6. imabclub-draft1 (96 bars, holyvox-led) still exists as the longer
   sibling — this 72-bar cut is the sacredvox floor. Which spine gets
   draft 2?
