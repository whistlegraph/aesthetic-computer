# hellsine

a concept track. the whole point is one constraint:

> **every voice is a sine wave. nothing else. no noise, no saw, no
> square, no samples.**

`hellsine` = the sine that went to hell. the "hell" is what hard
saturation does to a pure sine — and that distorted sine **is** the
gabber kick. so "all-sine + hardcore" is not a contradiction. it is
hardcore's literal DNA. the Rotterdam kick is a pitch-enveloped sine
clipped past its own skin.

the research question this lane exists to test:

> can a complete **hardcore** track — carrying a full **John Williams
> melodic structure** — be built from nothing but `Math.sin`, and still
> work as **epic study music** (continuous, immersive, no dead air)?

bottom-up + compositional, the strictest possible reading of the `pop/`
posture: one waveform, a whole genre, a whole compositional tradition.

## the all-sine law

a voice is legal iff its sample value is a sum of `sin()` terms (plus
envelopes and **memoryless waveshaping** of those sines). allowed:

- additive sine partials (brass / strings / pad / bell)
- sine-on-sine **FM** (the metallic stab, the hoover scream)
- **pitch-enveloped sine** (the kick body, the riser)
- **memoryless saturation** of a sine — `tanh`, hard clip, foldback —
  the harmonics are *derived from* the sine, so it stays legal. this is
  the kick. this is the "hell".
- ring-mod = product of two sines → still sines.

illegal, no exceptions: white/pink noise, sampled hats/breaks, saw or
square oscillators, any audio file decoded as a source. percussion that
is "usually noise" (hats, claps) is re-derived here as **very high,
very short sine blips** — honoring the law literally is the point.

## the voices (all sine)

| voice    | how it's a sine |
|----------|-----------------|
| **kick** | sine, pitch env ~250→45 Hz over ~35 ms, then `tanh(drive·x)` hard-clipped — the gabber/Rotterdam kick. `drive` is the *hell knob*. |
| **sub**  | low sine at the root, sidechain-ducked under each kick (the pump). |
| **brass**/**strings** | additive sine partials (1·2·3·4·5, odd-harmonic bite for brass), vibrato, octave doublings — carries the **leitmotif**. |
| **hoover** | a few detuned sines + rising sine-on-sine FM index + saturation — the aggressive lead, all sine. |
| **stab** | 2-op sine FM (sine modulates sine, high index), clipped — the hardcore stab. |
| **perc** | 8–12 kHz ultra-short sine blips (sub-ms decay) = the tick/hat; ring-modded sine pair = metallic open hat. |
| **scream/riser** | sine swept up a bar with rising FM index + saturation — the build. |

## form — John Williams × hardcore × epic study

- **tempo** 182 BPM, 4/4 (hardcore range)
- **key** D minor (Williams' heroic register), with a step-up **key
  lift** at the final restatement
- **theme** a hand-written, singable heroic leitmotif: a leap up, an
  expressive stepwise descent (Williams hallmark). a contrasting,
  lyrical **B theme** is the calm "study" core.
- **arc** (continuous — no dead air, loop-friendly tail; the "study"
  use-case lives on uninterrupted immersion under a film arc):

  ```
  overture   theme hinted in soft sine strings, no kick
  statement  theme in full sine "brass" + the gabber kick enters
  bridge     B theme, kick thins to a pulse — the study zone
  develop    theme fragmented + sequenced through related keys, hoover
  climax     full gabber kick + theme restated, transposed up (key lift)
  coda       theme dissolves back to strings, continuous fade
  ```
- **length** ~1:45 (radio edit, slightly long for the epic-study form)

## source → track

```
pop/hellsine/bin/hellsine.mjs   the all-sine engine (deterministic seed)
  → <out>/hellsine-MASTER.wav   engine → scratch-mix post-FX → Spotify finalize
  → <out>/hellsine.mp3          320k
```

deterministic: same seed + same flags = byte-identical audio. the
theme/harmony are hand-shaped *data*, not random — "highly melodic" is
composed, not stumbled into.

## pipeline

```bash
node pop/hellsine/bin/bake.mjs            # render → post-fx → master
node pop/hellsine/bin/bake.mjs <outDir> -- --hell 14 --bpm 182   # tune
```

reuses `pop/dance/bin/scratch-mix.mjs` (genre-agnostic beat-locked
post-FX, anchored to this engine's own `struct.json` grid) and a
`bake.mjs`-shaped finalize chain (loudnorm I=-14 / TP=-1.5 / limiter /
end fade), same as the dance lane.

> mastering note (inherited, see `pop/RELEASES.md`): pure-sine mixes
> render **dark**. hardcore saturation adds upper harmonics so hellsine
> starts brighter than the trance bed, but A/B every master against a
> brightened take before release.

## tracks

- **hellsine** — the concept track itself. status: in flight (see
  `pop/RELEASES.md`).

---

*maintained by @jeffrey*
