# sleephellsine

A 15-minute minimal-sine **sleep mix** forked from
[`pop/hellsine`](../hellsine/). Where hellsine is the 2:42 hardcore-arc
single — D minor, 182 BPM, ULTIMATE strategy, layered sampled SFX
(kicks, drums, crow, splash, grenade, train, vocals…) — sleephellsine
is the calm winter twin: same D-minor leitmotif and chord world,
but stripped to **pure sine voices** and slowed to a near-stationary
crawl.

## What's in (the sleep palette)

- `voice()` — sine-triad pad voices with soft attacks (0.45-0.85s),
  long releases, gentle vibrato. Carries the chord wash + the soft
  theme + counter restatements.
- `bell()` — additive-partial sine bells with very long exponential
  decay (~5.5 s τ), heavy wet-bus send, no fizzle. One ping on the
  downbeat of every 4th bar.
- `sub()` — optional barely-there sub heartbeat, one pulse every 8
  bars, smooth fade-in. Disabled with `--heartbeat off`.
- A **Schroeder reverb** (4 comb + 2 allpass per channel, ≈ 6 s tail,
  0.58 wet) sitting over the whole mix.

## What's out (vs hellsine)

- No `kick()`, `snare()`, `steam()`, `sawLead()`, `hoover()`, `stab()`,
  `tick()`, `woodTick()`, `riser()`, `piano()`, `bubble()`.
- No samples — no crow, splash, grenade, train, drum break, scratches,
  cards, rattle, shakes, vocal stamps, jeffrey-pvc, Tri-Tone ding,
  melon stab, typewriter, I-need-you, AC stamp, etc.
- No vocals. No drums. No drops. No buildups. No climax. No tempo ramp.
  No scratch lane. No acdsp character pass.

## Audio target

- **Tempo:** 42 BPM (1/4 of hellsine's 182).
- **Form:** 7 sections × 24 bars + 14 s tail ≈ 960 s engine, truncated
  with a 14 s fade-out to **15:00** final.
- **Master:** -18 LUFS, TP -3 dBTP, soft limiter (limit=0.75), no
  loudness boost.

## Files

- `bin/sleephellsine.mjs` — the engine. Pure sine. Writes 32-bit float
  pre-master WAV (no struct.json — there's nothing else to align to).
- `bin/bake.mjs` — engine → loudnorm/limiter → wav + 320 mp3. No
  scratch lane, no acdsp character pass.
- `bin/gen-illy.mjs` — cover generator. Same identity refs +
  pals-logo.png as the rest of the pixsies series.
- `sleephellsine.illy.txt` — cover prompt. Same composition as
  hellsine.illy.txt (felt-craft jeffrey + grad-student pixsies with
  PALS-glowing laptops, smooshed wide-angle group portrait, ice
  bullet train + ice unicorn-ish horse on the horizon) BUT every
  surface is now frozen / kawaii / peaceful: lava → ice sine channels,
  flames → frosted-glass cones, fire eyes → closed felt eyelids or
  cool star-glow, hellscape → calm winter dream under moonlight.

## Run

```bash
# render + master in one go
node pop/sleephellsine/bin/bake.mjs

# generate cover (cached unless --force)
node pop/sleephellsine/bin/gen-illy.mjs

# embed cover into the rendered mp3 after the fact
node pop/sleephellsine/bin/gen-illy.mjs --embed-only
```

Master output: `~/Documents/Working Desktop/sleephellsine/sleephellsine-MASTER.wav`
