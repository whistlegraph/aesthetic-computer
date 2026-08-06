# Aesthetic Wing

Real chill street aviation.

## What it is

Aesthetic Wing is a speculative personal electric vehicle that combines a
self-balancing one-wheel ground vehicle with short, bounded, computer-stabilized
hover. Most of the time it rolls. When the surface changes, a guarded distributed
lift system progressively unloads the wheel, crosses the interruption, and settles
back onto the street.

The product idea is deliberately smaller than a flying car. It proposes a new
gesture between riding and flying: the **air step**.

## Campaign thesis

> A little more street, in every direction.

The campaign follows the visual and documentary grammar of
`comodiddies/twofa`: Jeffrey uses an implausible future product as if it were an
ordinary part of his morning. The humor and desire come from calm use rather than
spectacle.

The generated drawings are concept illustrations, not evidence of a working
vehicle. Human-carrying testing is outside the depicted Phase I program.

## Files

- `proposal.md` — non-confidential funder-facing concept proposal.
- `funding-roadmap.md` — current 2026 funding paths and staged raise strategy.
- `gens/v1-hover-hero.png` — principal hover illustration.
- `gens/v2-roll.png` — ordinary rolling mode.
- `gens/v3-air-step.png` — bounded curb transition.
- `gens/v4-design-bible.png` — accepted road/lift/underside industrial design.
- `gens/v5-wing-wakes.png` — lift petals opening and wheel unloading.
- `gens/v6-more-lift.png` — sustained eight-foot controlled hover.
- `gens/v7-settle.png` — gentle wheel-first landing.
- `gens/v8-road-mode.png` — redesigned compact road mode.
- `prompts/` — exact GPT Image 2 production prompts.
- `refs/` — approved Jeffrey identity references used for generation.
- `product/sheet.tex` — printable two-page concept sheet.
- `story/vo-v1.txt` — revised Jeffrey voiceover copy.
- `story/sound.mjs` — original AC music and wheel/lift/landing sound synthesis.
- `story/gen-motion.mjs` — fal.ai Seedance 2.0 shot driver.
- `story/storyboard.md` — reel timing, captions, motion, and truth constraints.

## Rebuild

From the repository root:

```bash
cd marketing/campaigns/aesthetic-wing/product
xelatex -interaction=nonstopmode -halt-on-error sheet.tex

cd ../story
node ../../../../pop/bin/say.mjs vo-v1.txt --out vo-v1.mp3 --timestamps
node sound.mjs
node build.mjs
node gen-motion.mjs --dry-run
```

`say.mjs` uses the production Jeffrey voice and costs real money when the
content-addressed cache is cold. `build.mjs` recreates its ignored `build/`
directory and writes `aesthetic-wing-reel-v1.mp4` with a 48 kHz stereo mix.
The Seedance dry run writes the shot manifest and reports cost without submitting
generation. The current five-shot fast/720p pass is approximately \$7.26.

## Reel shape

The accepted v1 reel is a 26.7-second 9:16 sequence:

1. compact road mode
2. wing-petal deployment
3. sustained eight-foot hover
4. controlled settle
5. industrial-design folio

As with the 2FA Brush reel, all cuts and captions derive from the ElevenLabs
word-alignment sidecar. The score and sound effects are synthesized locally from
code: modal AC tones, prompt bells, tire texture, guarded-fan lift/downwash, and
a wheel-first landing cue.

## Safety and truth

- No public-street testing is proposed.
- No human-carrying Phase I testing is proposed.
- The eight-foot scene is a controlled design vision in an empty flood channel,
  not evidence of testing or authorization for public operation.
- Rotor guards, bounded height, automatic settling, and controlled test venues
  are part of the product story rather than fine print.
- Performance figures are design targets until supported by measured test data.
