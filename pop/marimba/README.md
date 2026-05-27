# marimba

Pop lane exploring synthesized marimba. Bottom-up modal synthesis — no
samples, no Kontakt libraries. Lives next to the dance / hippyhayzard /
chillwave / hellsine / jungle lanes.

## the synth

`synths/marimba.mjs` — modal additive synthesis with mallet impulse +
tube resonator. Same module shape as `../dance/synths/sinepower.mjs` and
`../hippyhayzard/synths/zitar.mjs`.

```
mixEventMarimba(ev, out, { sampleRate, preset })   // node bed
renderMarimba(ev, { sampleRate, preset }) -> f32   // bare DSP
playMarimba(sound, ev, { preset })                 // AC sound.synth fallback
```

### presets

Core marimba (the lane's reason for being):

| preset    | partials        | character                                |
|-----------|-----------------|------------------------------------------|
| rosewood  | 1 : 4 : 9.2     | classic concert marimba, warm, full tube |
| kelon     | 1 : 4 : 10      | synthetic-bar, slightly brighter         |
| bass      | 1 : 4           | low extension, mostly tube, soft mallet  |
| staccato  | 1 : 4 : 10 : 17 | hard dry "tok," no resonator             |
| roll      | 1 : 4 : 9.5     | short ring for tremolo / sustained lines |

Tuned-percussion family (same engine, different partial ratios — full
mallet stack from one model):

| preset           | partials         | character                          |
|------------------|------------------|------------------------------------|
| xylophone        | 1 : 3 : 6        | bright, short ring, hard mallet    |
| vibraphone       | 1 : 4 : 10       | metal bars, very long decay + motor|
| vibraphone_off   | 1 : 4 : 10       | same but motor disengaged          |
| glockenspiel     | 1 : 2.76 : 5.4   | high steel, mode-2-dominated pitch |
| gamelan          | 1 : 2.4 : 4.7    | bronze, inharmonic, long ring      |
| woodblock        | 1 : 1.8 : 2.7 :… | barely pitched, mostly attack      |
| kalimba          | 1 : 5.9 : 8.1    | thumb-piano tine, near-pure mode 1 |

### CLI demo

```
node pop/marimba/synths/marimba.mjs                           # rosewood
node pop/marimba/synths/marimba.mjs --preset kelon
node pop/marimba/synths/marimba.mjs --preset bass --out ~/m.mp3
```

Renders a short showcase phrase across the marimba range to
`pop/marimba/out/marimba-<preset>.mp3`. Listen with QuickTime, not Apple
Music: `open -a "QuickTime Player" pop/marimba/out/marimba-rosewood.mp3`.

## the song

`marimba.np` — TBD. A short investigative piece that puts the four
presets to work in their natural roles (rosewood lead, bass under,
staccato accents, roll for a hold).

## marimbaba++ — the disco remix

`marimbabapp.np` — the lullaby grows up and goes to the club. Same
F-major DNA as `marimbaba`, re-barred from a slow 56 BPM 3/4 nap into
a driving **6/8 disco-shuffle** (~115 BPM dotted-quarter). The triple
feel survives the move — 6/8 is just a compound 3/4 — so the
"twin-kle" and "ba-ba-ba bap" syllables still scan over a
four-on-the-floor.

A ~6:07 club arc in 8 movements: intro → groove A → break → groove B
(the full hook) → breakdown → build → drop → outro. The drums and the
disco-octave bass are procedural DSP; the marimba presets carry the
melody, chicken-scratch chord stabs, sparkle, and fills. The
breakdown introduces **"the float"** — a calmer bridge melody not in
`marimbaba` — and the final drop **lifts the whole track +2 to G
major** at bar 264 for the classic disco key-change euphoria. A
kick-triggered sidechain pumps the bass / pad / comp.

```
node bin/render-marimbabapp.mjs                  # → out/marimbabapp.mp3
node bin/render-marimbabapp.mjs --out ~/m.mp3
node bin/render-marimbabapp.mjs --bpm 122        # tempo variation
```

Section bounds land in `out/marimbabapp.struct.json` (same shape as
`marimbaba.struct.json`). `marimbaba` itself — the lullaby + its
storyline visualizer — is untouched.

## the storyline visualizer

`marimbaba` ships with a 9:16 Instagram-Story visualizer that tells one
quiet story across the lullaby — jeffrey's late-night **computer help
call** with Bill Gates. **10 colored-pencil + gouache panels**, two per
`.np` section, with a changing-emotion arc:

| § | beat | emotion |
|---|------|---------|
| 0 | hush1 — jeffrey approaches the house at night | anticipation |
| 1 | hush2 — the doorway hello | shy warmth |
| 2 | twinkle1 — crossing the room, Gates greets him | relief |
| 3 | twinkle2 — settles into a chair at the Model M | cozy |
| 4 | wow1 — points out the first step | uncertainty |
| 5 | wow2 — it clicks | wonder |
| 6 | baba1 — Gates tries it himself | anxious effort |
| 7 | baba2 — hands-on, together | teamwork |
| 8 | sleep1 — it worked | triumph |
| 9 | sleep2 — the drowsy close (the album cover) | calm |

```
node bin/render-marimbaba.mjs --no-open   # → audio + out/marimbaba.struct.json
node bin/gen-sections.mjs                 # gpt-image-2 → cover + 10 panels (cached)
node bin/gen-sections.mjs --force         # regenerate all
node bin/gen-sections.mjs --only wow2     # one panel
node bin/preview-score.mjs                # → out/marimbaba-preview-score-portrait-insta-story.mp4
```

`gen-sections.mjs` inherits medium + identity verbatim from
`marimbaba.illy.txt`; each beat variant re-stages the composition and
carries its own emotion. `render-marimbaba.mjs` emits
`out/marimbaba.struct.json` — 10 story sections + the marimba's note
events per voice lane. `preview-score.mjs` builds the mp4 on the shared
engine (`pop/lib/preview-shared.mjs` + `cover-engine.mjs`):

- **the string + sound elements** — a verlet-physics playhead string
  down the centre; the note events ride past it as pitch-coloured
  blocks across 4 voice lanes, each note plucking the string;
- **3-layer backlight** — warm glow from behind + a contrast vignette
  (so the figures read backlit, not lit-on-top) + a per-figure halo;
- **face zoom** — the camera eases wide↔face twice per section,
  alternating jeffrey / gates;
- **6 transitions** — iris / blinds / push / zoom-punch / pixel /
  diagonal-wipe, cycled across the 9 boundaries.

Figure bounding boxes for the face zoom + backlight live in
`marimbaba-forms.json` — re-eyeball a box if its panel is regenerated.

## sample sources

Most of `marimbaba` is pure synthesis, but a few sourced samples sit in
`assets/`. All are CC0 / public-domain (commercial-safe — see
`pop/SCORE.md`). The canonical registry is `pop/lib/menu.mjs` →
`sample_sources`; per-track provenance is also logged in
`out/.sfx-credits.txt`.

| sample              | source                                      | license |
|---------------------|---------------------------------------------|---------|
| manhattan-siren.mp3 | Freesound #223824 "Wail and Yelp.mp3" (WBJB1)| CC0     |

`assets/whip-crack.*` and `assets/lamb-bleat.*` are optional — drop a
CC0 sample in and the renderer picks it up automatically.

## research notes

See `STUDY.md` for the physics / DSP background and the literature
trail behind the preset numbers.
