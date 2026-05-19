# Pop song pipeline audit — trancenwaltz vs undabeach

_Generated 2026-05-16. Scope: the two pop songs currently in the repo —
**dance/trancenwaltz** and **chillwave/undabeach** — their pipelines,
what is common in the formats, and a plan to bring undabeach up to the
effect + prompting set trancenwaltz just gained._

---

## 1. The two songs at a glance

| | **trancenwaltz** (`pop/dance`) | **undabeach** (`pop/chillwave`) |
|---|---|---|
| Mood | dark / emo / extreme war-arc | chill / sunny beach |
| Audio gen | `recap/bin/trance.mjs` — procedural **synth composition** → mp3 + `struct.json` | `pop/chillwave/bin/render.mjs` — **`.np` score** → mp3 |
| Score source | section templates inside `trance.mjs` | `pop/chillwave/undabeach.np` |
| Illustration gen | `marketing/bin/gen-promo.mjs` — **per-section** campaign dirs (9), jeffrey refs + campaign refs | `pop/chillwave/bin/gen-illy.mjs` — **single cover**, jeffrey refs |
| Prompt source | 9 × `…/trancenwaltz-sections/<sec>/cover-prompt.txt` (shared header/trailer + unique `SECTION:` line + `refs/`) | 1 × `pop/chillwave/undabeach.illy.txt` |
| Video render | `pop/dance/bin/cover-video.mjs` (2035 ln) | `pop/chillwave/bin/preview-score.mjs` (1011 ln) |
| Orchestrator | `pop/dance/bin/build.mjs` (versioned `~/Desktop/builds/<name>/bNNN/`) | none — manual `render.mjs` + `preview-score.mjs` |
| Preview player | `pop/dance/bin/preview.sh` + `hover-loop.lua` (borderless, hover-to-play, loop) | none |
| Shared lib | `pop/lib/preview-shared.mjs` (YWFT typography, ken-burns, waveform-in-events, BGRA, audio decode) | same `pop/lib/preview-shared.mjs` |

---

## 2. What is common in the formats (the shared spine)

Both songs follow the same **audio → struct → per-frame Canvas2D cover
video → ffmpeg BGRA → mp4** spine:

- **Struct/event model.** Audio render emits a `struct.json` of lane
  events (`kick/hat/snare/sub/lead/bells/piano/supersaw/…/sfx/vox`,
  `dropImpact`, sections with `startSec/endSec`). The video reads it.
- **YWFT-Processing-Bold typography**, rasterised via ImageMagick
  (Cairo/fontconfig can't resolve the TTF) — title chars + timecode
  + lane/section labels. Centralised in `pop/lib/preview-shared.mjs`.
- **Screen-blended waveform events** drawn into the illustration
  (no boxes), playhead-driven press/held/played alpha.
- **node-canvas → rawvideo BGRA → libx264 + AAC** encode path.
- **jeffrey identity refs** (SHOOT + SELFIE corpus) on every gpt-image
  call, same path as `recap/bin/jeffrey-photos.mjs`.
- **Ken-burns** illustration motion, **segmented bottom progress bar**,
  **bottom-right timecode**, fixed scene + rotating "track group".
- Lowercase / fragment **papers VOICE.md** prompt register.

undabeach is effectively the **reference implementation** of the
chrome (verlet string, rotating disc, segmented bar, YWFT timecode,
chunky bar-segment waveform) — those were ported _from_ it into
trancenwaltz this cycle.

---

## 3. Where they diverge

### Audio
- trancenwaltz is **fully procedural** (synth section-templates in
  `trance.mjs`) and has a **scripted opening/closing sonification**:
  AC keyclick typing intro → boot melody → sniper → music; tape-stop
  + shutdown melody at the end. It mixes a **jeffrey-pvc sung vocal
  stem** (`pop/dance/bin/sing.mjs`).
- undabeach renders a **fixed `.np` score** (`render.mjs`), no scripted
  boot/shutdown story, no vocal stem path.

### Illustration / prompting sequencing
- trancenwaltz has **per-section prompt sequencing**: a war arc across
  9 sections (intro → drops → outro), each its own campaign dir with a
  shared header/trailer (medium, butterfly-on-laptop-lid rule,
  PALS-only pixie laptops, diverse casting) + a unique `SECTION:`
  beat + curated `refs/`. Section illustrations **crossfade / doom-melt
  on the musical drop**; a head-down "prelude" swaps in before the
  first hit. Versioned (v8…v12, never overwritten).
- undabeach has a **single `undabeach.illy.txt`** cover. No
  per-section set, no crossfade, no prelude, no campaign refs system.

### Effects present in trancenwaltz, ABSENT in undabeach
1. **Stained-glass transmissive backlight** — lane hits light the
   illustration _through_ a per-illustration luminance transmission
   mask (additive transmitted pass + multiply leaded-contrast pass),
   hue drifting, amplitude-reactive.
2. **Two-layer parallax depth** + env shake.
3. **Music-driven fine-row slitscan** that swells into each drop.
4. **Section illustration crossfade / doom-melt** snapped to
   `dropImpact`.
5. **Fullscreen pixelate / fuzz / blink decay** for **startup &
   shutdown**, beep-synced (no geometry squash).
6. **AC pink-prompt typing cold-open** — keyclick audio lead-in in
   `trance.mjs` + the prompt block typing the title in cover-video.
7. **Prelude swap** (head-down/laptops-closed → first gunshot).
8. **Env-bounce zoom** framing (pulled back, pumps on loudness).
9. **`--probe-frames`** still-render mode for ~10 s iteration.
10. **Versioned build orchestrator** (`build.mjs`) + **clean hover
    preview** (`preview.sh` + `hover-loop.lua`).
11. **gpt-image retry/timeout resilience** (`gen-promo.mjs`).

### Effects present in undabeach, matched in trancenwaltz this cycle
verlet-string playhead, rotating-disc, YWFT timecode w/ section-tint
recolor + env bump, full-width segmented progress bar, chunky
bar-segment waveform, string-bend on the bars. (Ported across.)

---

## 4. Risk / divergence notes

- **Two cover-video engines.** `cover-video.mjs` (2035 ln) and
  `preview-score.mjs` (1011 ln) now overlap heavily (string, disc,
  timecode, bar, progress) but are **forked copies**, not a shared
  module. Each new effect has to be hand-ported (this cycle proved
  that — verlet/disc/bar were copied by hand). This will keep
  drifting.
- `pop/lib/preview-shared.mjs` only holds typography + ken-burns +
  audio helpers — **not** the chrome/effects. The valuable new
  effects (backlight, slit, decay, typing-intro, bar model) live
  only in `cover-video.mjs`.
- undabeach audio (`render.mjs`, `.np`) and trancenwaltz audio
  (`trance.mjs`, procedural) are **different engines**; the boot/
  shutdown/keyclick sonification is trance-only and not trivially
  reusable without a shared SFX helper.

---

## 5. Recommended plan (port the new set to undabeach)

**Phase 1 — extract a shared cover engine (highest leverage).**
Lift the now-canonical effect set out of `cover-video.mjs` into
`pop/lib/cover-engine.mjs`: transmission-mask backlight, two-layer
parallax, fine-row slitscan, doom-melt section crossfade, pixelate
startup/shutdown decay, typing cold-open, verlet string (+colour
+bend), beach bar waveform, rotating disc, env-bounce zoom,
`--probe-frames`. Both `cover-video.mjs` and `preview-score.mjs`
become thin per-song configs (lanes, palette, prelude, illo map).

**Phase 2 — give undabeach per-section prompting sequencing.**
Mirror the dance campaign-dir model: `pop/chillwave/undabeach-sections/<sec>/{cover-prompt.txt,refs/}`
with a shared header/trailer + per-section beat, generated via
`gen-promo.mjs` (already retry-resilient), versioned. Wire a section
illustration map + crossfade into the (now shared) engine.

**Phase 3 — shared opening/closing sonification helper.**
Factor the keyclick / boot-melody / shutdown-melody synth + event
emission out of `trance.mjs` into a small helper both audio engines
(`trance.mjs` procedural and `render.mjs` .np) can call, so undabeach
can get the same AC typing cold-open + pixelate startup if desired
(chill variant: gentler, optional).

**Phase 4 — adopt the orchestrator + preview for undabeach.**
Add an undabeach CONFIG to `build.mjs` (versioned builds) and let
`preview.sh` pick up undabeach builds. One pipeline, two songs.

**Net:** after Phase 1+2 a new effect is written once and both songs
get it; a new song is a config + a `.np`/template + section prompts.

---

## 6. File map (quick reference)

```
pop/lib/preview-shared.mjs          shared: YWFT, ken-burns, waveform, BGRA, audio
pop/dance/
  bin/cover-video.mjs               trancenwaltz video engine (canonical effects)
  bin/build.mjs                     versioned build orchestrator
  bin/trance.mjs?  → recap/bin/trance.mjs   procedural audio + struct (+ keyclick/boot/arp)
  bin/sing.mjs / vocal.mjs          jeffrey-pvc sung vocal stem
  bin/preview.sh + hover-loop.lua   clean borderless hover-to-play player
  trance-hook.np                    sung-vocal melody score
pop/chillwave/
  bin/render.mjs                    undabeach .np → mp3
  bin/preview-score.mjs             undabeach video (verlet/disc/bar reference)
  bin/gen-illy.mjs                  single-cover illustration
  undabeach.np / .illy.txt / .txt   score / prompt / lyric
marketing/bin/gen-promo.mjs         per-section campaign illustration gen (retry-resilient)
~/Documents/Working Desktop/gens/trancenwaltz-sections/<sec>/   prompts + refs + gens (v8..v12)
~/Desktop/builds/<name>/bNNN/       versioned build outputs
```
