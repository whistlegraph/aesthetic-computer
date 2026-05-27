# booch

audio-only **fermented hip-hop**, built bottom-up from AC instruments. the lane and its first track share a name — `booch` — because the name *is* the form: a **home-brewed boom-bap base** (Queen Latifah / Pete Rock golden-era warmth, SP-1200 grain) **cultured with Timbaland future-funk on top** (sparse stutters, tabla/tumbi accents, swung 16th micro-timing). booch = kombucha = slow-fermented mother culture. the lane studies that metaphor literally.

opens on the **booch** sub-format. future sub-formats (g-funk, neo-soul-rap, Phrygian dancehall-hop) land alongside if and when they earn a finished track.

## the blend (decided @jeffrey, 2026-05-24)

classic boom-bap and Timbaland future-funk live on opposite ends of "what is a beat for." boom-bap is **warm and full** — every 16th filled with hat, the kick fat and round, jazz horn loop sitting under everything ([Pete Rock / RZA / Premier on the SP-1200](https://blog.landr.com/sp-1200/)). Timbaland is **cold and sparse** — silence as a percussion choice, syllables landing in the gaps, tabla and tumbi standing in for samples ([Get Ur Freak On bhangra source](https://en.wikipedia.org/wiki/Get_Ur_Freak_On)). the lane refuses to average them. it switches between them on purpose:

- **Latifah** is the *foundation*: ~94 BPM, swung 16ths, fat boom kick on 1 & 3, sharp bap snare on 2 & 4, swung closed hats, dusty jazz horn or Rhodes stab loop, sub bass riding root + 5th. its own canon, its own pocket.
- **Timbaland** is the *agitator*: tabla/tumbi-style melodic percussion, hard-gated rest bars, stuttered ad-libs, ducked hat fills, unexpected silence where the bed would normally fill. a *treatment* applied to the foundation, not the substrate (that would be top-down).
- **the ferment** is the *swing* — straight 16ths sucked back to ~63% triplet pull, with humanized micro-timing, so the whole bed *cultures* into its groove the way booch cultures into its tang. without the fermented swing this is just boom-bap with Timbaland fills.

"home-cooked aesthetic, lid pops slow." dusty + funky + a little weird.

## format spec — booch

- **length**: ~1:30 (radio edit)
- **tempo**: 94 BPM, 4/4, swung 16ths (~63% — between straight and triplet pull)
- **key**: modal — **D Dorian** default (D E F G A B C; bluesy pentatonic-leaning D F G A C with the Dorian B for color). the Timbaland-leaning tracks may flip to F Phrygian for a darker accent.
- **structure**: intro (4) → hook (4) → verse 1 (8) → hook (4) → verse 2 (8) → bridge (4) → hook (4) → outro (4) ≈ 40 bars, ~1:42 at 94 BPM
- **bed**: boom kick + bap snare + swung closed hat + open hat accents + sub bass + jazz horn/Rhodes stab loop + vinyl crackle texture + (optional, Timbaland mode) tabla/tumbi melodic percussion
- **vocal**: jeffrey-pvc via ElevenLabs (same `/api/say` route as big-pictures). rapped, not sung — narrow pitch range, tight to the grid, Latifah-confident on the hook, Missy-playful on verse 2. instrumental valid for sub-format experiments.
- **output**: single mp3 in `out/<slug>.mp3`. no video. ID3 album `pixsies`.

## source → track

one AC vision per track. compressed to a *hook that boasts*, then verses that fill it in — Latifah's posture: state the value clearly, then defend it.

```
papers/arxiv-<slug>/<slug>.tex   (or any platter source)
  → pop/booch/<slug>.txt          (plain lyrics)
  → pop/booch/<slug>.np           (notepat score: per-syllable pitch)
  → pop/booch/out/<slug>.mp3      (mix)
```

the `.np` carries the **vocal melody contour** as primary content. verses sit close to the modal anchor (A4 / D4); the hook is the most melodic line and lands on the tonic. matches the `pop/big-pictures/plork.np` notation (folk-songs paper §3).

## arrangement notation

section markers as comments, so `bin/render.mjs` knows when to switch feel + voice routing:

```
# intro 4 [bed-only, vinyl-crackle, no-vocal]
# hook 4 [boom-bap-full, latifah-feel]
A:vis- A:-ua- D:-lize C:my A:booch — D:what F:you A:see A:is D:what A:i G:brewed ...

# verse 1 8 [boom-bap-full, narrow-pitch-rap]
# hook 4 [boom-bap-full]
# verse 2 8 [timbaland-sparse, tumbi-accent, gated-rests]
# bridge 4 [break-down, talky, ad-lib]
# hook 4 [full + tumbi]
# outro 4 [bed fade, hook fragments]
```

bracket flags tell the renderer which voices/feel to wire per section. unflagged sections inherit their block-type default.

## new voices (to build)

three new bottom-up voices, all pure-float per-sample DSP (C-portable for an eventual ac-native port). reuse the inventory: `system/public/aesthetic.computer/lib/percussion.mjs` already provides the kick/snare/hat foundation; `pop/dance/synths/sinepower.mjs` covers the additive-sine stab. the gaps:

- `synths/vinyl.mjs` — vinyl crackle bed (pink-noise base + low-rate impulse train for crackle ticks + slow LP modulation). preset: `dusty` / `clean` / `hiss`.
- `synths/rhodes.mjs` — Rhodes-y FM electric piano (sine carrier + sine modulator, slight stretched partials for bell tine, slow tremolo LFO). presets: `mellow` / `bright` / `stab`. C-port path: extend `fedac/native/src/audio.c`.
- `synths/sub.mjs` — fat P-funk sub (single sine + light tape-style saturation; sidechain-ducked under the kick). preset: `funk` / `clean` / `wide`. distinct from `hoover` and `skrill` bass character.

deferred (Timbaland-mode-only, can ship the first track without): `synths/tumbi.mjs` (Karplus-Strong one-string drone — closely related to `zitar`, may just be a `zitar.mjs` preset).

## pipeline

`bin/render.mjs` — node buffer-mix, no Web Audio, every sample synthesized here. modes: `bed` (instrumental only), **`song`** (bed → out/booch.mp3 with section grid hooks for the vocal pass). swung-16th timing baked into the score evaluator.

`bin/sing.mjs` — the rap-tuned jeffrey-pvc vocal pipeline (say → whisper align → per-word snap to 16th grid → mix vocal-forward onto the bed → out/booch-song.mp3). lyric in `<slug>.txt`, score in `<slug>.np` (word order/count must match). mirrors `pop/big-pictures/` posture: TTS is performance-on-composition; bottom-up posture preserved at the compositional layer.

```
node pop/booch/bin/render.mjs --mode song   # → out/visualize-my-booch.mp3 (bed)
node pop/booch/bin/sing.mjs                 # → out/visualize-my-booch-song.mp3 (sung)
```

media (beds, finals, the billable vocal stem) are backed to the assets system, not git — see [../ASSETS.md](../ASSETS.md).

## tracks

- **visualize-my-booch** — the eponymous track. AC-vision-as-fermentation: home-brewed runtime, scoby-as-mother-culture, slow ferment as the aesthetic. D Dorian, 94 BPM, ~1:30. status: scaffolded 2026-05-24 — lyrics + .np score drafted; render/sing pipeline pending.

---

*maintained by @jeffrey*
